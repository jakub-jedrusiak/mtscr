#' Simple top-scoring for creativity research
#'
#' Get creativity measures using simple top-scoring, i.e., calculate
#' a single index based only on top-X best scores.
#'
#' @param df Data frame in long format.
#' @param id_column Name of the column containing participant's unique id.
#' @param score_column Name of the column containing idea-level scores.
#' @param item_column Name of the column containing separate trials for the task (e.g., AUT items).
#' Optional. Supplying this argument changes the way the scores are calculated. See Deatils.
#' @param top A number or an integer vector specifying on how many best ideas
#' the final score should be based.
#' @param by_item Boolean specifying whether the return value should aggregate scores
#' from different items.
#' @param na_if_less Whether to return `NA` if the number of ideas is less than top. Otherwise
#' will calculate the scores based on the available number of ideas (default).
#' @param append Boolean specifying whether the return value should be a new data frame with
#' person-level scores (`FALSE`, default) or the original data frame with scores appended as
#' new columns (`TRUE`).
#' @param aggregate_function The function that should be used to aggregate idea-level scores
#' into person-level scores. Should be a function, not a call
#' (e.g., `aggregate_function = mean` and not `aggregate_function = mean()`)
#' @param top_all Whether to calculate the top score based on all ideas, not only the top-X.
#'
#' @return The return value is a dataframe. By default, it contains an id column and a series
#' of score columns named `top1`, `top2` etc. for each element of the vector given in the `top`
#' argument. If `by_item = TRUE`, the return value also contains an item column with item indices.
#' A separate score for each item is calculated. If `append = TRUE`, the return value is the
#' original dataframe with the score columns appended.
#'
#' @details
#' The way the top-X scores are calculated is based mainly on the `aggregate_function` and
#' on whether the `item_column` was supplied. If the `item_column` wasn't supplied, the
#' top-X scores are the participants' best ideas across all trials. For example, if `top = 2`,
#' then the score is based on the person's 2 best ideas, even if there were 3 different items
#' and both best ideas were uses for a brick.
#'
#' Now if the `item_column` was supplied and `by_item = FALSE` (default), the score will be
#' based on X best ideas per item. For example, if `top = 2` and there were 3 different items,
#' the final score will be the mean of 6 best scores – 2 per item. Set `by_item = TRUE` to
#' get separate scores for each item.
#'
#' @examples
#' data("mtscr_creativity", package = "mtscr")
#' mtscr_creativity <- mtscr_creativity |>
#'   dplyr::slice_sample(n = 500) # for performance, ignore
#'
#' # Get top1, top2, and top3 scores for each participant
#' top_scoring(mtscr_creativity, id, SemDis_MEAN, item, top = 1:3)
#'
#' # Get top2 scores ignoring items
#' top_scoring(mtscr_creativity, id, SemDis_MEAN, top = 2)
#'
#' # Get top2-top4 scores for each item separately
#' top_scoring(mtscr_creativity, id, SemDis_MEAN, item, top = 2:4, by_item = TRUE)
#'
#' # Add the scores to the original data frame
#' top_scoring(mtscr_creativity, id, SemDis_MEAN, item, top = 2:4, append = TRUE)
#'
#' # Get scores by the sum of 3 top scores (note no parentheses after the function)
#' top_scoring(
#'   mtscr_creativity,
#'   id,
#'   SemDis_MEAN,
#'   item,
#'   top = 3,
#'   aggregate_function = sum
#' )
#'
#' # Create a custom aggregate function (here: scale by 100, round and then get the mean)
#' top_scoring(
#'   mtscr_creativity,
#'   id,
#'   SemDis_MEAN,
#'   item,
#'   top = 1:3,
#'   aggregate_function = \(x) mean(round(x * 100))
#' )
#'
#' @export

top_scoring <- function(
  df,
  id_column,
  score_column,
  item_column = NULL,
  top = 1,
  by_item = FALSE,
  na_if_less = FALSE,
  append = FALSE,
  aggregate_function = mean,
  top_all = TRUE
) {
  id_column <- rlang::ensym(id_column)
  item_column_quo <- rlang::enquo(item_column)
  if (!rlang::quo_is_null(item_column_quo)) {
    item_column <- rlang::ensym(item_column)
  } else {
    item_column <- item_column_quo
  }
  score_column <- rlang::ensym(score_column)

  if (by_item & rlang::quo_is_null(item_column_quo)) {
    cli::cli_inform(c(
      "i" = "{.var by_item} has no effect if {.var item_column} is not supplied!"
    ))
  }

  if (append | top_all) {
    original_df <- df
  }

  df <- purrr::map(c(top), \(top) {
    df |>
      dplyr::arrange(!!id_column, !!item_column, dplyr::desc(!!score_column)) |>
      dplyr::slice(seq(1, top), .by = c(!!id_column, !!item_column)) |>
      dplyr::mutate(.top_number = paste0("top", top))
  }) |>
    dplyr::bind_rows()

  if (top_all) {
    df <- dplyr::bind_rows(
      df,
      dplyr::mutate(original_df, .top_number = "top_all")
    )
  }

  if (!by_item) {
    item_column <- rlang::quo(NULL)
  }

  aggregate_scores <- function(score_column, top_number) {
    if (
      na_if_less &
        dplyr::n() < as.numeric(stringr::str_extract(top_number, "\\d+")) &
        top_number != "top_all"
    ) {
      return(NA)
    }
    aggregate_function(score_column)
  }

  df <- df |>
    dplyr::summarise(
      .score = aggregate_scores(!!score_column, unique(.data$.top_number)),
      .by = c(!!id_column, !!item_column, .data$.top_number)
    ) |>
    tidyr::pivot_wider(
      names_from = .data$.top_number,
      values_from = .data$.score
    )

  if (append) {
    if (rlang::quo_is_null(item_column_quo) | !by_item) {
      by <- c(rlang::as_name(id_column))
    } else {
      by <- c(rlang::as_name(id_column), rlang::as_name(item_column))
    }
    df <- dplyr::left_join(original_df, df, by = by)
  }

  return(df)
}
