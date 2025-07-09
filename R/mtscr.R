#' Create MTS model
#'
#' Create MTS model for creativity analysis. Use with [summary.mtscr()] and [predict.mtscr()].
#'
#' @param df Data frame in long format.
#' @param id_column Name of the column containing participants' id.
#' @param score_column Name of the column containing divergent thinking scores
#'     (e.g. semantic distance).
#' @param item_column Optional, name of the column containing distinct trials
#'     (e.g. names of items in AUT).
#' @param top Integer or vector of integers (see examples), number of top answers
#'     to prepare indicators for. Default is 1, i.e. only the top answer.
#' @param ties_method Character string specifying how ties are treated when
#'     ordering. Can be `"average"` (better for continuous scores like semantic
#'     distance) or `"random"` (default, better for ratings). See [rank()] for details.
#' @param normalise Logical, should the creativity score be normalised? Default is `TRUE` and
#'    it's recommended to leave it as such.
#' @param self_ranking Name of the column containing answers' self-ranking.
#'     Provide if model should be based on top answers self-chosen by the participant.
#'     Every item should have its own ranks. The top answers should have a value of 1,
#'     and the other answers should have a value of 0. In that case, the `top` argument
#'     doesn't change anything and should be left as `top = 1`. `ties_method` is not used if `self_ranking`
#'     was provided. See [mtscr_self_rank] for example.
#'
#' @return The return value depends on length of the `top` argument. If `top` is a single
#'     integer, a `mtscr` model is returned. If `top` is a vector of integers, a `mtscr_list` object
#'     is returned, with names corresponding to the `top` values, e.g. `top1`, `top2`, etc.
#'
#' @export
#'
#' @examples
#' data("mtscr_creativity", package = "mtscr")
#'
#' mtscr_creativity <- mtscr_creativity |>
#'   dplyr::slice_sample(n = 500) # for performance, ignore
#'
#' # single model for top 1 answer
#' mtscr(mtscr_creativity, id, SemDis_MEAN, item) |>
#'   summary()
#'
#' # three models for top 1, 2, and 3 answers
#' fit3 <- mtscr(
#'   mtscr_creativity,
#'   id,
#'   SemDis_MEAN,
#'   item,
#'   top = 1:3,
#'   ties_method = "average"
#' )
#'
#' # add the scores to the database
#' predict(fit3)
#'
#' # get the socres only
#' predict(fit3, minimal = TRUE)
#'
#' @seealso
#' - [summary.mtscr()] for the fit measures of the model.
#' - [predict.mtscr()] for getting the scores.

mtscr <- function(
  df,
  id_column,
  score_column,
  item_column = NULL,
  top = 1,
  ties_method = c("random", "average"),
  normalise = TRUE,
  self_ranking = NULL
) {
  id_column <- rlang::ensym(id_column)
  item_column_quo <- rlang::enquo(item_column)
  if (!rlang::quo_is_null(item_column_quo)) {
    item_column <- rlang::ensym(item_column)
  } else {
    item_column <- item_column_quo
  }
  score_column <- rlang::ensym(score_column)
  ties_method <- rlang::arg_match(ties_method)
  self_ranking_quo <- rlang::enquo(self_ranking)
  if (!rlang::quo_is_null(self_ranking_quo)) {
    self_ranking <- rlang::ensym(self_ranking)
  } else {
    self_ranking <- self_ranking_quo
  }

  # prepare
  original_df <- df
  df <- mtscr_wrangle(
    df,
    !!id_column,
    !!item_column,
    !!score_column,
    top = top,
    minimal = TRUE,
    ties_method = ties_method,
    normalise = normalise,
    self_ranking = !!self_ranking
  )

  # implicit conversion to factors
  df <- df |>
    dplyr::mutate(
      !!id_column := factor(!!id_column)
    )

  if (!rlang::quo_is_null(item_column_quo)) {
    df <- df |>
      dplyr::mutate(
        !!item_column := factor(!!item_column)
      )
  }

  if (!rlang::quo_is_null(item_column_quo)) {
    n_items <- length(unique(df[[rlang::as_label(item_column)]])) # number of unique items
  } else {
    n_items <- 1
  }

  # count ordering columns
  ordering_columns <- df |>
    dplyr::select(dplyr::starts_with(".ordering_top")) |>
    names()

  # create formulas
  # formula example: .z_score ~ -1 + item + item:.ordering_topX + (.ordering_topX | id)
  # formula if normalise = FALSE: SemDis_MEAN ~ -1 + item + item:.ordering_topX + (.ordering_topX | id)
  formulas <- purrr::map_vec(
    ordering_columns,
    \(x) {
      if (normalise) {
        formula <- ".z_score"
      } else {
        formula <- rlang::as_label(score_column)
      }
      formula <- paste0(formula, " ~ -1 + ")
      if (n_items != 1) {
        # item effect only when more than 1 item
        formula <- paste0(
          formula,
          rlang::as_name(item_column),
          " + ",
          rlang::as_name(item_column),
          ":"
        )
      }
      formula <- paste0(
        formula,
        x,
        " + (",
        x,
        " | ",
        rlang::as_name(id_column),
        ")"
      )

      formula |>
        stats::as.formula() |>
        c() # convert to vector
    }
  )

  # models
  models <- purrr::map(formulas, function(formula) {
    glmmTMB::glmmTMB(
      formula,
      data = df,
      family = stats::gaussian()
    )
  })

  if (length(ordering_columns) == 1) {
    return(new_mtscr(models[[1]], df = original_df))
  } else {
    names(models) <- paste0(
      "top",
      stringr::str_remove(ordering_columns, "\\.ordering_top")
    )
    return(new_mtscr_list(!!!models, df = original_df))
  }
}
