#' Generic methods for mtscr models
#'
#' These methods are for internal use.
#'
#' @method summary mtscr
#' @export
summary.mtscr <- function(object, ...) {
  summary(new_mtscr_list(object))
}

#' @method summary mtscr_list
#' @export
summary.mtscr_list <- function(object, ...) {
  list_obj <- purrr::map(
    object,
    \(x) {
      st <- glmmTMB::VarCorr(x)[1]$cond[[1]][1]
      diag_cov <- x$sdr$diag.cov.random
      se <- diag_cov[seq(1, length(diag_cov) / 2)] |>
        mean()

      broom.mixed::glance(x) |>
        dplyr::mutate(
          emp_rel = 1 - se / st,
          FDI = sqrt(.data$emp_rel)
        )
    }
  )
  if (length(list_obj) > 1) {
    return(dplyr::bind_rows(list_obj, .id = "model"))
  }
  return(list_obj[[1]])
}

#' @method predict mtscr
#' @export
predict.mtscr <- function(object, ..., minimal = FALSE, id_col = TRUE) {
  result <- glmmTMB::ranef(object)$cond
  id_col_name <- object$modelInfo$grpVar
  top <- stringr::str_remove(names(result[[1]])[[2]], "\\.ordering_")
  scores <- result[[1]] |>
    tibble::as_tibble(rownames = ".id") |>
    dplyr::select(.id, !!top := `(Intercept)`)

  if (is.numeric(object$original_df[[id_col_name]])) {
    scores <- scores |>
      dplyr::mutate(.id = as.numeric(.id))
  }

  if (minimal) {
    if (!id_col) {
      scores <- dplyr::select(scores, -.id)
    }
    return(scores)
  }

  dplyr::left_join(
    object$original_df,
    scores,
    by = dplyr::join_by(!!id_col_name == ".id")
  )
}

#' @method predict mtscr_list
#' @export
predict.mtscr_list <- function(object, ..., minimal = FALSE, id_col = TRUE) {
  id_col_name <- object[[1]]$modelInfo$grpVar
  scores <- purrr::map(object, \(x) {
    predict(x, ..., minimal = TRUE, id_col = TRUE)
  }) |>
    Reduce(\(x, y) dplyr::full_join(x, y, by = dplyr::join_by(".id")), x = _)

  if (minimal) {
    if (!id_col) {
      scores <- dplyr::select(scores, -.id)
    } else {
      scores <- scores |>
        dplyr::rename(!!id_col_name := ".id")
    }
    return(scores)
  }

  dplyr::left_join(
    object[[1]]$original_df,
    scores,
    by = dplyr::join_by(!!id_col_name == .id)
  )
}
