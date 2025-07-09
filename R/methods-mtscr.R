#' @title Fit measures for mtscr model
#' @method summary mtscr
#' @description
#' Summarise the overall fit of a single model fitted with [mtscr()].
#'
#' @param object mtscr model or a mtscr_list object.
#' @param ... Additional arguments. Currently not used.
#'
#' @return A tibble with the following columns:
#'     \describe{
#'         \item{model}{The model number (only if a list of models is provided)}
#'         \item{nobs}{Number of observations}
#'         \item{sigma}{The square root of the estimated residual variance}
#'         \item{logLik}{The log-likelihood of the model}
#'         \item{AIC}{The Akaike information criterion}
#'         \item{BIC}{The Bayesian information criterion}
#'         \item{df.residual}{The residual degrees of freedom}
#'         \item{emp_rel}{The empirical reliability}
#'         \item{FDI}{The first difference of the empirical reliability}
#'     }
#'
#' @examples
#' data("mtscr_creativity", package = "mtscr")
#'
#' mtscr_creativity <- mtscr_creativity |>
#'   dplyr::slice_sample(n = 500) # for performance, ignore
#'
#' fit1 <- mtscr(mtscr_creativity, id, SemDis_MEAN, item, ties_method = "average")
#' fit3 <- mtscr(mtscr_creativity, id, SemDis_MEAN, item, top = 1:3, ties_method = "average")
#'
#' summary(fit1)
#'
#' summary(fit3)
#' @export
summary.mtscr <- function(object, ...) {
  summary(new_mtscr_list(object))
}

#' @title Fit measures for mtscr model list
#' @method summary mtscr_list
#' @describeIn summary.mtscr Get fit measures for a list of models fitted with [mtscr()].
#'
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

#' @title Extract scores from mtscr model
#' @description
#' Extract the scores from a model fitted with [mtscr()].
#'
#' @param object A model or a model list fitted with [mtscr()].
#' @param ... Additional arguments. Currently not used.
#' @param minimal If `TRUE`, returns only the person-level scores without the original data.
#' @param id_col If `TRUE`, returns the id column in the result. If `FALSE`, the id column is not returned.
#' Only has an impact when `minimal = TRUE`.
#' @method predict mtscr
#'
#' @return The return value is always a tibble but its content depends mainly on the `minimal` argument:
#' - If `minimal = FALSE` (default), the original data frame is returned with the creativity scores columns added.
#' - If `minimal = TRUE`, only the creativity scores are returned (i.e., one row per person).
#'
#' @examples
#' data("mtscr_creativity", package = "mtscr")
#'
#' mtscr_creativity <- mtscr_creativity |>
#'   dplyr::slice_sample(n = 500) # for performance, ignore
#'
#' fit <- mtscr(mtscr_creativity, id, SemDis_MEAN, item, top = 1:3)
#'
#' # for a single model from a list
#' predict(fit$top1)
#'
#' # for a whole list of models
#' predict(fit)
#'
#' # person-level scores only
#' predict(fit, minimal = TRUE)
#'
#' # you can also achieve more classic predict() behaviour
#' mtscr_creativity$score <- predict(fit, id_col = FALSE)
#'
#' mtscr_creativity |>
#'   tidyr::unnest_wider(score, names_sep = "_") # Use to expand list-col
#'
#' @export
predict.mtscr <- function(object, ..., minimal = FALSE, id_col = TRUE) {
  result <- glmmTMB::ranef(object)$cond
  id_col_name <- object$modelInfo$grpVar
  top <- stringr::str_remove(names(result[[1]])[[2]], "\\.ordering_")
  scores <- result[[1]] |>
    tibble::as_tibble(rownames = ".id") |>
    dplyr::select(".id", !!top := .data$`(Intercept)`)

  if (is.numeric(object$original_df[[id_col_name]])) {
    scores <- scores |>
      dplyr::mutate(".id" = as.numeric(.data$.id))
  }

  if (minimal) {
    if (!id_col) {
      scores <- dplyr::select(scores, -".id")
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
#' @describeIn predict.mtscr Extract scores from a model list fitted with [mtscr()].
#' @export
predict.mtscr_list <- function(object, ..., minimal = FALSE, id_col = TRUE) {
  id_col_name <- object[[1]]$modelInfo$grpVar
  scores <- purrr::map(object, \(x) {
    predict(x, ..., minimal = TRUE, id_col = TRUE)
  }) |>
    Reduce(\(x, y) dplyr::full_join(x, y, by = dplyr::join_by(".id")), x = _)

  if (minimal) {
    if (!id_col) {
      scores <- dplyr::select(scores, -".id")
    } else {
      scores <- scores |>
        dplyr::rename(!!id_col_name := ".id")
    }
    return(scores)
  }

  dplyr::left_join(
    object[[1]]$original_df,
    scores,
    by = dplyr::join_by(!!id_col_name == ".id")
  )
}
