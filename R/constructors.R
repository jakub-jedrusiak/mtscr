#' Construct mtscr objects
#'
#' These constructors are for internal use.
#'
#' @keywords internal

new_mtscr <- function(x, df, ...) {
  stopifnot(inherits(x, "glmmTMB"))
  if (!inherits(x, "mtscr")) {
    class(x) <- c("mtscr", class(x))
    x$original_df <- df
  }
  return(x)
}

new_mtscr_list <- function(..., df) {
  objs <- rlang::list2(...)
  objs <- purrr::map(objs, \(x) new_mtscr(x, df = df))
  structure(objs, class = "mtscr_list")
}
