# Simple test suite for mtscr package
# Tests basic functionality with embedded datasets
# Note: Some convergence warnings from glmmTMB are expected and do not indicate test failure

# Load test data
data("mtscr_creativity", package = "mtscr")
data("mtscr_self_rank", package = "mtscr")

# Use larger samples for reliable model convergence
set.seed(42)
test_creativity <- mtscr_creativity |>
  dplyr::filter(item %in% c("clock", "pencil")) |> # Just 2 items
  dplyr::slice_sample(n = 500) # Larger sample for convergence

test_self_rank <- mtscr_self_rank |>
  dplyr::slice_sample(n = 400) # Larger sample for convergence

# Helper function to suppress only convergence warnings
suppress_convergence_warnings <- function(expr) {
  withCallingHandlers(expr,
    warning = function(w) {
      if (grepl("Model convergence problem|singular convergence|non-positive-definite Hessian", w$message)) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

# Basic functionality tests ----
test_that("mtscr() works with basic parameters", {
  fit <- suppress_convergence_warnings(mtscr(test_creativity, id, SemDis_MEAN, item, top = 1))

  expect_s3_class(fit, "mtscr")
  expect_s3_class(fit, "glmmTMB")
})

test_that("mtscr() works without item column", {
  simple_data <- test_creativity |>
    dplyr::select(-item)

  fit <- suppress_convergence_warnings(mtscr(simple_data, id, SemDis_MEAN, top = 1))

  expect_s3_class(fit, "mtscr")
})

test_that("mtscr() works with self_ranking", {
  fit <- suppress_convergence_warnings(mtscr(test_self_rank, subject, avr, task,
    top = 1, self_ranking = top_two
  ))

  expect_s3_class(fit, "mtscr")
})

# Summary methods ----
test_that("summary.mtscr() returns data frame", {
  fit <- suppress_convergence_warnings(mtscr(test_creativity, id, SemDis_MEAN, item, top = 1))
  summ <- summary(fit)

  expect_s3_class(summ, "data.frame")
  expect_true("emp_rel" %in% names(summ))
})

# Predict methods ----
test_that("predict.mtscr() returns data frame", {
  fit <- suppress_convergence_warnings(mtscr(test_creativity, id, SemDis_MEAN, item, top = 1))
  pred <- predict(fit, minimal = TRUE)

  expect_s3_class(pred, "data.frame")
  expect_true("top1" %in% names(pred))
})

# Error handling ----
test_that("mtscr() handles invalid input", {
  expect_error(mtscr(test_creativity, nonexistent_col, SemDis_MEAN, item))
  expect_error(mtscr(test_creativity, id, nonexistent_col, item))
})

# App function ----
test_that("mtscr_app() exists", {
  expect_true(exists("mtscr_app"))
  expect_true(is.function(mtscr_app))
})
