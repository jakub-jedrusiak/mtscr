# Create a test data frame
set.seed(1234)

df <- data.frame(
  id = rep(1:2, each = 9),
  item = rep(letters[1:3], 2, each = 3),
  score = runif(18, 0, 1)
)

# Test that the function returns a tibble
test_that("mtscr_prepare returns a tibble", {
  expect_true(tibble::is_tibble(mtscr_prepare(df, id, item, score)))
})

# Test that the function adds the expected columns
test_that("mtscr_prepare adds the expected columns", {
  result <- mtscr_prepare(df, id, item, score)
  expect_named(result, c(names(df), ".z_score", ".max_ind", ".top2_ind", ".ordering", ".ordering_0", ".ordering_top2_0"), ignore.order = TRUE)
})

# Test that the function returns the expected number of rows
test_that("mtscr_prepare returns the expected number of rows", {
  result <- mtscr_prepare(df, id, item, score)
  expect_equal(nrow(result), nrow(df))
})

# Test that df must be a data frame
test_that("df must be a data frame", {
  # call function with a vector
  expect_error(mtscr_prepare(1:10, id, item, score), regexp = "must be a data frame.")
})

# Test that all columns exist in the data
# id_column
test_that("id_column must exist in the data", {
  # create a test data frame without the id column
  df_no_id <- df[, c("item", "score")]

  # call function with test data frame and no id column
  expect_error(mtscr_prepare(df_no_id, id, item, score), regexp = "does not exist.")
})

# item_column
test_that("item_column must exist in the data", {
  # create a test data frame without the item column
  df_no_item <- df[, c("id", "score")]

  # call function with test data frame and no item column
  expect_error(mtscr_prepare(df_no_item, id, item, score), regexp = "does not exist.")
})

# score_column
test_that("score_column must exist in the data", {
  # create a test data frame without the score column
  df_no_score <- df[, c("id", "item")]

  # call function with test data frame and no score column
  expect_error(mtscr_prepare(df_no_score, id, item, score), regexp = "does not exist.")
})

# Test that score_column must be numeric
test_that("score_column must be numeric", {
  # create a test data frame with a non-numeric value column
  df_string_scores <- data.frame(id = c(1, 2), item = c("apple", "banana"), value = c("red", "yellow"))

  # call function with test data frame and non-numeric value column
  expect_error(mtscr_prepare(df_string_scores, id, item, value), regexp = "must be numeric.")
})

# Test that minimal argument must be logical
expect_error(mtscr_prepare(df, id, item, score, minimal = "yes."), regexp = "must be logical.")

# Test that minimal argument works as expected
test_that("minimal argument works as expected", {
  # call function with minimal = TRUE
  res_minimal <- mtscr_prepare(df, id, item, score, minimal = TRUE)

  # call function with minimal = FALSE
  res_full <- mtscr_prepare(df, id, item, score, minimal = FALSE)

  # check that res_minimal has only the additional columns
  expect_equal(ncol(res_minimal), 8)
  expect_true(".z_score" %in% colnames(res_minimal))
  expect_true(".ordering" %in% colnames(res_minimal))
  expect_true(".max_ind" %in% colnames(res_minimal))
  expect_true(".top2_ind" %in% colnames(res_minimal))

  # check that res_full has the additional columns
  expect_equal(ncol(res_full), 9)
  expect_true(".z_score" %in% colnames(res_full))
  expect_true(".ordering" %in% colnames(res_full))
  expect_true(".max_ind" %in% colnames(res_full))
  expect_true(".top2_ind" %in% colnames(res_full))

  # check that res_minimal and res_full have the same values for the additional columns
  expect_equal(res_minimal$.z_score, res_full$.z_score)
  expect_equal(res_minimal$.max_ind, res_full$.max_ind)
  expect_equal(res_minimal$.top2_ind, res_full$.top2_ind)
  expect_equal(res_minimal$.ordering, res_full$.ordering)
})