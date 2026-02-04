test_that("summarize_data handles numeric vectors", {
  x <- c(1, 2, 3, 4, 5)
  result <- dtaudit:::summarize_data(x)

  expect_equal(result[["type"]], "numeric")
  expect_equal(as.numeric(result[["missing"]]), 0)
  expect_equal(as.numeric(result[["mean"]]), 3)
  expect_equal(as.numeric(result[["min"]]), 1)
  expect_equal(as.numeric(result[["max"]]), 5)
})

test_that("summarize_data handles all-NA numeric vectors", {
  x <- c(NA_real_, NA_real_, NA_real_)
  result <- dtaudit:::summarize_data(x)

  expect_equal(result[["type"]], "numeric")
  expect_equal(as.numeric(result[["missing"]]), 3)
  expect_true(is.na(result[["mean"]]))
  expect_true(is.na(result[["min"]]))
  expect_true(is.na(result[["max"]]))
  expect_true(is.na(result[["sd"]]))
})

test_that("summarize_data handles character vectors", {
  x <- c("a", "b", "a", "c")
  result <- dtaudit:::summarize_data(x)

  expect_equal(result[["type"]], "character")
  expect_equal(as.numeric(result[["missing"]]), 0)
  expect_equal(result[["most_frequent"]], "a")
})

test_that("summarize_data handles all-NA character vectors", {
  x <- c(NA_character_, NA_character_)
  result <- dtaudit:::summarize_data(x)

  expect_equal(result[["type"]], "character")
  expect_equal(as.numeric(result[["missing"]]), 2)
  expect_true(is.na(result[["min"]]))
  expect_true(is.na(result[["max"]]))
})

test_that("summarize_data handles logical vectors", {
  x <- c(TRUE, FALSE, TRUE, TRUE)
  result <- dtaudit:::summarize_data(x)

  expect_equal(result[["type"]], "logical")
  expect_equal(as.numeric(result[["mean"]]), 0.75)
})

test_that("summarize_data handles Date vectors", {
  x <- as.Date(c("2023-01-01", "2023-06-15", "2023-12-31"))
  result <- dtaudit:::summarize_data(x)

  expect_equal(result[["type"]], "Date")
  expect_equal(result[["min"]], "2023-01-01")
  expect_equal(result[["max"]], "2023-12-31")
})

test_that("summarize_data handles all-NA Date vectors", {
  x <- as.Date(c(NA, NA))
  result <- dtaudit:::summarize_data(x)

  expect_equal(result[["type"]], "Date")
  expect_true(is.na(result[["min"]]))
  expect_true(is.na(result[["max"]]))
})

test_that("check_months_coverage detects missing months", {
  dates <- as.Date(c("2023-01-15", "2023-03-20"))

  expect_output(
    check_months_coverage(dates, "2023-01-01", "2023-03-31"),
    "1 months missing"
  )
})

test_that("check_months_coverage reports all months present", {
  dates <- as.Date(c("2023-01-15", "2023-02-20", "2023-03-10"))

  expect_output(
    check_months_coverage(dates, "2023-01-01", "2023-03-31"),
    "All months show up"
  )
})
