test_that("summarize_vector handles numeric vectors", {
  x <- c(1, 2, 3, 4, 5)
  result <- dtaudit:::summarize_vector(x)

  expect_equal(result[["type"]], "numeric")
  expect_equal(as.numeric(result[["n_unique"]]), 5)
  expect_equal(as.numeric(result[["missing"]]), 0)
  expect_equal(as.numeric(result[["mean"]]), 3)
  expect_equal(as.numeric(result[["min"]]), 1)
  expect_equal(as.numeric(result[["max"]]), 5)
})

test_that("summarize_vector handles all-NA numeric vectors", {
  x <- c(NA_real_, NA_real_, NA_real_)
  result <- dtaudit:::summarize_vector(x)

  expect_equal(result[["type"]], "numeric")
  expect_equal(as.numeric(result[["n_unique"]]), 0)
  expect_equal(as.numeric(result[["missing"]]), 3)
  expect_true(is.na(result[["mean"]]))
  expect_true(is.na(result[["min"]]))
  expect_true(is.na(result[["max"]]))
  expect_true(is.na(result[["sd"]]))
})

test_that("summarize_vector handles character vectors", {
  x <- c("a", "b", "a", "c")
  result <- dtaudit:::summarize_vector(x)

  expect_equal(result[["type"]], "character")
  expect_equal(as.numeric(result[["n_unique"]]), 3)
  expect_equal(as.numeric(result[["missing"]]), 0)
  expect_equal(result[["most_frequent"]], "a")
})

test_that("summarize_vector handles all-NA character vectors", {
  x <- c(NA_character_, NA_character_)
  result <- dtaudit:::summarize_vector(x)

  expect_equal(result[["type"]], "character")
  expect_equal(as.numeric(result[["missing"]]), 2)
  expect_true(is.na(result[["min"]]))
  expect_true(is.na(result[["max"]]))
})

test_that("summarize_vector handles factor vectors", {
  x <- factor(c("a", "b", "a", "c", NA))
  result <- dtaudit:::summarize_vector(x)

  expect_equal(result[["type"]], "factor")
  expect_equal(as.numeric(result[["n_unique"]]), 3)
  expect_equal(as.numeric(result[["missing"]]), 1)
  expect_equal(result[["most_frequent"]], "a")
  expect_equal(result[["min"]], "a")
  expect_equal(result[["max"]], "c")
})

test_that("summarize_vector handles logical vectors", {
  x <- c(TRUE, FALSE, TRUE, TRUE)
  result <- dtaudit:::summarize_vector(x)

  expect_equal(result[["type"]], "logical")
  expect_equal(as.numeric(result[["n_unique"]]), 2)
  expect_equal(as.numeric(result[["mean"]]), 0.75)
})

test_that("summarize_vector handles Date vectors", {
  x <- as.Date(c("2023-01-01", "2023-06-15", "2023-12-31"))
  result <- dtaudit:::summarize_vector(x)

  expect_equal(result[["type"]], "Date")
  expect_equal(as.numeric(result[["n_unique"]]), 3)
  expect_equal(result[["min"]], "2023-01-01")
  expect_equal(result[["max"]], "2023-12-31")
})

test_that("summarize_vector handles all-NA Date vectors", {
  x <- as.Date(c(NA, NA))
  result <- dtaudit:::summarize_vector(x)

  expect_equal(result[["type"]], "Date")
  expect_true(is.na(result[["min"]]))
  expect_true(is.na(result[["max"]]))
})

test_that("summarize_vector returns consistent stat names", {
  result <- dtaudit:::summarize_vector(c(1, 2, 3))
  expect_equal(names(result), dtaudit:::.summary_stat_names)
})

test_that("get_summary_table validates input", {
  expect_error(get_summary_table(data.frame(a = 1)), "is.data.table")
  dt <- data.table::data.table(a = 1, b = 2)
  expect_error(get_summary_table(dt, cols = c("a", "z")), "not found")
})

test_that("check_months_coverage detects missing months", {
  dates <- as.Date(c("2023-01-15", "2023-03-20"))

  expect_output(
    check_months_coverage(dates, "2023-01-01", "2023-03-31"),
    "1 month periods missing"
  )
})

test_that("check_months_coverage reports all months present", {
  dates <- as.Date(c("2023-01-15", "2023-02-20", "2023-03-10"))

  expect_output(
    check_months_coverage(dates, "2023-01-01", "2023-03-31"),
    "All month periods show up"
  )
})

test_that("check_months_coverage returns missing months invisibly", {
  dates <- as.Date(c("2023-01-15", "2023-03-20"))
  result <- invisible(capture.output(
    res <- check_months_coverage(dates, "2023-01-01", "2023-03-31")
  ))
  expect_length(res, 1L)
  expect_s3_class(res, "IDate")
})

test_that("check_date_coverage works with daily granularity", {
  dates <- as.Date(c("2023-01-01", "2023-01-03"))
  expect_output(
    res <- check_date_coverage(dates, "2023-01-01", "2023-01-03", by = "day"),
    "1 day periods missing"
  )
  expect_length(res, 1L)
})

test_that("check_date_coverage validates by argument", {
  expect_error(
    check_date_coverage(Sys.Date(), "2023-01-01", "2023-12-31", by = "hourly"),
    "`by` must be one of"
  )
})

test_that("diagnose_nas reports NA counts correctly", {
  dt <- data.table::data.table(
    a = c(1, NA, 3),
    b = c(NA, NA, "x"),
    c = c(TRUE, FALSE, TRUE)
  )
  res <- diagnose_nas(dt)
  expect_s3_class(res, "diagnose_na")
  expect_output(print(res), "2 of 3 columns have missing values")
  expect_equal(nrow(res$table), 3L)
  expect_equal(res$table[variable == "b", n_na], 2L)
  expect_equal(res$table[variable == "c", n_na], 0L)
  expect_equal(res$n_with_na, 2L)
})

test_that("diagnose_nas handles table with no NAs", {
  dt <- data.table::data.table(a = 1:3, b = letters[1:3])
  res <- diagnose_nas(dt)
  expect_output(print(res), "0 of 2 columns have missing values")
  expect_true(all(res$table$n_na == 0L))
  expect_equal(res$n_with_na, 0L)
})

test_that("diagnose_nas validates input", {
  expect_error(diagnose_nas(data.frame(a = 1)), "is.data.table")
})

test_that("diagnose_nas sorts by pct_na descending", {
  dt <- data.table::data.table(
    a = c(NA, NA, NA),
    b = c(1, NA, 3),
    c = c(1, 2, 3)
  )
  res <- diagnose_nas(dt)
  expect_equal(res$table$variable[1], "a")
  expect_equal(res$table$variable[3], "c")
})

test_that("print.diagnose_na returns invisibly", {
  dt <- data.table::data.table(a = c(1, NA), b = c(NA, NA))
  res <- diagnose_nas(dt)
  expect_output(ret <- print(res))
  expect_s3_class(ret, "diagnose_na")
})

test_that("get_summary_table returns correct structure", {
  dt <- data.table::data.table(
    id = 1:5,
    value = c(10.0, 20.0, 30.0, 40.0, 50.0),
    cat = c("a", "b", "a", "c", "b")
  )
  result <- get_summary_table(dt)

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 3L)  # one row per column
  expect_true("variable" %in% names(result))
  expect_true("type" %in% names(result))
  expect_true("n_unique" %in% names(result))
})

test_that("get_summary_table respects cols argument", {
  dt <- data.table::data.table(a = 1:3, b = 4:6, c = letters[1:3])
  result <- get_summary_table(dt, cols = c("a", "c"))

  expect_equal(nrow(result), 2L)
  expect_setequal(result$variable, c("a", "c"))
})

test_that("check_date_coverage quarterly granularity", {
  dates <- as.Date(c("2023-01-15", "2023-07-20"))
  expect_output(
    res <- check_date_coverage(dates, "2023-01-01", "2023-09-30", by = "quarter"),
    "1 quarter periods missing"
  )
  expect_length(res, 1L)
})

test_that("check_date_coverage yearly granularity", {
  dates <- as.Date(c("2020-06-15", "2022-03-20"))
  expect_output(
    res <- check_date_coverage(dates, "2020-01-01", "2022-12-31", by = "year"),
    "1 year periods missing"
  )
})

test_that("check_date_coverage quiet parameter suppresses output", {
  dates <- as.Date(c("2023-01-15", "2023-03-20"))
  expect_silent(
    res <- check_date_coverage(dates, "2023-01-01", "2023-03-31", quiet = TRUE)
  )
  expect_length(res, 1L)
})
