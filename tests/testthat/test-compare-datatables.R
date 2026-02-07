test_that("compare_datatables returns S3 object of class compare_dt", {
  dt1 <- data.table::data.table(id = 1:3, value = c(10, 20, 30))
  dt2 <- data.table::data.table(id = 1:3, value = c(10, 20, 30))

  result <- compare_datatables(dt1, dt2)
  expect_s3_class(result, "compare_dt")
})

test_that("compare_datatables reports correct row counts", {
  dt1 <- data.table::data.table(id = 1:5, value = 1:5)
  dt2 <- data.table::data.table(id = 1:3, value = 1:3)

  result <- compare_datatables(dt1, dt2)

  expect_equal(result$nrow_dt1, 5L)
  expect_equal(result$nrow_dt2, 3L)
})

test_that("compare_datatables detects column name differences", {
  dt1 <- data.table::data.table(id = 1:3, value = 1:3, extra1 = "a")
  dt2 <- data.table::data.table(id = 1:3, value = 1:3, extra2 = "b")

  result <- compare_datatables(dt1, dt2)

  expect_equal(sort(result$common_columns), c("id", "value"))
  expect_equal(result$only_dt1, "extra1")
  expect_equal(result$only_dt2, "extra2")
})

test_that("compare_datatables detects no type mismatches when types match", {
  dt1 <- data.table::data.table(id = 1:3, value = c("a", "b", "c"))
  dt2 <- data.table::data.table(id = 4:6, value = c("x", "y", "z"))

  result <- compare_datatables(dt1, dt2)
  expect_null(result$type_mismatches)
})

test_that("compare_datatables detects type mismatches", {
  dt1 <- data.table::data.table(id = 1:3, value = c("a", "b", "c"))
  dt2 <- data.table::data.table(id = as.character(1:3), value = c("x", "y", "z"))

  result <- compare_datatables(dt1, dt2)

  expect_s3_class(result$type_mismatches, "data.table")
  expect_equal(nrow(result$type_mismatches), 1L)
  expect_equal(result$type_mismatches$column, "id")
  expect_equal(result$type_mismatches$type_dt1, "integer")
  expect_equal(result$type_mismatches$type_dt2, "character")
})

test_that("compare_datatables detects multiple type mismatches", {
  dt1 <- data.table::data.table(
    id = 1:3,
    value = c(1.0, 2.0, 3.0),
    flag = c(TRUE, FALSE, TRUE)
  )
  dt2 <- data.table::data.table(
    id = as.character(1:3),
    value = as.integer(c(1, 2, 3)),
    flag = c("yes", "no", "yes")
  )

  result <- compare_datatables(dt1, dt2)

  expect_s3_class(result$type_mismatches, "data.table")
  expect_equal(nrow(result$type_mismatches), 3L)
  expect_setequal(result$type_mismatches$column, c("id", "value", "flag"))
})

test_that("compare_datatables computes key summary with auto-detected keys", {
  dt1 <- data.table::data.table(id = 1:3, value = c(10.0, 20.0, 30.0))
  dt2 <- data.table::data.table(id = c(2L, 3L, 4L), value = c(20.0, 30.0, 40.0))

  result <- compare_datatables(dt1, dt2)

  expect_false(is.null(result$key_summary))
  expect_true(result$key_summary$auto)
  expect_equal(result$key_summary$matches, 2L)  # ids 2 and 3
  expect_equal(result$key_summary$only_dt1, 1L)  # id 1
  expect_equal(result$key_summary$only_dt2, 1L)  # id 4
})

test_that("compare_datatables computes numeric discrepancies via keys", {
  dt1 <- data.table::data.table(id = 1:3, value = c(10.0, 20.0, 30.0))
  dt2 <- data.table::data.table(id = 1:3, value = c(10.1, 20.0, 30.5))

  result <- compare_datatables(dt1, dt2)

  expect_equal(result$numeric_method, "keys")
  expect_false(is.null(result$numeric_summary))
  expect_equal(nrow(result$numeric_summary), 1L)
  expect_equal(result$numeric_summary$column, "value")
  expect_equal(result$numeric_summary$n, 3L)
  expect_equal(result$numeric_summary$min, 0.0)
  expect_equal(result$numeric_summary$max, 0.5)
  expect_equal(result$rows_matched, 3L)
})

test_that("compare_datatables falls back to row-index when no keys", {
  dt1 <- data.table::data.table(value = c(10.0, 20.0, 30.0))
  dt2 <- data.table::data.table(value = c(10.0, 25.0, 30.0))

  result <- compare_datatables(dt1, dt2)

  expect_equal(result$numeric_method, "row_index")
  expect_false(is.null(result$numeric_summary))
  expect_equal(result$numeric_summary$max, 5.0)
})

test_that("compare_datatables with custom key_cols", {
  dt1 <- data.table::data.table(id = 1:3, cat = c("a", "b", "c"), value = c(10, 20, 30))
  dt2 <- data.table::data.table(id = 1:3, cat = c("a", "b", "c"), value = c(11, 22, 33))

  result <- compare_datatables(dt1, dt2, key_cols = "id")

  expect_false(is.null(result$key_summary))
  expect_equal(result$key_summary$keys, "id")
  expect_false(result$key_summary$auto)
})

test_that("compare_datatables with identical tables shows zero discrepancies", {
  dt1 <- data.table::data.table(id = 1:3, value = c(10.0, 20.0, 30.0))
  dt2 <- data.table::copy(dt1)

  result <- compare_datatables(dt1, dt2)

  expect_false(is.null(result$numeric_summary))
  expect_true(all(result$numeric_summary$max == 0))
})

test_that("compare_datatables with no common numeric columns", {
  dt1 <- data.table::data.table(id = 1:3, cat = c("a", "b", "c"))
  dt2 <- data.table::data.table(id = 1:3, cat = c("x", "y", "z"))

  result <- compare_datatables(dt1, dt2)

  expect_true(is.na(result$numeric_method))
  expect_null(result$numeric_summary)
})

test_that("compare_datatables errors on no common columns", {
  dt1 <- data.table::data.table(a = 1:3)
  dt2 <- data.table::data.table(b = 1:3)

  expect_error(compare_datatables(dt1, dt2), "No matching column names")
})

test_that("compare_datatables errors on invalid key_cols", {
  dt1 <- data.table::data.table(id = 1:3, value = 1:3)
  dt2 <- data.table::data.table(id = 1:3, value = 1:3)

  expect_error(compare_datatables(dt1, dt2, key_cols = "nonexistent"),
               "key_cols not present")
})

test_that("print.compare_dt returns invisibly", {
  dt1 <- data.table::data.table(id = 1:3, value = c(10, 20, 30))
  dt2 <- data.table::data.table(id = 1:3, value = c(10, 20, 30))
  result <- compare_datatables(dt1, dt2)

  expect_output(ret <- print(result))
  expect_s3_class(ret, "compare_dt")
})

test_that("print.compare_dt outputs key sections", {
  dt1 <- data.table::data.table(id = 1:3, value = c(10.0, 20.0, 30.0))
  dt2 <- data.table::data.table(id = 1:3, value = c(10.1, 20.0, 30.5))
  result <- compare_datatables(dt1, dt2)

  output <- capture.output(print(result))
  expect_true(any(grepl("Number of rows", output)))
  expect_true(any(grepl("Column names", output)))
  expect_true(any(grepl("Key columns", output)))
  expect_true(any(grepl("Numeric column discrepancies", output)))
})
