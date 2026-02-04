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

test_that("compare_datatables returns type_mismatches in result list", {
  dt1 <- data.table::data.table(id = 1:3, value = 10:12)
  dt2 <- data.table::data.table(id = 1:3, value = 10:12)

  result <- compare_datatables(dt1, dt2)

  expect_true("type_mismatches" %in% names(result))
})
