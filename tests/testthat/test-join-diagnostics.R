test_that("validate_join detects one-to-one relationships", {
  dt1 <- data.table::data.table(id = 1:3, value = c("a", "b", "c"))
  dt2 <- data.table::data.table(id = 2:4, score = c(10, 20, 30))

  result <- validate_join(dt1, dt2, by = "id")

  expect_s3_class(result, "validate_join")
  expect_equal(result$relation, "one-to-one")
  expect_equal(result$counts$x_rows, 3)
  expect_equal(result$counts$y_rows, 3)
  expect_equal(result$counts$n_key_overlap, 2)  # ids 2 and 3
})

test_that("validate_join detects many-to-one relationships", {
  dt1 <- data.table::data.table(id = c(1, 1, 2, 3), value = letters[1:4])
  dt2 <- data.table::data.table(id = 1:3, score = c(10, 20, 30))

  result <- validate_join(dt1, dt2, by = "id")

  expect_equal(result$relation, "many-to-one")
  expect_true(result$duplicates$x_has_dups)
  expect_false(result$duplicates$y_has_dups)
})

test_that("validate_join detects one-to-many relationships", {
  dt1 <- data.table::data.table(id = 1:3, value = letters[1:3])
  dt2 <- data.table::data.table(id = c(1, 1, 2, 3), score = 10:13)

  result <- validate_join(dt1, dt2, by = "id")

  expect_equal(result$relation, "one-to-many")
  expect_false(result$duplicates$x_has_dups)
  expect_true(result$duplicates$y_has_dups)
})

test_that("validate_join detects many-to-many relationships", {
  dt1 <- data.table::data.table(id = c(1, 1, 2), value = letters[1:3])
  dt2 <- data.table::data.table(id = c(1, 2, 2), score = 10:12)

  result <- validate_join(dt1, dt2, by = "id")

  expect_equal(result$relation, "many-to-many")
  expect_true(result$duplicates$x_has_dups)
  expect_true(result$duplicates$y_has_dups)
})

test_that("validate_join handles no matches", {
  dt1 <- data.table::data.table(id = 1:3, value = letters[1:3])
  dt2 <- data.table::data.table(id = 4:6, score = 10:12)

  result <- validate_join(dt1, dt2, by = "id")

  expect_equal(result$relation, "no matches")
  expect_equal(result$counts$n_key_overlap, 0)
})

test_that("validate_join errors on missing key columns", {
  dt1 <- data.table::data.table(id = 1:3, value = letters[1:3])
  dt2 <- data.table::data.table(other_id = 1:3, score = 10:12)

  expect_error(validate_join(dt1, dt2, by = "id"), "missing key")
})

test_that("validate_join supports different key names", {
  dt1 <- data.table::data.table(id_x = 1:3, value = letters[1:3])
  dt2 <- data.table::data.table(id_y = 2:4, score = 10:12)

  result <- validate_join(dt1, dt2, by.x = "id_x", by.y = "id_y")

  expect_equal(result$by.x, "id_x")
  expect_equal(result$by.y, "id_y")
  expect_equal(result$relation, "one-to-one")
})

test_that("print.validate_join returns invisibly", {
  dt1 <- data.table::data.table(id = 1:3, value = letters[1:3])
  dt2 <- data.table::data.table(id = 2:4, score = 10:12)

  result <- validate_join(dt1, dt2, by = "id")

  expect_output(ret <- print(result), "Join Validation Summary")
  expect_identical(ret, result)
})

test_that("summary.validate_join returns summary_table", {
  dt1 <- data.table::data.table(id = 1:3, value = letters[1:3])
  dt2 <- data.table::data.table(id = 2:4, score = 10:12)

  result <- validate_join(dt1, dt2, by = "id")

  expect_output(tbl <- summary(result), "Join Validation Summary")
  expect_s3_class(tbl, "data.table")
})
