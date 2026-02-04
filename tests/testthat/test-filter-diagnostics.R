test_that("filter_keep.data.table keeps correct rows", {
  dt <- data.table::data.table(
    id = 1:5,
    keep = c(TRUE, FALSE, TRUE, TRUE, FALSE),
    value = c(10, 20, 30, 40, 50)
  )

  result <- filter_keep(dt, keep == TRUE, quiet = TRUE)

  expect_equal(nrow(result), 3)
  expect_equal(result$id, c(1, 3, 4))
})

test_that("filter_keep.data.table reports dropped rows", {
  dt <- data.table::data.table(
    id = 1:4,
    keep = c(TRUE, FALSE, TRUE, FALSE)
  )

  expect_output(
    filter_keep(dt, keep == TRUE),
    "Dropped 2 of 4 rows"
  )
})

test_that("filter_keep.data.table reports stat values", {
  dt <- data.table::data.table(
    id = 1:4,
    keep = c(TRUE, FALSE, TRUE, FALSE),
    sales = c(100, 50, 200, 150)
  )

  expect_output(
    filter_keep(dt, keep == TRUE, stat = sales),
    "Dropped 200 of 500 for sales"
  )
})

test_that("filter_keep.data.table handles NA with na_as = FALSE", {
  dt <- data.table::data.table(
    id = 1:4,
    keep = c(TRUE, FALSE, NA, TRUE)
  )

  result <- filter_keep(dt, keep == TRUE, na_as = FALSE, quiet = TRUE)

  expect_equal(nrow(result), 2)  # Only rows where keep is TRUE
  expect_equal(result$id, c(1, 4))
})

test_that("filter_keep.data.table handles NA with na_as = TRUE", {
  dt <- data.table::data.table(
    id = 1:4,
    keep = c(TRUE, FALSE, NA, TRUE)
  )

  result <- filter_keep(dt, keep == TRUE, na_as = TRUE, quiet = TRUE)

  expect_equal(nrow(result), 3)  # Rows where keep is TRUE or NA
  expect_equal(result$id, c(1, 3, 4))
})

test_that("filter_keep.data.table reports NA count", {
  dt <- data.table::data.table(
    id = 1:4,
    keep = c(TRUE, FALSE, NA, NA)
  )

  expect_output(
    filter_keep(dt, keep == TRUE, na_as = FALSE),
    "2 rows had NA"
  )
})

test_that("filter_keep.data.table returns data.table", {
  dt <- data.table::data.table(id = 1:3, keep = c(TRUE, TRUE, FALSE))

  result <- filter_keep(dt, keep == TRUE, quiet = TRUE)

  expect_s3_class(result, "data.table")
})

test_that("filter_keep.data.table errors on non-data.table", {
  df <- data.frame(id = 1:3, keep = c(TRUE, TRUE, FALSE))

  expect_error(filter_keep.data.table(df, keep == TRUE), "must be a data.table")
})

test_that("filter_keep.data.table errors on invalid expression", {
  dt <- data.table::data.table(id = 1:3, keep = c(TRUE, TRUE, FALSE))

  expect_error(
    filter_keep(dt, id),  # Returns integer, not logical
    "must evaluate to a logical vector"
  )
})

test_that("filter_keep.data.table handles empty result", {
  dt <- data.table::data.table(id = 1:3, keep = c(FALSE, FALSE, FALSE))

  result <- filter_keep(dt, keep == TRUE, quiet = TRUE)

  expect_equal(nrow(result), 0)
  expect_s3_class(result, "data.table")
})

test_that("filter_keep.data.table quiet mode suppresses output", {
  dt <- data.table::data.table(id = 1:3, keep = c(TRUE, FALSE, TRUE))

  expect_silent(filter_keep(dt, keep == TRUE, quiet = TRUE))
})

test_that("filter_keep.data.table prints data.table name and expression in header", {
  dt <- data.table::data.table(id = 1:4, keep = c(TRUE, FALSE, TRUE, FALSE))

  # Output includes diagnostics + printed data.table; we check for header line
  expect_output(
    filter_keep(dt, keep == TRUE),
    "filter_keep\\(dt, keep == TRUE\\)"
  )
})

test_that("filter_keep.data.table warn_threshold triggers warning when exceeded", {
  dt <- data.table::data.table(id = 1:4, keep = c(TRUE, FALSE, FALSE, FALSE))

  # 75% dropped exceeds 50% threshold - should warn
  expect_warning(
    filter_keep(dt, keep == TRUE, quiet = TRUE, warn_threshold = 0.5),
    "exceeds threshold"
  )
})

test_that("filter_keep.data.table warn_threshold silent when not exceeded", {
  dt <- data.table::data.table(id = 1:4, keep = c(TRUE, FALSE, TRUE, TRUE))

  # 25% dropped is below 50% threshold - no warning
  expect_silent(
    filter_keep(dt, keep == TRUE, quiet = TRUE, warn_threshold = 0.5)
  )
})

# Tests for filter_drop
test_that("filter_drop.data.table drops correct rows", {
  dt <- data.table::data.table(
    id = 1:5,
    bad = c(FALSE, TRUE, FALSE, TRUE, FALSE),
    value = c(10, 20, 30, 40, 50)
  )

  result <- filter_drop(dt, bad == TRUE, quiet = TRUE)

  expect_equal(nrow(result), 3)
  expect_equal(result$id, c(1, 3, 5))
})

test_that("filter_drop.data.table reports dropped rows", {
  dt <- data.table::data.table(
    id = 1:4,
    bad = c(FALSE, TRUE, FALSE, TRUE)
  )

  expect_output(
    filter_drop(dt, bad == TRUE),
    "Dropped 2 of 4 rows"
  )
})

test_that("filter_drop.data.table prints correct header", {
  dt <- data.table::data.table(
    id = 1:4,
    bad = c(FALSE, TRUE, FALSE, TRUE)
  )

  expect_output(
    filter_drop(dt, bad == TRUE),
    "filter_drop\\(dt, bad == TRUE\\)"
  )
})

test_that("filter_drop.data.table reports stat values", {
  dt <- data.table::data.table(
    id = 1:4,
    bad = c(FALSE, TRUE, FALSE, TRUE),
    sales = c(100, 50, 200, 150)
  )

  expect_output(
    filter_drop(dt, bad == TRUE, stat = sales),
    "Dropped 200 of 500 for sales"
  )
})

test_that("filter_drop.data.table handles NA with na_as = FALSE", {
  dt <- data.table::data.table(
    id = 1:4,
    bad = c(FALSE, TRUE, NA, FALSE)
  )

  result <- filter_drop(dt, bad == TRUE, na_as = FALSE, quiet = TRUE)

  # NA treated as FALSE → not dropped, so rows 1, 3, 4 are kept
  expect_equal(nrow(result), 3)
  expect_equal(result$id, c(1, 3, 4))
})

test_that("filter_drop.data.table handles NA with na_as = TRUE", {
  dt <- data.table::data.table(
    id = 1:4,
    bad = c(FALSE, TRUE, NA, FALSE)
  )

  result <- filter_drop(dt, bad == TRUE, na_as = TRUE, quiet = TRUE)

  # NA treated as TRUE → dropped, so only rows 1 and 4 are kept
  expect_equal(nrow(result), 2)
  expect_equal(result$id, c(1, 4))
})

test_that("filter_drop.data.table reports NA count", {
  dt <- data.table::data.table(
    id = 1:4,
    bad = c(FALSE, TRUE, NA, NA)
  )

  expect_output(
    filter_drop(dt, bad == TRUE, na_as = FALSE),
    "2 rows had NA"
  )
})

test_that("filter_drop.data.table returns data.table", {
  dt <- data.table::data.table(id = 1:3, bad = c(TRUE, TRUE, FALSE))

  result <- filter_drop(dt, bad == TRUE, quiet = TRUE)

  expect_s3_class(result, "data.table")
})

test_that("filter_drop.data.table errors on non-data.table", {
  df <- data.frame(id = 1:3, bad = c(TRUE, TRUE, FALSE))

  expect_error(filter_drop.data.table(df, bad == TRUE), "must be a data.table")
})

test_that("filter_drop.data.table errors on invalid expression", {
  dt <- data.table::data.table(id = 1:3, bad = c(TRUE, TRUE, FALSE))

  expect_error(
    filter_drop(dt, id),  # Returns integer, not logical
    "must evaluate to a logical vector"
  )
})

test_that("filter_drop.data.table handles empty result", {
  dt <- data.table::data.table(id = 1:3, bad = c(TRUE, TRUE, TRUE))

  result <- filter_drop(dt, bad == TRUE, quiet = TRUE)

  expect_equal(nrow(result), 0)
  expect_s3_class(result, "data.table")
})

test_that("filter_drop.data.table quiet mode suppresses output", {
  dt <- data.table::data.table(id = 1:3, bad = c(TRUE, FALSE, TRUE))

  expect_silent(filter_drop(dt, bad == TRUE, quiet = TRUE))
})

test_that("filter_drop.data.table warn_threshold triggers warning when exceeded", {
  dt <- data.table::data.table(id = 1:4, bad = c(TRUE, TRUE, TRUE, FALSE))

  # 75% dropped exceeds 50% threshold - should warn
  expect_warning(
    filter_drop(dt, bad == TRUE, quiet = TRUE, warn_threshold = 0.5),
    "exceeds threshold"
  )
})

test_that("filter_drop.data.table warn_threshold silent when not exceeded", {
  dt <- data.table::data.table(id = 1:4, bad = c(TRUE, FALSE, FALSE, FALSE))

  # 25% dropped is below 50% threshold - no warning
  expect_silent(
    filter_drop(dt, bad == TRUE, quiet = TRUE, warn_threshold = 0.5)
  )
})
