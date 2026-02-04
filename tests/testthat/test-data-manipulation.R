test_that("embed_into_cartesian expands to full cartesian product", {
  dt <- data.table::data.table(
    year = c(2020, 2020, 2021),
    region = c("A", "B", "A"),
    value = c(10, 20, 30)
  )

  result <- embed_into_cartesian(dt, c("year", "region"))

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 4L)  # 2 years x 2 regions
  expect_true(all(c("year", "region", "value") %in% names(result)))
})

test_that("embed_into_cartesian fills missing combinations with NA by default", {
  dt <- data.table::data.table(
    year = c(2020, 2021),
    region = c("A", "A"),
    value = c(10, 30)
  )

  result <- embed_into_cartesian(dt, c("year", "region"))

  # Missing combination (2020, B) and (2021, B) should have NA values
  missing_rows <- result[is.na(value)]
  expect_equal(nrow(missing_rows), 0L)  # fill=NA means no filling
})

test_that("embed_into_cartesian with custom fill value", {
  dt <- data.table::data.table(
    year = c(2020),
    region = c("A"),
    value = c(10)
  )

  result <- embed_into_cartesian(dt, c("year", "region"), fill = 0)

  expect_equal(nrow(result), 1L)  # Only one combination exists
})

test_that("embed_into_cartesian errors on column name collision", {
  dt <- data.table::data.table(
    year = 2020,
    ORIG_CJ__ = TRUE,
    value = 10
  )

  expect_error(
    embed_into_cartesian(dt, "year"),
    "Column name collision"
  )
})

test_that("embed_into_cartesian with custom dt_frame", {
  dt <- data.table::data.table(
    year = c(2020),
    region = c("A"),
    value = c(10)
  )

  dt_frame <- data.table::data.table(
    year = c(2020, 2020, 2021, 2021),
    region = c("A", "B", "A", "B")
  )

  result <- embed_into_cartesian(dt, c("year", "region"), dt_frame = dt_frame)

  expect_equal(nrow(result), 4L)
})

test_that("most_frequent returns most common value", {
  x <- c("a", "b", "a", "c", "a", "b")
  result <- most_frequent(x)
  expect_equal(result, "a")
})
test_that("most_frequent returns NA_character_ for empty input", {
  x <- character(0)
  result <- most_frequent(x)
  expect_true(is.na(result))
  expect_type(result, "character")
})

test_that("most_frequent warns for numeric input", {
  expect_warning(most_frequent(c(1, 2, 1)), "numeric vector")
})

test_that("empty_char_to_NA converts empty strings", {
  dt <- data.table::data.table(
    id = 1:3,
    name = c("Alice", "", "Charlie"),
    city = c("", "Boston", "")
  )

  empty_char_to_NA(dt)

  expect_true(is.na(dt$name[2]))
  expect_true(is.na(dt$city[1]))
  expect_true(is.na(dt$city[3]))
  expect_equal(dt$name[1], "Alice")
  expect_equal(dt$city[2], "Boston")
})

test_that("empty_char_to_NA ignores non-character columns", {
  dt <- data.table::data.table(
    id = 1:3,
    value = c(0, NA, 10)
  )

  empty_char_to_NA(dt)

  expect_equal(dt$value[1], 0)  # 0 is not converted to NA
})
