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

# --- stat argument tests ---

test_that("stat computes totals for x when column only in x", {
  orders <- data.table::data.table(id = 1:4, revenue = c(100, 200, 300, 400))
  products <- data.table::data.table(id = 2:5, cost = c(10, 20, 30, 40))

  result <- validate_join(orders, products, by = "id", stat.x = "revenue")

  expect_false(is.null(result$stat))
  expect_equal(result$stat$stat_col_x, "revenue")
  expect_null(result$stat$stat_col_y)
  # Total revenue = 100+200+300+400 = 1000
  expect_equal(result$stat$x$total, 1000)
  # Matched keys: 2,3,4 -> 200+300+400 = 900
  expect_equal(result$stat$x$matched, 900)
  # Only in x: key 1 -> 100
  expect_equal(result$stat$x$only, 100)
  expect_equal(result$stat$x$rate, 90)
  expect_null(result$stat$y)
})

test_that("stat works via shorthand stat= for same column in both tables", {
  x <- data.table::data.table(id = 1:3, amount = c(10, 20, 30))
  y <- data.table::data.table(id = 2:4, amount = c(100, 200, 300))

  result <- validate_join(x, y, by = "id", stat = "amount")

  expect_true(!is.null(result$stat$x))
  expect_true(!is.null(result$stat$y))
  # x: total=60, matched (keys 2,3)=50, only (key 1)=10
  expect_equal(result$stat$x$total, 60)
  expect_equal(result$stat$x$matched, 50)
  expect_equal(result$stat$x$only, 10)
  # y: total=600, matched (keys 2,3)=300, only (key 4)=300
  expect_equal(result$stat$y$total, 600)
  expect_equal(result$stat$y$matched, 300)
  expect_equal(result$stat$y$only, 300)
})

test_that("stat.x and stat.y with different column names", {
  x <- data.table::data.table(id = 1:3, revenue = c(100, 200, 300))
  y <- data.table::data.table(id = 2:4, cost = c(10, 20, 30))

  result <- validate_join(x, y, by = "id", stat.x = "revenue", stat.y = "cost")

  expect_equal(result$stat$stat_col_x, "revenue")
  expect_equal(result$stat$stat_col_y, "cost")
  # x: matched keys 2,3 -> 200+300=500; only key 1 -> 100
  expect_equal(result$stat$x$matched, 500)
  expect_equal(result$stat$x$only, 100)
  # y: matched keys 2,3 -> 10+20=30; only key 4 -> 30
  expect_equal(result$stat$y$matched, 30)
  expect_equal(result$stat$y$only, 30)
})

test_that("stat.y only reports for y", {
  x <- data.table::data.table(id = 1:3, value = letters[1:3])
  y <- data.table::data.table(id = 2:4, revenue = c(100, 200, 300))

  result <- validate_join(x, y, by = "id", stat.y = "revenue")

  expect_null(result$stat$x)
  expect_true(!is.null(result$stat$y))
  # y: total=600, matched (keys 2,3)=300, only (key 4)=300
  expect_equal(result$stat$y$total, 600)
  expect_equal(result$stat$y$matched, 300)
  expect_equal(result$stat$y$only, 300)
})

test_that("stat handles NAs in stat column", {
  x <- data.table::data.table(id = 1:4, revenue = c(100, NA, 300, 400))
  y <- data.table::data.table(id = 2:5, cost = 10:13)

  result <- validate_join(x, y, by = "id", stat.x = "revenue")

  # Total (na.rm): 100+300+400 = 800
  expect_equal(result$stat$x$total, 800)
  expect_equal(result$stat$x$n_na, 1)
  # Matched keys: 2,3,4 -> NA+300+400 = 700
  expect_equal(result$stat$x$matched, 700)
  expect_equal(result$stat$x$only, 100)
})

test_that("stat handles no overlapping keys", {
  x <- data.table::data.table(id = 1:3, revenue = c(100, 200, 300))
  y <- data.table::data.table(id = 4:6, revenue = c(400, 500, 600))

  result <- validate_join(x, y, by = "id", stat = "revenue")

  expect_equal(result$stat$x$matched, 0)
  expect_equal(result$stat$x$only, 600)
  expect_equal(result$stat$y$matched, 0)
  expect_equal(result$stat$y$only, 1500)
})

test_that("stat handles many-to-many without double-counting", {
  x <- data.table::data.table(id = c(1L, 1L, 2L), revenue = c(10, 20, 30))
  y <- data.table::data.table(id = c(1L, 2L, 2L), revenue = c(100, 200, 300))

  result <- validate_join(x, y, by = "id", stat = "revenue")

  # x: total=60, all keys match -> matched=60, only=0
  expect_equal(result$stat$x$total, 60)
  expect_equal(result$stat$x$matched, 60)
  expect_equal(result$stat$x$only, 0)
  # y: total=600, all keys match -> matched=600, only=0
  expect_equal(result$stat$y$total, 600)
  expect_equal(result$stat$y$matched, 600)
  expect_equal(result$stat$y$only, 0)
})

test_that("stat works with different key names (by.x/by.y)", {
  x <- data.table::data.table(key_x = 1:3, revenue = c(100, 200, 300))
  y <- data.table::data.table(key_y = 2:4, revenue = c(10, 20, 30))

  result <- validate_join(x, y, by.x = "key_x", by.y = "key_y", stat = "revenue")

  # x: matched keys 2,3 -> 200+300=500; only key 1 -> 100
  expect_equal(result$stat$x$matched, 500)
  expect_equal(result$stat$x$only, 100)
  # y: matched keys 2,3 -> 10+20=30; only key 4 -> 30
  expect_equal(result$stat$y$matched, 30)
  expect_equal(result$stat$y$only, 30)
})

test_that("stat handles all-zero stat column", {
  x <- data.table::data.table(id = 1:3, revenue = c(0, 0, 0))
  y <- data.table::data.table(id = 2:4, score = 10:12)

  result <- validate_join(x, y, by = "id", stat.x = "revenue")

  expect_equal(result$stat$x$total, 0)
  expect_true(is.na(result$stat$x$rate))
})

test_that("stat=NULL returns no stat info (backward compat)", {
  x <- data.table::data.table(id = 1:3, value = 10:12)
  y <- data.table::data.table(id = 2:4, score = 20:22)

  result <- validate_join(x, y, by = "id")
  expect_null(result$stat)
})

test_that("stat errors when column not found", {
  x <- data.table::data.table(id = 1:3, value = 10:12)
  y <- data.table::data.table(id = 2:4, score = 20:22)

  expect_error(validate_join(x, y, by = "id", stat.x = "nonexistent"), "not found")
  expect_error(validate_join(x, y, by = "id", stat.y = "nonexistent"), "not found")
})

test_that("stat errors when column is not numeric", {
  x <- data.table::data.table(id = 1:3, name = c("a", "b", "c"))
  y <- data.table::data.table(id = 2:4, score = 20:22)

  expect_error(validate_join(x, y, by = "id", stat.x = "name"), "not numeric")
})

test_that("stat errors when stat is not a single string", {
  x <- data.table::data.table(id = 1:3, value = 10:12)
  y <- data.table::data.table(id = 2:4, score = 20:22)

  expect_error(
    validate_join(x, y, by = "id", stat.x = c("value", "score")),
    "single column name"
  )
})

test_that("stat and stat.x/stat.y conflict errors", {
  x <- data.table::data.table(id = 1:3, value = 10:12)
  y <- data.table::data.table(id = 2:4, value = 20:22)

  expect_error(
    validate_join(x, y, by = "id", stat = "value", stat.x = "value"),
    "not both"
  )
})

test_that("print shows stat section when stat is provided", {
  x <- data.table::data.table(id = 1:3, revenue = c(100, 200, 300))
  y <- data.table::data.table(id = 2:4, score = 10:12)

  result <- validate_join(x, y, by = "id", stat.x = "revenue")

  expect_output(print(result), "Stat: revenue")
  expect_output(print(result), "Total revenue in x")
  expect_output(print(result), "Matched revenue in x")
})

test_that("print omits stat section when no stat provided", {
  x <- data.table::data.table(id = 1:3, revenue = c(100, 200, 300))
  y <- data.table::data.table(id = 2:4, score = 10:12)

  result <- validate_join(x, y, by = "id")

  output <- capture.output(print(result))
  expect_false(any(grepl("Stat:", output)))
})

test_that("print shows both table names with different stat columns", {
  x <- data.table::data.table(id = 1:3, revenue = c(100, 200, 300))
  y <- data.table::data.table(id = 2:4, cost = c(10, 20, 30))

  result <- validate_join(x, y, by = "id", stat.x = "revenue", stat.y = "cost")

  expect_output(print(result), "revenue")
  expect_output(print(result), "cost")
  expect_output(print(result), "Total revenue in x")
  expect_output(print(result), "Total cost in y")
})
