# Tests for validate_primary_keys

test_that("validate_primary_keys detects valid single-column primary key", {
  dt <- data.table::data.table(id = 1:5, value = letters[1:5])

  result <- validate_primary_keys(dt, "id")

  expect_s3_class(result, "validate_pk")
  expect_true(result$is_primary_key)
  expect_equal(result$n_rows, 5)
  expect_equal(result$n_unique_keys, 5)
  expect_equal(result$n_duplicate_keys, 0)
})

test_that("validate_primary_keys detects valid composite primary key", {
  dt <- data.table::data.table(
    group = c("A", "A", "B", "B"),
    id = c(1L, 2L, 1L, 2L),
    value = 10:13
  )

  result <- validate_primary_keys(dt, c("group", "id"))

  expect_true(result$is_primary_key)
  expect_equal(result$n_rows, 4)
  expect_equal(result$n_unique_keys, 4)
})

test_that("validate_primary_keys detects non-unique single column", {
  dt <- data.table::data.table(
    group = c("A", "A", "B", "B"),
    value = 1:4
  )

  result <- validate_primary_keys(dt, "group")

  expect_false(result$is_primary_key)
  expect_equal(result$n_rows, 4)
  expect_equal(result$n_unique_keys, 2)
  expect_equal(result$n_duplicate_keys, 2)
  expect_equal(nrow(result$duplicate_keys), 2)
})

test_that("validate_primary_keys warns on numeric (double) keys", {
  dt <- data.table::data.table(
    id = c(1.0, 2.0, 3.0),
    value = letters[1:3]
  )

  expect_warning(
    result <- validate_primary_keys(dt, "id"),
    "numeric \\(double\\)"
  )

  expect_true(result$has_numeric_keys)
})

test_that("validate_primary_keys does not warn on integer keys", {
  dt <- data.table::data.table(id = 1:3L, value = letters[1:3])

  expect_no_warning(result <- validate_primary_keys(dt, "id"))
  expect_false(result$has_numeric_keys)
})

test_that("validate_primary_keys errors on missing column", {
  dt <- data.table::data.table(id = 1:3, value = letters[1:3])

  expect_error(
    validate_primary_keys(dt, "nonexistent"),
    "not found"
  )
})

test_that("validate_primary_keys errors on empty keys", {
  dt <- data.table::data.table(id = 1:3, value = letters[1:3])

  expect_error(
    validate_primary_keys(dt, character(0)),
    "non-empty"
  )
})

test_that("print.validate_pk returns invisibly", {
  dt <- data.table::data.table(id = 1:3, value = letters[1:3])

  result <- validate_primary_keys(dt, "id")

  expect_output(ret <- print(result), "Primary Key Validation")
  expect_identical(ret, result)
})

test_that("print.validate_pk shows duplicates when not a primary key", {
  dt <- data.table::data.table(group = c("A", "A", "B"), value = 1:3)

  result <- validate_primary_keys(dt, "group")

  expect_output(print(result), "Duplicate keys")
})


# Tests for validate_var_relationship

test_that("validate_var_relationship detects one-to-one relationship", {
  dt <- data.table::data.table(
    id = 1:3L,
    code = c("A", "B", "C")
  )

  result <- validate_var_relationship(dt, "id", "code")

  expect_s3_class(result, "validate_var_rel")
  expect_equal(result$relation, "one-to-one")
  expect_false(result$var1_has_dups)
  expect_false(result$var2_has_dups)
})

test_that("validate_var_relationship detects one-to-many relationship", {
  dt <- data.table::data.table(
    department = c("Sales", "Sales", "Engineering"),
    employee = c("Alice", "Bob", "Charlie")
  )

  result <- validate_var_relationship(dt, "department", "employee")

  expect_equal(result$relation, "one-to-many")
  expect_true(result$var1_has_dups)
  expect_false(result$var2_has_dups)
})

test_that("validate_var_relationship detects many-to-one relationship", {
  dt <- data.table::data.table(
    employee = c("Alice", "Bob", "Charlie"),
    department = c("Sales", "Sales", "Engineering")
  )

  result <- validate_var_relationship(dt, "employee", "department")

  expect_equal(result$relation, "many-to-one")
  expect_false(result$var1_has_dups)
  expect_true(result$var2_has_dups)
})

test_that("validate_var_relationship detects many-to-many relationship", {
  dt <- data.table::data.table(
    student = c("Alice", "Alice", "Bob", "Bob"),
    course = c("Math", "Science", "Math", "Art")
  )

  result <- validate_var_relationship(dt, "student", "course")

  expect_equal(result$relation, "many-to-many")
  expect_true(result$var1_has_dups)
  expect_true(result$var2_has_dups)
})

test_that("validate_var_relationship works with factor variables", {
  dt <- data.table::data.table(
    id = 1:3L,
    group = factor(c("A", "B", "C"))
  )

  result <- validate_var_relationship(dt, "id", "group")

  expect_equal(result$relation, "one-to-one")
})

test_that("validate_var_relationship works with integer variables", {
  dt <- data.table::data.table(
    id = 1:4L,
    code = c(100L, 100L, 200L, 200L)
  )

  result <- validate_var_relationship(dt, "id", "code")

  expect_equal(result$relation, "many-to-one")
})

test_that("validate_var_relationship errors on numeric (double) variable", {
  dt <- data.table::data.table(
    id = 1:3L,
    value = c(1.5, 2.5, 3.5)
  )

  expect_error(
    validate_var_relationship(dt, "id", "value"),
    "character, integer, or factor"
  )
})

test_that("validate_var_relationship errors on missing variable", {
  dt <- data.table::data.table(id = 1:3L, value = letters[1:3])

  expect_error(
    validate_var_relationship(dt, "id", "nonexistent"),
    "not found"
  )
})

test_that("validate_var_relationship errors on non-character var1", {
  dt <- data.table::data.table(id = 1:3L, value = letters[1:3])

  expect_error(
    validate_var_relationship(dt, 123, "value"),
    "single character string"
  )
})

test_that("print.validate_var_rel returns invisibly", {
  dt <- data.table::data.table(id = 1:3L, code = c("A", "B", "C"))

  result <- validate_var_relationship(dt, "id", "code")

  expect_output(ret <- print(result), "Variable Relationship Validation")
  expect_identical(ret, result)
})

test_that("validate_var_relationship counts are correct", {
  dt <- data.table::data.table(
    person = c("Alice", "Bob", "Charlie", "Alice"),
    city = c("NYC", "LA", "NYC", "LA")
  )

  result <- validate_var_relationship(dt, "person", "city")

  expect_equal(result$var1_unique, 3)  # Alice, Bob, Charlie
  expect_equal(result$var2_unique, 2)  # NYC, LA
  expect_equal(result$n_combinations, 4)  # Alice-NYC, Bob-LA, Charlie-NYC, Alice-LA
})
