#' Validate Primary Keys
#'
#' Tests whether a set of columns constitute primary keys of a data.table,
#' i.e., whether they uniquely identify every row in the table.
#'
#' @param dt A data.table.
#' @param keys Character vector of column names to test as primary keys.
#'
#' @returns An S3 object of class `validate_pk` containing:
#' \describe{
#'   \item{table_name}{Name of the input table from the original call}
#'   \item{keys}{Character vector of column names tested}
#'   \item{is_primary_key}{Logical: TRUE if keys uniquely identify all rows}
#'   \item{n_rows}{Total number of rows in the table}
#'   \item{n_unique_keys}{Number of distinct key combinations}
#'   \item{n_duplicate_keys}{Number of key combinations that appear more than once}
#'   \item{duplicate_keys}{A data.table of duplicated key values with their counts}
#'   \item{has_numeric_keys}{Logical: TRUE if any key column is of type double}
#' }
#'
#' @details
#' A warning is issued if any key column is numeric (double), as floating-point
#' values can cause unexpected behavior in exact matching operations.
#'
#' @seealso [validate_join()] for join relationship analysis,
#'   [validate_var_relationship()] for variable relationship analysis
#'
#' @examples
#' library(data.table)
#' dt <- data.table(
#'   id = c(1L, 2L, 3L, 4L),
#'   group = c("A", "A", "B", "B"),
#'   value = c(10, 20, 30, 40)
#' )
#'
#' # Single column that IS a primary key
#' validate_primary_keys(dt, "id")
#'
#' # Single column that is NOT a primary key
#' validate_primary_keys(dt, "group")
#'
#' # Composite key that IS a primary key
#' validate_primary_keys(dt, c("group", "id"))
#'
#' @export
validate_primary_keys <- function(dt, keys) {
  stopifnot(is.data.table(dt))

  # Capture table name from the call
  table_name <- deparse(substitute(dt))

  # Validate keys argument
  if (!is.character(keys) || length(keys) == 0L) {
    stop("`keys` must be a non-empty character vector.")
  }

  # Check that all key columns exist
  if (!all(keys %chin% names(dt))) {
    miss <- setdiff(keys, names(dt))
    stop(sprintf("Column(s) not found in table: %s", paste(miss, collapse = ", ")))
  }

  # Check for numeric (double) keys and warn
  numeric_keys <- keys[vapply(dt[, ..keys], is.double, logical(1))]
  has_numeric_keys <- length(numeric_keys) > 0L
  if (has_numeric_keys) {
    warning(sprintf(
      "Key column(s) '%s' are numeric (double). Consider using integer or character for exact matching.",
      paste(numeric_keys, collapse = "', '")
    ))
  }

  # Count occurrences of each key combination
  key_counts <- dt[, .N, keyby = keys]
  n_rows <- nrow(dt)
  n_unique_keys <- nrow(key_counts)

  # Identify duplicates (N > 1)
  duplicate_keys <- key_counts[N > 1L]
  n_duplicate_keys <- nrow(duplicate_keys)

  # Primary key test: unique keys == total rows
  is_primary_key <- n_unique_keys == n_rows

  # Build result object
  out <- list(
    table_name = table_name,
    keys = keys,
    is_primary_key = is_primary_key,
    n_rows = n_rows,
    n_unique_keys = n_unique_keys,
    n_duplicate_keys = n_duplicate_keys,
    duplicate_keys = duplicate_keys,
    has_numeric_keys = has_numeric_keys
  )
  class(out) <- c("validate_pk", "list")
  out
}

#' Print Method for validate_pk Objects
#'
#' Displays a compact summary of primary key validation results.
#'
#' @param x A `validate_pk` object.
#' @param ... Additional arguments (ignored).
#'
#' @returns Invisibly returns the input object.
#'
#' @export
print.validate_pk <- function(x, ...) {
  cat("\n============== Primary Key Validation ==============\n")
  cat("Table: ", x$table_name, "\n", sep = "")
  cat("Key column(s): ", paste(x$keys, collapse = ", "), "\n", sep = "")
  cat("-----------------------------------------------------\n")

  fmt_int <- function(z) format(z, big.mark = ",", scientific = FALSE, trim = TRUE)

  cat("  Total rows:              ", fmt_int(x$n_rows), "\n", sep = "")
  cat("  Unique key combinations: ", fmt_int(x$n_unique_keys), "\n", sep = "")
  cat("  Duplicate key combos:    ", fmt_int(x$n_duplicate_keys), "\n", sep = "")
  cat("-----------------------------------------------------\n")

  if (x$is_primary_key) {
    cat("Result: YES - Keys uniquely identify all rows.\n")
  } else {
    cat("Result: NO - Keys do NOT uniquely identify all rows.\n")
    if (x$n_duplicate_keys > 0L) {
      cat("\nDuplicate keys (showing up to 10):\n")
      print(head(x$duplicate_keys, 10L))
    }
  }

  if (x$has_numeric_keys) {
    cat("\nWarning: Numeric (double) key column(s) detected.\n")
  }

  invisible(x)
}


#' Validate Variable Relationship
#'
#' Determines the relationship between two variables in a data.table:
#' one-to-one, one-to-many, many-to-one, or many-to-many.
#'
#' @param dt A data.table.
#' @param var1 Character string: name of the first variable.
#' @param var2 Character string: name of the second variable.
#'
#' @returns An S3 object of class `validate_var_rel` containing:
#' \describe{
#'   \item{table_name}{Name of the input table from the original call}
#'   \item{var1, var2}{Names of the variables analyzed}
#'   \item{relation}{Character string: "one-to-one", "one-to-many",
#'     "many-to-one", or "many-to-many"}
#'   \item{var1_unique}{Number of distinct values in var1}
#'   \item{var2_unique}{Number of distinct values in var2}
#'   \item{n_combinations}{Number of unique (var1, var2) pairs}
#'   \item{var1_has_dups}{Logical: does any var1 value map to multiple var2 values?}
#'   \item{var2_has_dups}{Logical: does any var2 value map to multiple var1 values?}
#' }
#'
#' @details
#' This function only accepts variables of type character, integer, or factor.
#' Numeric (double) variables are not allowed due to potential floating-point
#' comparison issues.
#'
#' The relationship is determined as follows:
#' \itemize{
#'   \item \strong{one-to-one}: Each value of var1 maps to exactly one value of var2,
#'     and vice versa.
#'   \item \strong{one-to-many}: Each value of var1 maps to exactly one value of var2,
#'     but some var2 values map to multiple var1 values.
#'   \item \strong{many-to-one}: Some var1 values map to multiple var2 values,
#'     but each var2 value maps to exactly one var1 value.
#'   \item \strong{many-to-many}: Both variables have values that map to multiple
#'     values of the other.
#' }
#'
#' @seealso [validate_primary_keys()] for key uniqueness validation,
#'   [validate_join()] for join relationship analysis
#'
#' @examples
#' library(data.table)
#' dt <- data.table(
#'   person_id = c(1L, 2L, 3L, 4L),
#'   department = c("Sales", "Sales", "Engineering", "Engineering"),
#'   country = c("US", "US", "US", "UK")
#' )
#'
#' # Many-to-one: multiple persons per department
#' validate_var_relationship(dt, "person_id", "department")
#'
#' # Many-to-many: departments and countries have complex mapping
#' validate_var_relationship(dt, "department", "country")
#'
#' @export
validate_var_relationship <- function(dt, var1, var2) {
  stopifnot(is.data.table(dt))

  # Capture table name from the call
  table_name <- deparse(substitute(dt))

  # Validate variable arguments
  if (!is.character(var1) || length(var1) != 1L) {
    stop("`var1` must be a single character string.")
  }
  if (!is.character(var2) || length(var2) != 1L) {
    stop("`var2` must be a single character string.")
  }

  # Check that variables exist
  if (!var1 %chin% names(dt)) {
    stop(sprintf("Variable '%s' not found in table.", var1))
  }
  if (!var2 %chin% names(dt)) {
    stop(sprintf("Variable '%s' not found in table.", var2))
  }

  # Check allowed types
  allowed_types <- c("character", "integer", "factor")

  var1_class <- class(dt[[var1]])[1L]
  var2_class <- class(dt[[var2]])[1L]

  if (!var1_class %in% allowed_types) {
    stop(sprintf(
      "Variable '%s' must be character, integer, or factor. Got: %s",
      var1, var1_class
    ))
  }
  if (!var2_class %in% allowed_types) {
    stop(sprintf(
      "Variable '%s' must be character, integer, or factor. Got: %s",
      var2, var2_class
    ))
  }

  # Get unique combinations
  vars <- c(var1, var2)
  combos <- unique(dt[, ..vars])
  n_combinations <- nrow(combos)

  # Count unique values
  var1_unique <- uniqueN(dt[[var1]])
  var2_unique <- uniqueN(dt[[var2]])

  # Check if var1 has duplicates (same var1 -> different var2)
  # This means: for some value of var1, there are multiple distinct var2 values
  var1_mapping <- combos[, .N, keyby = var1]
  var1_has_dups <- any(var1_mapping[["N"]] > 1L)

  # Check if var2 has duplicates (same var2 -> different var1)
  var2_mapping <- combos[, .N, keyby = var2]
  var2_has_dups <- any(var2_mapping[["N"]] > 1L)

  # Determine relationship type
  relation <- if (!var1_has_dups && !var2_has_dups) {
    "one-to-one"
  } else if (!var1_has_dups && var2_has_dups) {
    "many-to-one"
  } else if (var1_has_dups && !var2_has_dups) {
    "one-to-many"
  } else {
    "many-to-many"
  }

  # Build result object
  out <- list(
    table_name = table_name,
    var1 = var1,
    var2 = var2,
    relation = relation,
    var1_unique = var1_unique,
    var2_unique = var2_unique,
    n_combinations = n_combinations,
    var1_has_dups = var1_has_dups,
    var2_has_dups = var2_has_dups
  )
  class(out) <- c("validate_var_rel", "list")
  out
}

#' Print Method for validate_var_rel Objects
#'
#' Displays a compact summary of variable relationship validation results.
#'
#' @param x A `validate_var_rel` object.
#' @param ... Additional arguments (ignored).
#'
#' @returns Invisibly returns the input object.
#'
#' @export
print.validate_var_rel <- function(x, ...) {
  cat("\n============== Variable Relationship Validation ==============\n")
  cat("Table: ", x$table_name, "\n", sep = "")
  cat("Variables: ", x$var1, " <--> ", x$var2, "\n", sep = "")
  cat("--------------------------------------------------------------\n")

  fmt_int <- function(z) format(z, big.mark = ",", scientific = FALSE, trim = TRUE)

  cat("  Unique values in ", x$var1, ": ", fmt_int(x$var1_unique), "\n", sep = "")
  cat("  Unique values in ", x$var2, ": ", fmt_int(x$var2_unique), "\n", sep = "")
  cat("  Unique (", x$var1, ", ", x$var2, ") pairs: ", fmt_int(x$n_combinations), "\n", sep = "")
  cat("--------------------------------------------------------------\n")

  cat("  ", x$var1, " -> ", x$var2, ": ",
      if (x$var1_has_dups) "one-to-many" else "one-to-one", "\n", sep = "")
  cat("  ", x$var2, " -> ", x$var1, ": ",
      if (x$var2_has_dups) "one-to-many" else "one-to-one", "\n", sep = "")
  cat("--------------------------------------------------------------\n")

  cat("Relationship: ", toupper(x$relation), "\n", sep = "")

  invisible(x)
}
