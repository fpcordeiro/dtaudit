#' Compare Two Data Tables
#'
#' Compares two data.tables by examining column names, row counts, key overlap,
#' and numeric discrepancies. Useful for validating data processing pipelines.
#'
#' @param dt1 First data.table to compare.
#' @param dt2 Second data.table to compare.
#' @param key_cols Character vector of column names to use as keys for matching
#'   rows. If NULL (default), automatically detects character, factor, and
#'   integer columns as keys.
#'
#' @returns An S3 object of class `compare_dt` containing:
#' \describe{
#'   \item{name1, name2}{Names of the compared objects}
#'   \item{common_columns}{Column names present in both tables}
#'   \item{only_dt1}{Column names only in dt1}
#'   \item{only_dt2}{Column names only in dt2}
#'   \item{type_mismatches}{Data.table of columns with same name but different
#'     types, with columns: column, type_dt1, type_dt2. NULL if no mismatches.}
#'   \item{nrow_dt1}{Number of rows in dt1}
#'   \item{nrow_dt2}{Number of rows in dt2}
#'   \item{key_summary}{Summary of key overlap (if keys found)}
#'   \item{numeric_summary}{Data.table of numeric column discrepancies}
#'   \item{numeric_method}{How numeric columns were compared ("keys", "row_index", or NA)}
#'   \item{rows_matched}{Number of rows matched on keys (when method is "keys")}
#' }
#'
#' @seealso [validate_join()] for analyzing join relationships,
#'   [validate_primary_keys()] for key uniqueness validation
#'
#' @examples
#' library(data.table)
#' dt1 <- data.table(id = 1:3, value = c(10.0, 20.0, 30.0))
#' dt2 <- data.table(id = 1:3, value = c(10.1, 20.0, 30.5))
#' compare_datatables(dt1, dt2)
#'
#' @export
compare_datatables <- function(dt1, dt2, key_cols = NULL) {
  # Capture original names from the calling environment
  name1 <- deparse(substitute(dt1))
  name2 <- deparse(substitute(dt2))

  # Ensure data.table
  if (!is.data.table(dt1)) dt1 <- as.data.table(dt1)
  if (!is.data.table(dt2)) dt2 <- as.data.table(dt2)

  ## 1. Compare number of rows ------------------------------------------------
  n1 <- nrow(dt1)
  n2 <- nrow(dt2)

  ## 2. Compare column names --------------------------------------------------
  names1 <- names(dt1)
  names2 <- names(dt2)

  common_cols <- intersect(names1, names2)
  only1      <- setdiff(names1, names2)
  only2      <- setdiff(names2, names1)

  ## 2b. Check for type mismatches in common columns ----------------------------
  type_mismatches <- NULL
  if (length(common_cols) > 0L) {
    type_list <- lapply(common_cols, function(nm) {
      type1 <- class(dt1[[nm]])[1L]
      type2 <- class(dt2[[nm]])[1L]
      if (type1 != type2) {
        data.table(column = nm, type_dt1 = type1, type_dt2 = type2)
      } else {
        NULL
      }
    })
    type_list <- Filter(Negate(is.null), type_list)
    if (length(type_list)) {
      type_mismatches <- rbindlist(type_list, use.names = TRUE)
    }
  }

  if (length(common_cols) == 0L) {
    stop("No matching column names between ", name1, " and ", name2)
  }

  ## 3. Merge on 'keys' (character/factor/integer) ----------------------------
  mismatched_cols <- if (!is.null(type_mismatches)) type_mismatches$column else character(0L)

  if (is.null(key_cols)) {
    key_candidates <- common_cols[vapply(common_cols, function(nm) {
      if (nm %in% mismatched_cols) return(FALSE)
      is1 <- is.integer(dt1[[nm]]) || is.factor(dt1[[nm]]) || is.character(dt1[[nm]])
      is2 <- is.integer(dt2[[nm]]) || is.factor(dt2[[nm]]) || is.character(dt2[[nm]])
      is1 && is2
    }, logical(1L))]
    key_cols  <- key_candidates
    auto_keys <- TRUE
  } else {
    auto_keys <- FALSE
    missing1 <- setdiff(key_cols, names1)
    missing2 <- setdiff(key_cols, names2)
    if (length(missing1) || length(missing2)) {
      stop(
        "Some key_cols not present in both data.tables.\n",
        "Missing from ", name1, ": ", paste(missing1, collapse = ", "),
        "\nMissing from ", name2, ": ", paste(missing2, collapse = ", ")
      )
    }
  }

  key_summary <- NULL
  if (length(key_cols) > 0L) {
    u1 <- unique(dt1[, ..key_cols])
    u2 <- unique(dt2[, ..key_cols])

    match_keys <- fintersect(u1, u2)
    only_dt1_keys <- fsetdiff(u1, u2)
    only_dt2_keys <- fsetdiff(u2, u1)

    key_summary <- list(
      keys       = key_cols,
      auto       = auto_keys,
      dt1_unique = nrow(u1),
      dt2_unique = nrow(u2),
      matches    = nrow(match_keys),
      only_dt1   = nrow(only_dt1_keys),
      only_dt2   = nrow(only_dt2_keys)
    )
  }

  ## 4. Numeric discrepancies for common numeric columns ----------------------
  num_cols <- common_cols[vapply(common_cols, function(nm) {
    is.numeric(dt1[[nm]]) && is.numeric(dt2[[nm]])
  }, logical(1L))]
  num_cols <- setdiff(num_cols, key_cols)

  num_summary <- NULL
  numeric_method <- NA_character_
  rows_matched <- NA_integer_

  if (length(num_cols) > 0L) {
    if (length(key_cols) == 0L) {
      numeric_method <- "row_index"
      maxn <- min(n1, n2)
      idx  <- seq_len(maxn)

      num_list <- lapply(num_cols, function(nm) {
        d <- abs(dt1[[nm]][idx] - dt2[[nm]][idx])
        d <- d[!is.na(d)]
        if (!length(d)) return(NULL)
        qs <- quantile(d, probs = c(0, 0.25, 0.5, 0.75, 1),
                       na.rm = TRUE, names = FALSE)
        data.table(
          column = nm, n = length(d),
          min = qs[1], q25 = qs[2], median = qs[3], q75 = qs[4], max = qs[5]
        )
      })

      num_list <- Filter(Negate(is.null), num_list)
      if (length(num_list)) {
        num_summary <- rbindlist(num_list, use.names = TRUE, fill = TRUE)
      }
    } else {
      numeric_method <- "keys"
      dt1_sub <- dt1[, c(key_cols, num_cols), with = FALSE]
      dt2_sub <- dt2[, c(key_cols, num_cols), with = FALSE]

      merged <- merge(
        dt1_sub, dt2_sub,
        by = key_cols, suffixes = c(".dt1", ".dt2"),
        allow.cartesian = FALSE
      )
      rows_matched <- nrow(merged)

      num_list <- lapply(num_cols, function(nm) {
        v1 <- merged[[paste0(nm, ".dt1")]]
        v2 <- merged[[paste0(nm, ".dt2")]]
        d  <- abs(v1 - v2)
        d  <- d[!is.na(d)]
        if (!length(d)) return(NULL)
        qs <- quantile(d, probs = c(0, 0.25, 0.5, 0.75, 1),
                       na.rm = TRUE, names = FALSE)
        data.table(
          column = nm, n = length(d),
          min = qs[1], q25 = qs[2], median = qs[3], q75 = qs[4], max = qs[5]
        )
      })

      num_list <- Filter(Negate(is.null), num_list)
      if (length(num_list)) {
        num_summary <- rbindlist(num_list, use.names = TRUE, fill = TRUE)
      }
    }
  }

  result <- structure(
    list(
      name1           = name1,
      name2           = name2,
      common_columns  = common_cols,
      only_dt1        = only1,
      only_dt2        = only2,
      type_mismatches = type_mismatches,
      nrow_dt1        = n1,
      nrow_dt2        = n2,
      key_summary     = key_summary,
      numeric_summary = num_summary,
      numeric_method  = numeric_method,
      rows_matched    = rows_matched
    ),
    class = "compare_dt"
  )

  result
}

#' Print Method for compare_dt Objects
#'
#' @param x A `compare_dt` object from [compare_datatables()].
#' @param ... Additional arguments (ignored).
#'
#' @returns The `compare_dt` object, invisibly.
#'
#' @export
print.compare_dt <- function(x, ...) {
  name1 <- x$name1
  name2 <- x$name2

  ## 1. Row counts
  cat("1. Number of rows\n")
  cat("   ", name1, ": ", x$nrow_dt1, " rows\n", sep = "")
  cat("   ", name2, ": ", x$nrow_dt2, " rows\n", sep = "")
  cat("   Difference (", name1, " - ", name2, "): ",
      x$nrow_dt1 - x$nrow_dt2, "\n\n", sep = "")

  ## 2. Column names
  cat("2. Column names\n")
  cat("   Matching column names :", length(x$common_columns), "\n")
  cat("   Only in ", name1, ": ", length(x$only_dt1),
      if (length(x$only_dt1)) paste0(" (", paste(x$only_dt1, collapse = ", "), ")") else "",
      "\n", sep = "")
  cat("   Only in ", name2, ": ", length(x$only_dt2),
      if (length(x$only_dt2)) paste0(" (", paste(x$only_dt2, collapse = ", "), ")") else "",
      "\n", sep = "")

  n_mismatches <- if (is.null(x$type_mismatches)) 0L else nrow(x$type_mismatches)
  cat("   Type mismatches  :", n_mismatches, "\n")
  if (n_mismatches > 0L) {
    print(x$type_mismatches)
  }
  cat("\n")

  ## 3. Key summary
  cat("3. Key columns used for matching\n")
  ks <- x$key_summary
  if (is.null(ks)) {
    cat("   No common character/factor/integer columns found. Skipping key-based match.\n\n")
  } else {
    cat("   Key columns: ", paste(ks$keys, collapse = ", "),
        if (ks$auto) " (auto-detected)" else "", "\n", sep = "")
    cat("   Distinct key combinations in ", name1, ": ", ks$dt1_unique, "\n", sep = "")
    cat("   Distinct key combinations in ", name2, ": ", ks$dt2_unique, "\n", sep = "")
    cat("   Matching key combinations: ", ks$matches, "\n", sep = "")
    cat("   Only in ", name1, ": ", ks$only_dt1, "\n", sep = "")
    cat("   Only in ", name2, ": ", ks$only_dt2, "\n\n", sep = "")
  }

  ## 4. Numeric discrepancies
  cat("4. Numeric column discrepancies (absolute differences)\n")
  if (is.na(x$numeric_method)) {
    cat("   No common numeric columns found.\n")
  } else if (x$numeric_method == "row_index") {
    cat("   No key columns; comparing numeric columns by row index ",
        "(1..min(nrow(", name1, "), nrow(", name2, "))).\n", sep = "")
  } else {
    cat("   Comparing numeric columns after merging on keys.\n")
    cat("   Rows matched on keys:", x$rows_matched, "\n")
  }

  if (!is.null(x$numeric_summary) && nrow(x$numeric_summary) > 0L) {
    print(x$numeric_summary)
  }

  invisible(x)
}
