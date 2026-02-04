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
#' @return Invisibly returns a list containing:
#' \describe{
#'   \item{common_columns}{Column names present in both tables}
#'   \item{only_dt1}{Column names only in dt1}
#'   \item{only_dt2}{Column names only in dt2}
#'   \item{type_mismatches}{Data.table of columns with same name but different
#'     types, with columns: column, type_dt1, type_dt2. NULL if no mismatches.}
#'   \item{nrow_dt1}{Number of rows in dt1}
#'   \item{nrow_dt2}{Number of rows in dt2}
#'   \item{key_summary}{Summary of key overlap (if keys found)}
#'   \item{numeric_summary}{Data.table of numeric column discrepancies}
#' }
#'
#' @examples
#' library(data.table)
#' dt1 <- data.table(id = 1:3, value = c(10.0, 20.0, 30.0))
#' dt2 <- data.table(id = 1:3, value = c(10.1, 20.0, 30.5))
#' compare_datatables(dt1, dt2)
#'
#' @export
compare_datatables <- function(dt1, dt2, key_cols = NULL) {
  # Ensure data.table
  if (!is.data.table(dt1)) dt1 <- as.data.table(dt1)
  if (!is.data.table(dt2)) dt2 <- as.data.table(dt2)

  ## 1. Compare number of rows ------------------------------------------------
  n1 <- nrow(dt1)
  n2 <- nrow(dt2)

  cat("1. Number of rows\n")
  cat("   dt1:", n1, "rows\n")
  cat("   dt2:", n2, "rows\n")
  cat("   Difference (dt1 - dt2):", n1 - n2, "\n\n")

  ## 2. Compare column names --------------------------------------------------
  names1 <- names(dt1)
  names2 <- names(dt2)

  common_cols <- intersect(names1, names2)
  only1      <- setdiff(names1, names2)
  only2      <- setdiff(names2, names1)

  cat("2. Column names\n")
  cat("   Matching column names :", length(common_cols), "\n")
  cat("   Only in dt1      :", length(only1),
      if (length(only1)) paste0(" (", paste(only1, collapse = ", "), ")") else "",
      "\n", sep = "")
  cat("   Only in dt2      :", length(only2),
      if (length(only2)) paste0(" (", paste(only2, collapse = ", "), ")") else "",
      "\n", sep = "")

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

  cat("   Type mismatches  :", if (is.null(type_mismatches)) 0L else nrow(type_mismatches), "\n")
  if (!is.null(type_mismatches) && nrow(type_mismatches) > 0L) {
    print(type_mismatches)
  }
  cat("\n")

  if (length(common_cols) == 0L) {
    stop("No matching column names between dt1 and dt2")
  }

  ## 3. Merge on 'keys' (character/factor/integer) ----------------------------
  # Get columns with type mismatches to exclude from key candidates

  mismatched_cols <- if (!is.null(type_mismatches)) type_mismatches$column else character(0L)

  if (is.null(key_cols)) {
    key_candidates <- common_cols[vapply(common_cols, function(nm) {
      # Skip columns with type mismatches
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
        "Missing from dt1: ", paste(missing1, collapse = ", "),
        "\nMissing from dt2: ", paste(missing2, collapse = ", ")
      )
    }
  }

  cat("3. Key columns used for matching\n")
  key_summary <- NULL

  if (length(key_cols) == 0L) {
    cat("   No common character/factor/integer columns found. Skipping key-based match.\n\n")
  } else {
    cat("   Key columns: ", paste(key_cols, collapse = ", "),
        if (auto_keys) " (auto-detected)" else "", "\n", sep = "")

    u1 <- unique(dt1[, ..key_cols])
    u2 <- unique(dt2[, ..key_cols])

    match_keys <- fintersect(u1, u2)
    only_dt1   <- fsetdiff(u1, u2)
    only_dt2   <- fsetdiff(u2, u1)

    cat("   Distinct key combinations in dt1:", nrow(u1), "\n")
    cat("   Distinct key combinations in dt2:", nrow(u2), "\n")
    cat("   Matching key combinations      :", nrow(match_keys), "\n")
    cat("   Only in dt1                    :", nrow(only_dt1), "\n")
    cat("   Only in dt2                    :", nrow(only_dt2), "\n\n")

    key_summary <- list(
      keys       = key_cols,
      dt1_unique = nrow(u1),
      dt2_unique = nrow(u2),
      matches    = nrow(match_keys),
      only_dt1   = nrow(only_dt1),
      only_dt2   = nrow(only_dt2)
    )
  }

  ## 4. Numeric discrepancies for common numeric columns ----------------------
  cat("4. Numeric column discrepancies (absolute differences)\n")

  # Find numeric columns, excluding key columns to avoid duplicates in merge
  num_cols <- common_cols[vapply(common_cols, function(nm) {
    is.numeric(dt1[[nm]]) && is.numeric(dt2[[nm]])
  }, logical(1L))]
  num_cols <- setdiff(num_cols, key_cols)

  num_summary <- NULL

  if (!length(num_cols)) {
    cat("   No common numeric columns found.\n")
  } else if (length(key_cols) == 0L) {
    # Fall back to row-wise alignment if we have no keys
    cat("   No key columns; comparing numeric columns by row index ",
        "(1..min(nrow(dt1), nrow(dt2))).\n", sep = "")

    maxn <- min(n1, n2)
    idx  <- seq_len(maxn)

    num_list <- lapply(num_cols, function(nm) {
      d <- abs(dt1[[nm]][idx] - dt2[[nm]][idx])
      d <- d[!is.na(d)]
      if (!length(d)) return(NULL)
      qs <- quantile(d, probs = c(0, 0.25, 0.5, 0.75, 1),
                     na.rm = TRUE, names = FALSE)
      data.table(
        column = nm,
        n      = length(d),
        min    = qs[1],
        q25    = qs[2],
        median = qs[3],
        q75    = qs[4],
        max    = qs[5]
      )
    })

    num_list <- Filter(Negate(is.null), num_list)
    if (length(num_list)) {
      num_summary <- rbindlist(num_list, use.names = TRUE, fill = TRUE)
    }

  } else {
    # Compare after merging on keys
    cat("   Comparing numeric columns after merging on keys.\n")

    dt1_sub <- dt1[, c(key_cols, num_cols), with = FALSE]
    dt2_sub <- dt2[, c(key_cols, num_cols), with = FALSE]

    merged <- merge(
      dt1_sub,
      dt2_sub,
      by       = key_cols,
      suffixes = c(".dt1", ".dt2"),
      allow.cartesian = FALSE
    )

    cat("   Rows matched on keys:", nrow(merged), "\n")

    num_list <- lapply(num_cols, function(nm) {
      v1 <- merged[[paste0(nm, ".dt1")]]
      v2 <- merged[[paste0(nm, ".dt2")]]
      d  <- abs(v1 - v2)
      d  <- d[!is.na(d)]
      if (!length(d)) return(NULL)
      qs <- quantile(d, probs = c(0, 0.25, 0.5, 0.75, 1),
                     na.rm = TRUE, names = FALSE)
      data.table(
        column = nm,
        n      = length(d),
        min    = qs[1],
        q25    = qs[2],
        median = qs[3],
        q75    = qs[4],
        max    = qs[5]
      )
    })

    num_list <- Filter(Negate(is.null), num_list)
    if (length(num_list)) {
      num_summary <- rbindlist(num_list, use.names = TRUE, fill = TRUE)
    }
  }

  if (!is.null(num_summary) && nrow(num_summary)) {
    print(num_summary)
  }

  invisible(list(
    common_columns  = common_cols,
    only_dt1        = only1,
    only_dt2        = only2,
    type_mismatches = type_mismatches,
    nrow_dt1        = n1,
    nrow_dt2        = n2,
    key_summary     = key_summary,
    numeric_summary = num_summary
  ))
}
