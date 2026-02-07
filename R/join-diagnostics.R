#' Validate Join Operations Between Two Data Tables
#'
#' Analyzes a potential join between two data.tables without performing the
#' full join between original tables. Reports relationship type
#' (one-to-one, one-to-many, etc.), match rates, duplicate keys, and 
#' unmatched rows. Optionally tracks a numeric statistic column through the
#' join to quantify impact.
#'
#' @param x A data.table (left table).
#' @param y A data.table (right table).
#' @param by Character vector of column names to join on (used for both tables).
#' @param by.x Character vector of column names in `x` to join on.
#' @param by.y Character vector of column names in `y` to join on.
#' @param stat Character string naming a numeric column present in both tables.
#'   Reports total, matched, and unmatched sums for each table.
#' @param stat.x Character string naming a numeric column in `x`.
#' @param stat.y Character string naming a numeric column in `y`.
#'
#' @returns An S3 object of class `validate_join` containing:
#' \describe{
#'   \item{x_name, y_name}{Names of the input tables from the original call}
#'   \item{by.x, by.y}{Key columns used for the join}
#'   \item{counts}{List with row counts, match rates, and overlap statistics}
#'   \item{stat}{When `stat`, `stat.x`, or `stat.y` is provided, a list with
#'     elements `stat_col_x` and/or `stat_col_y` (column names) and sublists
#'     `x` and/or `y` each containing `total`, `matched`, `only`, `rate`, and
#'     `n_na`. `NULL` when no stat is provided.}
#'   \item{duplicates}{List with duplicate key information for each table}
#'   \item{summary_table}{A data.table summarizing the join diagnostics}
#'   \item{relation}{Character string: "one-to-one", "one-to-many", "many-to-one",
#'     "many-to-many", or "no matches"}
#'   \item{keys_only_in_x}{Keys present in x but not in y}
#'   \item{keys_only_in_y}{Keys present in y but not in x}
#' }
#'
#' @seealso [validate_primary_keys()] for key uniqueness validation,
#'   [validate_var_relationship()] for variable relationship analysis,
#'   [compare_datatables()] for structural comparison
#'
#' @examples
#' library(data.table)
#' dt1 <- data.table(id = c(1, 2, 3, 3), value = c("a", "b", "c", "d"))
#' dt2 <- data.table(id = c(2, 3, 4), score = c(10, 20, 30))
#' result <- validate_join(dt1, dt2, by = "id")
#' print(result)
#'
#' # Track a numeric column through the join
#' orders <- data.table(id = 1:4, revenue = c(100, 200, 300, 400))
#' products <- data.table(id = 2:5, cost = c(10, 20, 30, 40))
#' validate_join(orders, products, by = "id", stat.x = "revenue", stat.y = "cost")
#'
#' @export
validate_join <- function(x, y, by = NULL, by.x = NULL, by.y = NULL,
                          stat = NULL, stat.x = NULL, stat.y = NULL) {
  stopifnot(is.data.table(x), is.data.table(y))

  # Capture names from the call
  x_name <- deparse(substitute(x))
  y_name <- deparse(substitute(y))

  # Resolve join columns
  if (!is.null(by)) {
    by.x <- by.y <- by
  }
  if (!is.character(by.x) || length(by.x) == 0L)
    stop("`by.x` must be a non-empty character vector.")
  if (!is.character(by.y) || length(by.y) == 0L)
    stop("`by.y` must be a non-empty character vector.")

  # Column checks (fast path with %chin%)
  if (!all(by.x %chin% names(x))) {
    miss_x <- setdiff(by.x, names(x))
    stop(sprintf("`x` missing key(s): %s", paste(miss_x, collapse = ", ")))
  }
  if (!all(by.y %chin% names(y))) {
    miss_y <- setdiff(by.y, names(y))
    stop(sprintf("`y` missing key(s): %s", paste(miss_y, collapse = ", ")))
  }

  # Resolve stat columns (mirrors by/by.x/by.y pattern)
  if (!is.null(stat)) {
    if (!is.null(stat.x) || !is.null(stat.y))
      stop("Use `stat` or `stat.x`/`stat.y`, not both.")
    stat.x <- stat.y <- stat
  }
  have_stat_x <- !is.null(stat.x)
  have_stat_y <- !is.null(stat.y)

  if (have_stat_x) {
    if (!is.character(stat.x) || length(stat.x) != 1L)
      stop("`stat.x` must be a single column name (character string).")
    if (!stat.x %chin% names(x))
      stop(sprintf("Column '%s' not found in `x`.", stat.x))
    if (!is.numeric(x[[stat.x]]))
      stop(sprintf("Column '%s' in `x` is not numeric.", stat.x))
  }
  if (have_stat_y) {
    if (!is.character(stat.y) || length(stat.y) != 1L)
      stop("`stat.y` must be a single column name (character string).")
    if (!stat.y %chin% names(y))
      stop(sprintf("Column '%s' not found in `y`.", stat.y))
    if (!is.numeric(y[[stat.y]]))
      stop(sprintf("Column '%s' in `y` is not numeric.", stat.y))
  }

  # Row counts
  x_rows <- nrow(x)
  y_rows <- nrow(y)

  # Aggregate to counts per key (no cartesian explosion)
  xc <- x[, .N, keyby = by.x] # Nx per key (sorted & keyed)
  yc <- y[, .N, keyby = by.y] # Ny per key

  x_unique <- nrow(xc)
  y_unique <- nrow(yc)
  x_has_dups <- any(xc[["N"]] > 1L)
  y_has_dups <- any(yc[["N"]] > 1L)

  # Merge aggregate counts
  comb <- merge(
    xc, yc,
    by.x = by.x, by.y = by.y,
    all = TRUE, suffixes = c(".x", ".y")
  )

  # Convenient Nx, Ny vectors (0 where absent)
  Nx <- comb[["N.x"]]
  Ny <- comb[["N.y"]]
  if (is.null(Nx)) Nx <- integer(0)
  if (is.null(Ny)) Ny <- integer(0)
  Nx0 <- fifelse(is.na(Nx), 0L, Nx)
  Ny0 <- fifelse(is.na(Ny), 0L, Ny)

  # Merge metrics
  n_matched      <- sum(as.double(Nx0) * as.double(Ny0))  # matched row *pairs*
  n_only_x       <- sum(Nx0[Ny0 == 0L])
  n_only_y       <- sum(Ny0[Nx0 == 0L])
  n_key_overlap  <- sum(Nx0 > 0L & Ny0 > 0L)
  match_rate_x   <- 100 * ((x_rows - n_only_x) / x_rows)
  match_rate_y   <- 100 * ((y_rows - n_only_y) / y_rows)

  # Stat diagnostics
  stat_info <- NULL
  if (have_stat_x || have_stat_y) {
    matched_key_rows <- comb[Nx0 > 0L & Ny0 > 0L]
    stat_info <- list()

    if (have_stat_x) {
      stat_total_x   <- x[, sum(get(stat.x), na.rm = TRUE)]
      stat_na_x      <- x[, sum(is.na(get(stat.x)))]
      matched_keys_x <- matched_key_rows[, by.x, with = FALSE]
      stat_matched_x <- if (nrow(matched_keys_x) > 0L) {
        x[matched_keys_x, sum(get(stat.x), na.rm = TRUE),
          on = by.x, nomatch = NULL]
      } else {
        0
      }
      stat_only_x    <- stat_total_x - stat_matched_x
      stat_rate_x    <- if (stat_total_x != 0) {
        100 * stat_matched_x / stat_total_x
      } else {
        NA_real_
      }
      stat_info$stat_col_x <- stat.x
      stat_info$x <- list(
        total = stat_total_x, matched = stat_matched_x,
        only = stat_only_x, rate = stat_rate_x, n_na = stat_na_x
      )
    }

    if (have_stat_y) {
      stat_total_y   <- y[, sum(get(stat.y), na.rm = TRUE)]
      stat_na_y      <- y[, sum(is.na(get(stat.y)))]
      matched_keys_y <- copy(matched_key_rows[, by.x, with = FALSE])
      if (!identical(by.x, by.y)) setnames(matched_keys_y, by.x, by.y)
      stat_matched_y <- if (nrow(matched_keys_y) > 0L) {
        y[matched_keys_y, sum(get(stat.y), na.rm = TRUE),
          on = by.y, nomatch = NULL]
      } else {
        0
      }
      stat_only_y    <- stat_total_y - stat_matched_y
      stat_rate_y    <- if (stat_total_y != 0) {
        100 * stat_matched_y / stat_total_y
      } else {
        NA_real_
      }
      stat_info$stat_col_y <- stat.y
      stat_info$y <- list(
        total = stat_total_y, matched = stat_matched_y,
        only = stat_only_y, rate = stat_rate_y, n_na = stat_na_y
      )
    }
  }

  # Relationship classification
  merge_type <- if (n_key_overlap == 0L) {
    "no matches"
  } else if (!x_has_dups && !y_has_dups) {
    "one-to-one"
  } else if (!x_has_dups && y_has_dups) {
    "one-to-many"
  } else if (x_has_dups && !y_has_dups) {
    "many-to-one"
  } else {
    "many-to-many"
  }

  # Build summary table
  fmt_int <- function(z) format(z, big.mark = ",", scientific = FALSE, trim = TRUE)
  key_lbl_x <- paste(by.x, collapse = ", ")
  key_lbl_y <- paste(by.y, collapse = ", ")

  summary_table <- data.table(
    Item  = c(
      "Relationship",
      sprintf("Key(s) in %s   [%s]", x_name, key_lbl_x),
      sprintf("Key(s) in %s   [%s]", y_name, key_lbl_y),
      sprintf("Rows in %s", x_name),
      sprintf("Distinct key combos in %s", x_name),
      sprintf("Rows in %s", y_name),
      sprintf("Distinct key combos in %s", y_name),
      "Overlapping distinct key combos",
      "Matched row pairs (cartesian)",
      sprintf("Match rate from %s", x_name),
      sprintf("Match rate from %s", y_name),
      sprintf("Rows only in %s (no match in %s)", x_name, y_name),
      sprintf("Rows only in %s (no match in %s)", y_name, x_name)
    ),
    Value = c(
      merge_type,
      sprintf("(%d col%s)", length(by.x), ifelse(length(by.x) == 1L, "", "s")),
      sprintf("(%d col%s)", length(by.y), ifelse(length(by.y) == 1L, "", "s")),
      fmt_int(x_rows),
      fmt_int(x_unique),
      fmt_int(y_rows),
      fmt_int(y_unique),
      fmt_int(n_key_overlap),
      fmt_int(n_matched),
      sprintf("%.2f%%", match_rate_x),
      sprintf("%.2f%%", match_rate_y),
      fmt_int(n_only_x),
      fmt_int(n_only_y)
    )
  )

  # Return a structured object
  out <- list(
    x_name = x_name,
    y_name = y_name,
    by.x = by.x,
    by.y = by.y,
    counts = list(
      x_rows = x_rows, y_rows = y_rows,
      x_unique = x_unique, y_unique = y_unique,
      n_key_overlap = n_key_overlap,
      n_matched_pairs = n_matched,
      match_rate_x = match_rate_x,
      match_rate_y = match_rate_y,
      n_only_x = n_only_x,
      n_only_y = n_only_y
    ),
    stat = stat_info,
    duplicates = list(
      x_has_dups = x_has_dups, y_has_dups = y_has_dups,
      x_dupe_keys = xc[N > 1L],
      y_dupe_keys = yc[N > 1L]
    ),
    summary_table = summary_table,
    relation = merge_type,
    keys_only_in_x = comb[is.na(N.y)],
    keys_only_in_y = comb[is.na(N.x)]
  )
  class(out) <- c("validate_join", "list")
  out
}

#' Print Method for validate_join Objects
#'
#' Displays a compact summary of join diagnostics.
#'
#' @param x A `validate_join` object.
#' @param ... Additional arguments (ignored).
#'
#' @returns Invisibly returns the input object.
#'
#' @export
print.validate_join <- function(x, ...) {
  cat("\n============== Join Validation Summary ==============\n", sep = "")
  # Tables line
  cat("Tables: ", x$x_name, " <--> ", x$y_name, "\n", sep = "")
  # Keys for each table
  cat("Keys in ", x$x_name, ": ", paste(x$by.x, collapse = ", "), "\n", sep = "")
  cat("Keys in ", x$y_name, ": ", paste(x$by.y, collapse = ", "), "\n", sep = "")

  # Print formatted table
  tbl <- x$summary_table
  w <- max(nchar(tbl$Item), 12L)
  for (i in seq_len(nrow(tbl))) {
    cat(sprintf("  %-*s : %s\n", w, tbl$Item[i], tbl$Value[i]))
  }

  # Stat diagnostics section
  if (!is.null(x$stat)) {
    fmt_num <- function(z) format(z, big.mark = ",", scientific = FALSE, trim = TRUE)
    has_x <- !is.null(x$stat$x)
    has_y <- !is.null(x$stat$y)
    same_col <- has_x && has_y && identical(x$stat$stat_col_x, x$stat$stat_col_y)

    # Build header
    if (same_col) {
      stat_hdr <- sprintf("--- Stat: %s ---", x$stat$stat_col_x)
    } else if (has_x && has_y) {
      stat_hdr <- sprintf("--- Stat: %s (%s), %s (%s) ---",
                          x$stat$stat_col_x, x$x_name,
                          x$stat$stat_col_y, x$y_name)
    } else if (has_x) {
      stat_hdr <- sprintf("--- Stat: %s (%s) ---", x$stat$stat_col_x, x$x_name)
    } else {
      stat_hdr <- sprintf("--- Stat: %s (%s) ---", x$stat$stat_col_y, x$y_name)
    }
    cat("\n  ", stat_hdr, "\n", sep = "")

    # Build stat items and compute max width
    stat_items <- character(0)
    stat_vals  <- character(0)

    if (has_x) {
      sx <- x$stat$x
      scx <- x$stat$stat_col_x
      stat_items <- c(stat_items,
        sprintf("Total %s in %s", scx, x$x_name),
        sprintf("Matched %s in %s", scx, x$x_name),
        sprintf("Unmatched %s in %s", scx, x$x_name)
      )
      stat_vals <- c(stat_vals,
        fmt_num(sx$total),
        sprintf("%s  (%.2f%%)", fmt_num(sx$matched), sx$rate),
        sprintf("%s  (%.2f%%)", fmt_num(sx$only), if (!is.na(sx$rate)) 100 - sx$rate else NA_real_)
      )
    }
    if (has_y) {
      sy <- x$stat$y
      scy <- x$stat$stat_col_y
      stat_items <- c(stat_items,
        sprintf("Total %s in %s", scy, x$y_name),
        sprintf("Matched %s in %s", scy, x$y_name),
        sprintf("Unmatched %s in %s", scy, x$y_name)
      )
      stat_vals <- c(stat_vals,
        fmt_num(sy$total),
        sprintf("%s  (%.2f%%)", fmt_num(sy$matched), sy$rate),
        sprintf("%s  (%.2f%%)", fmt_num(sy$only), if (!is.na(sy$rate)) 100 - sy$rate else NA_real_)
      )
    }

    sw <- max(nchar(stat_items), 12L)
    for (i in seq_along(stat_items)) {
      cat(sprintf("  %-*s : %s\n", sw, stat_items[i], stat_vals[i]))
    }
  }

  # Small footer about duplicates
  du <- x$duplicates
  dup_msg <- sprintf("Duplicates: %s=%s  %s=%s",
                     x$x_name, if (du$x_has_dups) "yes" else "no",
                     x$y_name, if (du$y_has_dups) "yes" else "no")
  cat(rep("-", nchar(dup_msg)), "\n", dup_msg, "\n", sep = "")
  invisible(x)
}

#' Summary Method for validate_join Objects
#'
#' Returns the summary table from a validate_join object.
#'
#' @param object A `validate_join` object.
#' @param ... Additional arguments (ignored).
#'
#' @returns Invisibly returns the summary data.table.
#'
#' @export
summary.validate_join <- function(object, ...) {
  print(object)
  invisible(object$summary_table)
}
