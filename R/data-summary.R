#' Check Date Coverage
#'
#' Verifies whether a date vector contains data for all periods within a
#' specified date range. Reports any missing periods.
#'
#' @param date_var A vector of dates to check.
#' @param start_date Character or Date. Start of the expected date range
#'   (format: "YYYY-MM-DD").
#' @param end_date Character or Date. End of the expected date range
#'   (format: "YYYY-MM-DD").
#' @param by Character. Period granularity: one of `"day"`, `"week"`,
#'   `"month"`, `"quarter"`, or `"year"`. Defaults to `"month"`.
#' @param quiet Logical. If `TRUE`, suppresses printed output. Defaults to
#'   `FALSE`.
#'
#' @returns An [IDate][data.table::IDate] vector of missing periods, returned
#'   invisibly.
#'
#' @seealso [check_months_coverage()] for a convenience wrapper with
#'   `by = "month"`
#'
#' @examples
#' library(data.table)
#' dates <- as.IDate(c("2023-01-15", "2023-02-20", "2023-04-10"))
#' check_date_coverage(dates, "2023-01-01", "2023-04-30")
#' check_date_coverage(dates, "2023-01-01", "2023-04-30", by = "quarter")
#'
#' @export
check_date_coverage <- function(date_var, start_date, end_date, by = "month",
                                quiet = FALSE) {
  valid_by <- c("day", "week", "month", "quarter", "year")
  if (!by %in% valid_by) {
    stop("`by` must be one of: ", paste(valid_by, collapse = ", "))
  }
  if (!quiet) {
    cat("Checking dates between", as.character(start_date), "and",
        as.character(end_date), "(by", by, ")\n")
  }
  all_periods <- seq(as.IDate(start_date), as.IDate(end_date), by = by)
  trunc_format <- switch(by,
    day     = "%Y-%m-%d",
    week    = "%Y-%m-%d",
    month   = "%Y-%m-01",
    quarter = NULL,
    year    = "%Y-01-01"
  )
  if (by == "quarter") {
    actual_dates <- as.Date(date_var)
    actual_periods <- unique(as.IDate(
      paste0(format(actual_dates, "%Y-"),
             sprintf("%02d", (as.integer(format(actual_dates, "%m")) - 1L) %/% 3L * 3L + 1L),
             "-01")
    ))
  } else {
    actual_periods <- unique(as.IDate(format(date_var, trunc_format)))
  }
  missing_periods <- as.IDate(setdiff(all_periods, actual_periods))
  if (!quiet) {
    if (length(missing_periods) == 0L) {
      cat("All", by, "periods show up in the data!\n")
    } else {
      display_format <- switch(by,
        day     = "%Y-%m-%d",
        week    = "%Y-%m-%d",
        month   = "%b-%Y",
        quarter = "%b-%Y",
        year    = "%Y"
      )
      cat("There are", length(missing_periods), by, "periods missing. These are:\n")
      cat(paste(format(missing_periods, display_format), collapse = ", "), "\n")
    }
  }
  invisible(missing_periods)
}

#' Check Monthly Date Coverage
#'
#' Verifies whether a date vector contains data for all months within a
#' specified date range. Reports any missing months.
#'
#' This is a convenience wrapper around [check_date_coverage()] with
#' `by = "month"`.
#'
#' @inheritParams check_date_coverage
#'
#' @returns An [IDate][data.table::IDate] vector of missing months, returned
#'   invisibly.
#'
#' @seealso [check_date_coverage()] for other period granularities
#'
#' @examples
#' library(data.table)
#' dates <- as.IDate(c("2023-01-15", "2023-02-20", "2023-04-10"))
#' check_months_coverage(dates, "2023-01-01", "2023-04-30")
#'
#' @export
check_months_coverage <- function(date_var, start_date, end_date, quiet = FALSE) {
  check_date_coverage(date_var, start_date, end_date, by = "month", quiet = quiet)
}

# Statistic names returned by summarize_vector() — used as row names in
# get_summary_table(). Defined once so both functions stay in sync.
.summary_stat_names <- c(
  "type", "n_unique", "missing", "most_frequent",
  "mean", "sd", "min", "q25", "q50", "q75", "max",
  "example1", "example2", "example3"
)

#' Summarize a Single Vector
#'
#' Computes summary statistics for a vector. Handles numeric, character,
#' factor, logical, Date, and other types with appropriate statistics for each.
#'
#' @param x A vector to summarize.
#'
#' @returns A named character vector with summary statistics including:
#'   type, unique count, missing count, most frequent value (for non-numeric),
#'   mean, sd, min, quartiles (q25, q50, q75), max, and three example values.
#'
#' @examples
#' summarize_vector(c(1, 2, 3, NA, 5))
#' summarize_vector(c("a", "b", "a", "c"))
#'
#' @export
summarize_vector <- function(x) {
  x_NAs <- is.na(x)
  x_no_NAs <- x[!x_NAs]
  n_valid <- length(x_no_NAs)
  n_uniq <- length(unique(x_no_NAs))
  if (is.numeric(x)) {
    result <- c(
      type = "numeric",
      n_unique = n_uniq,
      missing = sum(x_NAs),
      most_frequent = NA,
      mean = if (n_valid) mean(x_no_NAs) else NA,
      sd = if (n_valid > 1L) sd(x_no_NAs) else NA,
      min = if (n_valid) min(x_no_NAs) else NA,
      q25 = if (n_valid) quantile(x_no_NAs, 0.25, names = FALSE) else NA,
      q50 = if (n_valid) quantile(x_no_NAs, 0.5, names = FALSE) else NA,
      q75 = if (n_valid) quantile(x_no_NAs, 0.75, names = FALSE) else NA,
      max = if (n_valid) max(x_no_NAs) else NA,
      example1 = x_no_NAs[1],
      example2 = x_no_NAs[2],
      example3 = x_no_NAs[3]
    )
  } else if (is.factor(x)) {
    x_chr <- as.character(x_no_NAs)
    result <- c(
      type = "factor",
      n_unique = n_uniq,
      missing = sum(x_NAs),
      most_frequent = most_frequent(x),
      mean = NA,
      sd = NA,
      min = if (n_valid) min(x_chr) else NA,
      q25 = NA,
      q50 = NA,
      q75 = NA,
      max = if (n_valid) max(x_chr) else NA,
      example1 = x_no_NAs[1],
      example2 = x_no_NAs[2],
      example3 = x_no_NAs[3]
    )
  } else if (is.character(x)) {
    result <- c(
      type = "character",
      n_unique = n_uniq,
      missing = sum(x_NAs),
      most_frequent = most_frequent(x),
      mean = NA,
      sd = NA,
      min = if (n_valid) min(x_no_NAs) else NA,
      q25 = NA,
      q50 = NA,
      q75 = NA,
      max = if (n_valid) max(x_no_NAs) else NA,
      example1 = x_no_NAs[1],
      example2 = x_no_NAs[2],
      example3 = x_no_NAs[3]
    )
  } else if (is.logical(x)) {
    result <- c(
      type = "logical",
      n_unique = n_uniq,
      missing = sum(x_NAs),
      most_frequent = most_frequent(x),
      mean = mean(x_no_NAs),
      sd = sd(x_no_NAs),
      min = NA,
      q25 = NA,
      q50 = NA,
      q75 = NA,
      max = NA,
      example1 = x_no_NAs[1],
      example2 = x_no_NAs[2],
      example3 = x_no_NAs[3]
    )
  } else if ("Date" %in% class(x)) {
    result <- c(
      type = "Date",
      n_unique = n_uniq,
      missing = sum(x_NAs),
      most_frequent = most_frequent(x),
      mean = NA,
      sd = NA,
      min = if (n_valid) as.character(min(x_no_NAs)) else NA,
      q25 = NA,
      q50 = NA,
      q75 = NA,
      max = if (n_valid) as.character(max(x_no_NAs)) else NA,
      example1 = x_no_NAs[1],
      example2 = x_no_NAs[2],
      example3 = x_no_NAs[3]
    )
  } else {
    result <- c(
      type = class(x)[1],
      n_unique = n_uniq,
      missing = sum(x_NAs),
      most_frequent = most_frequent(x),
      mean = NA,
      sd = NA,
      min = NA,
      q25 = NA,
      q50 = NA,
      q75 = NA,
      max = NA,
      example1 = x_no_NAs[1],
      example2 = x_no_NAs[2],
      example3 = x_no_NAs[3]
    )
  }
  return(result)
}

#' Generate Summary Table for a Data Table
#'
#' Creates a comprehensive summary of all columns in a data.table, including
#' type, missing values, descriptive statistics, and example values.
#'
#' @param dt A data.table to summarize.
#' @param cols Optional character vector of column names to summarize. If
#'   `NULL` (the default), all columns are summarized.
#'
#' @returns A data.table with one row per column containing summary statistics.
#'
#' @seealso [summarize_vector()] for single-vector summaries,
#'   [diagnose_nas()] for missing value diagnostics
#'
#' @examples
#' library(data.table)
#' dt <- data.table(
#'   id = 1:100,
#'   value = rnorm(100),
#'   category = sample(letters[1:5], 100, replace = TRUE)
#' )
#' get_summary_table(dt)
#' get_summary_table(dt, cols = c("value", "category"))
#'
#' @export
get_summary_table <- function(dt, cols = NULL) {
  stopifnot(is.data.table(dt))
  if (!is.null(cols)) {
    if (!all(cols %chin% names(dt))) {
      missing_cols <- setdiff(cols, names(dt))
      stop(sprintf("Column(s) not found in `dt`: %s",
                   paste(missing_cols, collapse = ", ")))
    }
    dt <- dt[, .SD, .SDcols = cols]
  }
  apply_fun <- if (requireNamespace("pbapply", quietly = TRUE)) {
    pbapply::pblapply
  } else {
    lapply
  }
  dt_summary <- dt[, apply_fun(dt, summarize_vector)]
  dt_summary[, row_names := .summary_stat_names]
  transpose(dt_summary, keep.names = "variable", make.names = "row_names")
}

#' Diagnose Missing Values
#'
#' Reports NA counts and percentages for each column in a data.table,
#' sorted by missing percentage in descending order.
#'
#' @param dt A data.table to diagnose.
#'
#' @returns An S3 object of class `diagnose_na` containing:
#' \describe{
#'   \item{table}{A data.table with columns `variable`, `n_na`, `pct_na`, and
#'     `n_valid`, sorted by `pct_na` descending.}
#'   \item{n_cols}{Total number of columns in the input.}
#'   \item{n_with_na}{Number of columns that have at least one NA.}
#' }
#'
#' @seealso [get_summary_table()] for comprehensive column summaries,
#'   [diagnose_strings()] for string column quality
#'
#' @examples
#' library(data.table)
#' dt <- data.table(
#'   a = c(1, NA, 3),
#'   b = c(NA, NA, "x"),
#'   c = c(TRUE, FALSE, TRUE)
#' )
#' diagnose_nas(dt)
#'
#' @export
diagnose_nas <- function(dt) {
  stopifnot(is.data.table(dt))
  n_rows <- nrow(dt)
  na_counts <- vapply(dt, function(col) sum(is.na(col)), integer(1L))
  tbl <- data.table(
    variable = names(na_counts),
    n_na     = as.integer(na_counts),
    pct_na   = round(100 * na_counts / n_rows, 1),
    n_valid  = as.integer(n_rows - na_counts)
  )
  setorderv(tbl, "pct_na", order = -1L)

  structure(
    list(
      table     = tbl,
      n_cols    = ncol(dt),
      n_with_na = sum(tbl$n_na > 0L)
    ),
    class = "diagnose_na"
  )
}

#' Print Method for diagnose_na Objects
#'
#' @param x A `diagnose_na` object from [diagnose_nas()].
#' @param ... Additional arguments (unused).
#'
#' @returns The `diagnose_na` object, invisibly.
#'
#' @export
print.diagnose_na <- function(x, ...) {
  cat(sprintf("%d of %d columns have missing values\n", x$n_with_na, x$n_cols))
  if (x$n_with_na > 0L) {
    cols_with_na <- x$table[n_na > 0L]
    cat(sprintf("  %-30s %8s %8s\n", "variable", "n_na", "pct_na"))
    for (i in seq_len(nrow(cols_with_na))) {
      cat(sprintf("  %-30s %8d %7.1f%%\n",
                  cols_with_na$variable[i],
                  cols_with_na$n_na[i],
                  cols_with_na$pct_na[i]))
    }
  }
  invisible(x)
}
