#' Check Monthly Date Coverage
#'
#' Verifies whether a date vector contains data for all months within a
#' specified date range. Reports any missing months.
#'
#' @param date_var A vector of dates to check.
#' @param start_date Character or Date. Start of the expected date range
#'   (format: "YYYY-MM-DD").
#' @param end_date Character or Date. End of the expected date range
#'   (format: "YYYY-MM-DD").
#'
#' @returns Called for side effects (prints missing months). Returns NULL
#'   invisibly.
#'
#' @examples
#' library(data.table)
#' dates <- as.IDate(c("2023-01-15", "2023-02-20", "2023-04-10"))
#' check_months_coverage(dates, "2023-01-01", "2023-04-30")
#'
#' @export
check_months_coverage <- function(date_var, start_date, end_date) {
  cat("Checking dates between", as.character(start_date), "and", as.character(end_date), "\n")
  all_months <- seq(as.IDate(start_date), as.IDate(end_date), by = "month")
  actual_months <- unique(as.IDate(format(date_var, "%Y-%m-01")))
  missing_months <- as.IDate(setdiff(all_months, actual_months))
  if (length(missing_months) == 0) {
    cat("All months show up in the data!\n")
  } else {
    cat("There are", length(missing_months), "months missing. These are:\n")
    missing_formatted <- format(missing_months, "%b-%Y")
    cat(paste(missing_formatted, collapse = ", "), "\n")
  }
  invisible(NULL)
}

#' Summarize a Single Vector
#'
#' Computes summary statistics for a vector. Handles numeric, character,
#' logical, Date, and other types with appropriate statistics for each.
#'
#' @param x A vector to summarize.
#'
#' @returns A named character vector with summary statistics including:
#'   type, missing count, most frequent value (for non-numeric), mean, sd,
#'   min, quartiles (q25, q50, q75), max, and three example values.
#'
#' @examples
#' summarize_data(c(1, 2, 3, NA, 5))
#' summarize_data(c("a", "b", "a", "c"))
#'
#' @export
summarize_data <- function(x) {
  x_NAs <- is.na(x)
  x_no_NAs <- x[!x_NAs]
  n_valid <- length(x_no_NAs)
  if (is.numeric(x)) {
    result <- c(
      type = "numeric",
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
  } else if (is.character(x)) {
    result <- c(
      type = "character",
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
#'
#' @returns A data.table with one row per column containing summary statistics.
#'
#' @note Requires the \pkg{pbapply} package for progress bars.
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' dt <- data.table(
#'   id = 1:100,
#'   value = rnorm(100),
#'   category = sample(letters[1:5], 100, replace = TRUE)
#' )
#' get_summary_table(dt)
#' }
#'
#' @export
get_summary_table <- function(dt) {
  if (!requireNamespace("pbapply", quietly = TRUE)) {
    stop("Package 'pbapply' is required for get_summary_table(). ",
         "Please install it with install.packages('pbapply').")
  }
  # TODO: if pbapply is not available, fall back to lapply without progress bar
  dt_summary <- dt[, pbapply::pblapply(dt, summarize_data)]
  dt_summary[, row_names := names(summarize_data(c(1, 1, 1)))]
  transpose(dt_summary, keep.names = "variable", make.names = "row_names")
}
