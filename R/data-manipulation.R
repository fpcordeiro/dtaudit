#' Embed Data into Cartesian Product Frame
#'
#' Expands a data.table to include all combinations of the specified grouping
#' variables (cartesian product). Missing combinations are filled with NA or
#' a specified value.
#'
#' @param dt A data.table to expand.
#' @param group_vars Character vector of column names defining the grouping
#'   structure.
#' @param dt_frame Optional data.table containing the target cartesian frame.
#'   If NULL (default), creates cartesian product from unique values in dt.
#' @param fill Value to fill for missing combinations. Default is NA.
#'   Set to NA to leave gaps unfilled.
#'
#' @return A data.table with all combinations of group_vars, with original
#'   data merged in.
#'
#' @examples
#' library(data.table)
#' dt <- data.table(
#'   year = c(2020, 2020, 2021),
#'   region = c("A", "B", "A"),
#'   value = c(10, 20, 30)
#' )
#' embed_into_cartesian(dt, c("year", "region"))
#'
#' @export
embed_into_cartesian <- function(dt, group_vars, dt_frame = NULL, fill = NA) {
  # Check for reserved column names that would cause collision
  reserved_cols <- c("ORIG_CJ__", "ORIG_DT__")
  collision <- intersect(reserved_cols, names(dt))
  if (length(collision)) {
    stop("Column name collision: dt contains reserved column(s): ",
         paste(collision, collapse = ", "),
         ". Please rename these columns before using embed_into_cartesian().")
  }

  if (is.null(dt_frame)) {
    # Create cartesian product of grouping variables, i.e., the frame
    dt_frame <- do.call(CJ, lapply(group_vars, function(v) sort(unique(dt[[v]]))))
    setnames(dt_frame, group_vars)
  } else if (!all(group_vars %in% names(dt_frame))) {
    stop("dt_frame must contain all group_vars variables")
  }

  # Origin markers
  dt_frame[, ORIG_CJ__ := TRUE]
  dt[, ORIG_DT__ := TRUE]

  # Merge missing combinations into dt, i.e., embed onto the frame
  result <- dt[dt_frame, on = group_vars]

  # Fill in missing gaps
  if (!is.na(fill)) {
    vars_dt <- setdiff(names(dt), group_vars)
    result[is.na(ORIG_DT__) & ORIG_CJ__ == TRUE, (vars_dt) := fill]
  }

  # Remove markers
  result[, `:=`(ORIG_DT__ = NULL, ORIG_CJ__ = NULL)]
  dt[, ORIG_DT__ := NULL]

  return(result)
}

#' Find Most Frequent Value
#'
#' Returns the most frequently occurring value in a vector. Intended for
#' categorical or integer variables; issues a warning if used on numeric data.
#'
#' @param x A vector (typically character, factor, or integer).
#'
#' @returns The most frequent value as a character string, or `NA` if the
#'   vector is empty.
#'
#' @examples
#' most_frequent(c("a", "b", "a", "c", "a"))
#' most_frequent(c(1L, 2L, 2L, 3L))
#'
#' @export
most_frequent <- function(x) {
  if (is.numeric(x)) warning("x is a numeric vector. This function is primarily for categorical/integer variables")
  tab <- table(x)
  if (length(tab) == 0L) NA_character_ else names(tab)[which.max(tab)]
}

#' Convert Empty Strings to NA
#'
#' Replaces empty strings ("") with `NA` in all character columns of a
#' data.table. Modifies the data.table by reference.
#'
#' @param dt A data.table.
#'
#' @returns The modified data.table (invisibly). The original object is
#'   modified by reference.
#'
#' @examples
#' library(data.table)
#' dt <- data.table(a = c("x", "", "z"), b = c("", "y", ""))
#' empty_char_to_NA(dt)
#' dt
#'
#' @export
empty_char_to_NA <- function(dt) {
  char_cols <- dt[, names(.SD), .SDcols = is.character]
  dt[, (char_cols) := lapply(.SD, function(x) fifelse(x == "", NA_character_, x)), .SDcols = char_cols]
  invisible(dt)
}
