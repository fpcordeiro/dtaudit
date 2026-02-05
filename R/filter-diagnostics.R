#' Filter Data with Diagnostic Statistics
#'
#' Filters a data.table while reporting statistics about dropped rows and
#' optionally the sum of a statistic column that was dropped.
#'
#' @param x A data.table or other object.
#' @param ... Arguments passed to methods.
#'
#' @returns The filtered data.table.
#'
#' @export
filter_keep <- function(x, ...) UseMethod("filter_keep")

#' @describeIn filter_keep Method for data.table objects
#'
#' @param expr A filtering expression written in terms of columns of x.
#'   Rows where expr is TRUE are KEPT; others are dropped.
#' @param stat An unquoted column or expression to total, e.g., sales,
#'   price*qty, etc. Reports the amount dropped and its share of total.
#' @param na_as Logical. Treat NA results of expr as this value
#'   (default FALSE: drop rows where expr is NA).
#' @param quiet Logical. If TRUE, suppress printing diagnostics.
#' @param warn_threshold Numeric between 0 and 1. If set and the proportion of
#'   dropped rows exceeds this threshold, a warning is issued.
#'
#' @examples
#' library(data.table)
#' DT <- data.table(
#'   id = 1:6,
#'   keep = c(TRUE, FALSE, TRUE, NA, TRUE, FALSE),
#'   sales = c(100, 50, 200, 25, NA, 75)
#' )
#'
#' # Keep rows where keep == TRUE; report dropped statistics
#' DT2 <- filter_keep(DT, keep == TRUE)
#'
#' # Also report dropped sales value
#' DT3 <- filter_keep(DT, keep == TRUE, stat = sales)
#'
#' @export
filter_keep.data.table <- function(x, expr, stat = NULL, na_as = FALSE,
                                    quiet = FALSE, warn_threshold = NULL, ...) {
  if (!is.data.table(x)) stop("x must be a data.table")

  x_name <- deparse(substitute(x))
  expr_lab <- paste(deparse(substitute(expr)), collapse = " ")

  pred <- eval(substitute(expr), envir = x, enclos = parent.frame())
  if (!is.logical(pred) || length(pred) != nrow(x)) {
    stop("`expr` must evaluate to a logical vector of length nrow(x).")
  }

  have_stat <- !missing(stat)
  stat_expr <- if (have_stat) substitute(stat) else NULL

  .filter_diagnostic(x, pred, stat_expr, na_as, quiet, warn_threshold,
                     mode = "keep", x_name, expr_lab, have_stat)
}

#' Drop Data with Diagnostic Statistics
#'
#' Filters a data.table by DROPPING rows where the expression is TRUE,
#' while reporting statistics about dropped rows and optionally the sum of
#' a statistic column that was dropped.
#'
#' @param x A data.table or other object.
#' @param ... Arguments passed to methods.
#'
#' @returns The filtered data.table.
#'
#' @export
filter_drop <- function(x, ...) UseMethod("filter_drop")

#' @describeIn filter_drop Method for data.table objects
#'
#' @param expr A filtering expression written in terms of columns of x.
#'   Rows where expr is TRUE are DROPPED; others are kept.
#' @param stat An unquoted column or expression to total, e.g., sales,
#'   price*qty, etc. Reports the amount dropped and its share of total.
#' @param na_as Logical. Treat NA results of expr as this value
#'   (default FALSE: drop rows where expr is NA).
#' @param quiet Logical. If TRUE, suppress printing diagnostics.
#' @param warn_threshold Numeric between 0 and 1. If set and the proportion of
#'   dropped rows exceeds this threshold, a warning is issued.
#'
#' @examples
#' library(data.table)
#' DT <- data.table(
#'   id = 1:5,
#'   bad = c(FALSE, TRUE, FALSE, TRUE, FALSE),
#'   sales = 10:14
#' )
#'
#' # Drop rows where bad == TRUE; report dropped statistics
#' DT2 <- filter_drop(DT, bad == TRUE)
#'
#' # Also report dropped sales value
#' DT3 <- filter_drop(DT, bad == TRUE, stat = sales)
#'
#' @export
filter_drop.data.table <- function(x, expr, stat = NULL, na_as = FALSE,
                                    quiet = FALSE, warn_threshold = NULL, ...) {
  if (!is.data.table(x)) stop("x must be a data.table")

  x_name <- deparse(substitute(x))
  expr_lab <- paste(deparse(substitute(expr)), collapse = " ")

  pred <- eval(substitute(expr), envir = x, enclos = parent.frame())
  if (!is.logical(pred) || length(pred) != nrow(x)) {
    stop("`expr` must evaluate to a logical vector of length nrow(x).")
  }

  have_stat <- !missing(stat)
  stat_expr <- if (have_stat) substitute(stat) else NULL

  .filter_diagnostic(x, pred, stat_expr, na_as, quiet, warn_threshold,
                     mode = "drop", x_name, expr_lab, have_stat)
}

# Internal helper for filter_keep and filter_drop
.filter_diagnostic <- function(x, pred, stat_expr, na_as, quiet, warn_threshold,
                               mode, x_name, expr_lab, have_stat) {
  if (!is.data.table(x)) stop("x must be a data.table")

  # Handle NAs in predicate
  na_count <- sum(is.na(pred))
  if (na_count) pred[is.na(pred)] <- na_as

  # Apply filter based on mode
  if (mode == "keep") {
    filtered <- x[pred]
  } else {
    filtered <- x[!pred]
  }

  # Row stats
  n_total <- nrow(x)
  n_filtered <- nrow(filtered)
  n_drop  <- n_total - n_filtered
  share_drop <- if (n_total > 0) n_drop / n_total else NA_real_

  # Optional value stats
  if (have_stat) {
    total_val <- x[, sum(eval(stat_expr), na.rm = TRUE)]
    filtered_val <- filtered[, sum(eval(stat_expr), na.rm = TRUE)]
    drop_val  <- total_val - filtered_val
    share_val <- if (isTRUE(all.equal(total_val, 0))) NA_real_ else drop_val / total_val
    stat_lab  <- paste(deparse(stat_expr), collapse = "")
  }

  # Print summary
  func_name <- if (mode == "keep") "filter_keep" else "filter_drop"
  if (!quiet) {
    cat(sprintf("%s(%s, %s)\n", func_name, x_name, expr_lab))
    cat(sprintf("  Dropped %s of %s rows (%.2f%%).\n",
        format(n_drop, big.mark = ","), format(n_total, big.mark = ","), 100 * share_drop))
    if (have_stat) {
      cat(sprintf("  Dropped %s of %s for %s (%.2f%%).\n",
          format(drop_val, big.mark = ",", scientific = FALSE),
          format(total_val, big.mark = ",", scientific = FALSE),
          stat_lab, 100 * share_val))
    }
    if (na_count) {
      cat(sprintf("  Note: %s rows had NA in filter; treated as %s.\n",
          format(na_count, big.mark = ","), if (na_as) "KEEP" else "DROP"))
    }
  }

  # Threshold warning
  if (!is.null(warn_threshold) && !is.na(share_drop) && share_drop > warn_threshold) {
    warning(sprintf("Dropped %.1f%% of rows exceeds threshold (%.1f%%)",
                    100 * share_drop, 100 * warn_threshold), call. = FALSE)
  }

  filtered[]
}
