#' Clean fixest etable Output
#'
#' Converts fixest etable output to a clean data.table format, optionally
#' splitting significance stars into separate columns.
#'
#' @param ... One or more fixest model objects, or a list of fixest objects.
#' @param etable_args List of additional arguments passed to [fixest::etable()].
#'   Defaults include `tex = FALSE`, `se.below = TRUE`, `digits = 4`.
#' @param signif.col Logical. If TRUE, split significance stars into separate
#'   columns adjacent to coefficients.
#'
#' @returns A data.table with cleaned regression output.
#'
#' @note Requires the \pkg{fixest} package.
#'
#' @examples
#' \dontrun{
#' library(fixest)
#' library(data.table)
#' data(mtcars)
#' m1 <- feols(mpg ~ hp, data = mtcars)
#' m2 <- feols(mpg ~ hp + wt, data = mtcars)
#' clean_etable(m1, m2)
#' }
#'
#' @export
clean_etable <- function(..., etable_args = list(), signif.col = FALSE) {
  if (!requireNamespace("fixest", quietly = TRUE)) {
    stop("Package 'fixest' is required for clean_etable(). ",
         "Please install it with install.packages('fixest').")
  }

  defaults <- list(
    tex      = FALSE,
    se.below = TRUE,
    digits   = 4,
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10)
  )
  etable_args <- modifyList(defaults, etable_args)

  # Validate and prepare dots for etable call
  dots <- list(...)
  is_fixest_like <- function(x) inherits(x, c("fixest", "fixest_multi"))
  if (length(dots) == 1L && is.list(dots[[1]]) && !is_fixest_like(dots[[1]])) {
    # Unwrap if passed as a single list
    models <- dots[[1]]
  } else {
    # Otherwise treat dots as the models
    models <- dots
  }
  if (length(models) == 0L || !all(vapply(models, is_fixest_like, logical(1)))) {
    stop("All arguments must be fixest objects.")
  }

  etab <- do.call(fixest::etable, c(list(models), etable_args))

  dt <- as.data.table(etab)
  setnames(dt, 1L, "term")

  # Remove dashes and underscores filling entire cells
  dt[[1L]] <- stri_replace_all_regex(dt[[1L]], "^[-_]+$", " ")

  # Clean regression columns
  model_cols <- names(dt)[2:ncol(dt)]
  for (col in model_cols) {
    col_clean <- stri_replace_all_regex(dt[[col]], "^[-_]+$", " ")
    if (signif.col) {
      # Split significance codes into a separate column
      sig <- stri_extract_last_regex(dt[[col]], "(?<=\\d)\\*+")
      sig[is.na(sig)] <- " "
      sig_col <- paste0(col, "_sig")
      dt[[sig_col]] <- sig
      col_clean <- stri_replace_all_fixed(col_clean, "*", "")
    }
    dt[[col]] <- col_clean
  }

  if (signif.col) {
    # Reorder columns such that each significance column is adjacent to coefficient
    new_order <- c("term", as.vector(rbind(model_cols, paste0(model_cols, "_sig"))))
    setcolorder(dt, new_order)
    names(dt)[grepl("(_sig|term)$", names(dt))] <- ""
  }

  dt[]
}

#' Export Coefficient Table from Models
#'
#' Creates a publication-ready coefficient table from fixest regression models
#' using modelsummary. Organizes output with sections for Variables, Fit
#' Statistics, and Fixed Effects.
#'
#' @param models A list of fixest model objects.
#' @param models_names Optional character vector of model names (currently unused).
#'
#' @returns A data.table with formatted regression output.
#'
#' @note Requires the \pkg{modelsummary} and \pkg{fixest} packages.
#'
#' @examples
#' \dontrun{
#' library(fixest)
#' library(data.table)
#' data(mtcars)
#' m1 <- feols(mpg ~ hp, data = mtcars)
#' m2 <- feols(mpg ~ hp + wt, data = mtcars)
#' export_coef_table(list(m1, m2))
#' }
#'
#' @export
export_coef_table <- function(models, models_names = NULL) {
  if (!requireNamespace("modelsummary", quietly = TRUE)) {
    stop("Package 'modelsummary' is required for export_coef_table(). ",
         "Please install it with install.packages('modelsummary').")
  }

  tbl_ols <- modelsummary::msummary(
    models,
    output = "data.frame",
    stars = c("*" = .1, "**" = .05, "***" = 0.01),
    fmt = function(x) {
      ifelse(
        abs(x - round(x)) < 1e-6,
        formatC(x, format = "f", big.mark = ",", digits = 0),
        formatC(x, format = "f", big.mark = ",", digits = 3)
      )
    }
  )
  tbl_ols <- as.data.table(tbl_ols)

  # Remove variable name for Std error rows
  tbl_ols[statistic == "std.error" & part == "estimates", term := ""]

  # Keep selected rows
  tbl_ols <- tbl_ols[part == "estimates" | term %in% c("Num.Obs.", "R2", "R2 Within", "Std.Errors") | grepl("^FE:", term), !"statistic"]

  # Remove parenthesis (especially arising from I() function)
  tbl_ols[part == "estimates", term := gsub("\\bI\\(|\\)\\b", "", term)]

  # Copy original column order
  col_order <- copy(names(tbl_ols))

  # Add row with FE title
  fe_rows <- tbl_ols[grepl("^FE:", term)]
  tbl_ols <- tbl_ols[!grepl("^FE:", term)]
  fe_rows_title <- c("Fixed Effects")
  names(fe_rows_title) <- "part"
  fe_rows <- rbindlist(list(as.list(fe_rows_title), fe_rows), use.names = TRUE, fill = TRUE)
  fe_rows[, term := sub("^FE:", "", term)]

  # Add row with fit statistic title
  gof_rows <- tbl_ols[!part %in% c("estimates", "Model")]
  gof_rows_title <- c("Fit Statistics")
  names(gof_rows_title) <- "part"
  gof_rows <- rbindlist(list(as.list(gof_rows_title), gof_rows), use.names = TRUE, fill = TRUE)

  # Add row with estimate "title"
  est_rows <- tbl_ols[part == "estimates"]
  est_rows_title <- c("Variables")
  names(est_rows_title) <- "part"
  est_rows <- rbindlist(list(as.list(est_rows_title), est_rows), use.names = TRUE, fill = TRUE)

  # Finalize table
  tbl_ols_final <- rbindlist(list(est_rows, gof_rows, fe_rows), use.names = TRUE, fill = TRUE)
  setcolorder(tbl_ols_final, col_order)
  tbl_ols_final[part %in% c("estimates", "gof"), part := NA_character_]
  tbl_ols_final <- tbl_ols_final[, lapply(.SD, function(x) fifelse(is.na(x), "", x))]

  return(tbl_ols_final)
}
