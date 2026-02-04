#' Save Data to Excel Workbook
#'
#' Writes a data.frame or data.table to an Excel worksheet. Creates a new
#' workbook if the file doesn't exist, or adds/replaces a sheet in an
#' existing workbook.
#'
#' @param table A data.frame or data.table to write.
#' @param path_out Path to the output Excel file (.xlsx).
#' @param sheet Name of the worksheet to create or replace.
#' @param na.string Optional string to represent NA values. If NULL (default),
#'   NA cells are left blank.
#'
#' @return Called for side effects (writes file). Returns NULL invisibly.
#'
#' @note Requires the \pkg{openxlsx} package.
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' dt <- data.table(x = 1:3, y = letters[1:3])
#' save_to_excel(dt, "output.xlsx", "Sheet1")
#' }
#'
#' @export
save_to_excel <- function(table, path_out, sheet, na.string = NULL) {
  # TODO: switch to openxlsx2. (Is it more stable now?)
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required for save_to_excel(). ",
         "Please install it with install.packages('openxlsx').")
  }

  # Get the directory part of the path
  out_dir <- dirname(path_out)

  # Halt if that directory doesn't exist
  if (!dir.exists(out_dir)) {
    stop(sprintf("Directory '%s' does not exist. Please create it before proceeding.", out_dir))
  }

  # Load or create workbook
  if (file.exists(path_out)) {
    wb_out <- openxlsx::loadWorkbook(path_out)
  } else {
    wb_out <- openxlsx::createWorkbook()
  }

  # If sheet exists, remove it so we can overwrite cleanly
  if (sheet %in% names(wb_out)) {
    openxlsx::removeWorksheet(wb_out, sheet)
  }

  # Now add a fresh sheet
  openxlsx::addWorksheet(wb_out, sheetName = sheet)

  # Write table to sheet
  if (is.null(na.string)) {
    openxlsx::writeData(wb_out, sheet = sheet, x = table, startRow = 1, startCol = 1)
  } else {
    openxlsx::writeData(wb_out, sheet = sheet, x = table, startRow = 1, startCol = 1,
                        na.string = na.string, keepNA = TRUE)
  }

  # Save workbook
  openxlsx::saveWorkbook(wb_out, path_out, overwrite = TRUE)
  invisible(NULL)
}
