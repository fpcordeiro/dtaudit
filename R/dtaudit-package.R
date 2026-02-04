#' @keywords internal
"_PACKAGE"

#' @import data.table
#' @importFrom stringi stri_extract_first_regex stri_trans_general stri_trans_toupper
#'   stri_replace_all_regex stri_sub stri_trim_both stri_replace_last_regex
#'   stri_replace_all_fixed stri_extract_last_regex
#' @importFrom stats quantile sd
#' @importFrom utils modifyList
NULL

# Suppress R CMD check notes for data.table NSE variables
utils::globalVariables(c(

  ".", ".N", ".SD", "..by.x", "..key_cols", "..num_cols",
  "N", "N.x", "N.y", "ORIG_CJ__", "ORIG_DT__",
  "part", "statistic", "term", "row_names", "variable"
))
