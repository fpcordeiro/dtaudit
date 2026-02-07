#' @keywords internal
"_PACKAGE"

#' @import data.table
#' @importFrom stringi stri_detect_regex stri_paste stri_replace_all_regex
#'   stri_split_fixed stri_trans_general stri_trans_nfkc stri_trans_tolower
#'   stri_trans_toupper stri_trim_both
#' @importFrom stats quantile sd
#' @importFrom utils head
NULL

# Suppress R CMD check notes for data.table NSE variables
utils::globalVariables(c(
  ".", ".N", ".SD", "..by.x", "..key_cols", "..num_cols", "..keys", "..vars",
  "examples", "lower", "n_na", "n_variants", "N", "N.x", "N.y", "ORIG_CJ__",
  "ORIG_DT__", "original", "row_names", "variable"
))
