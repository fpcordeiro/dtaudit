#' Extract Initials from Text
#'
#' Extracts the first word from text, converts to uppercase ASCII, removes
#' special characters, and truncates to a maximum length.
#'
#' @param text Character vector of text strings.
#' @param max_length Maximum length of the returned initials (default 7).
#'
#' @return Character vector of extracted initials.
#'
#' @examples
#' extract_initials_charclass(c("Apple Inc.", "Microsoft Corp"))
#'
#' @export
extract_initials_charclass <- function(text, max_length = 7) {
  # TODO: NEEDS TO BE COMPLETELY REWRITTEN
  # Extract up to first whitespace
  initial_part <- stri_extract_first_regex(text, "^[^ ]+")

  # Remove special characters while keeping alphanumeric and convert to uppercase
  clean_text <- stri_trans_general(initial_part, "ascii")
  clean_text <- stri_trans_toupper(clean_text)
  clean_text <- stri_replace_all_regex(clean_text, "[^A-Z0-9]", "")

  # Truncate to max_length
  stri_sub(clean_text, 1, max_length)
}

#' Clean Variable Names
#'
#' Standardizes variable names by trimming whitespace, converting to lowercase
#' ASCII, replacing all non-alphanumeric characters with underscores, and
#' removing leading/trailing underscores.
#'
#' @param text Character vector of variable names to clean.
#'
#' @return Character vector of cleaned variable names containing only lowercase
#'   letters, digits, and underscores.
#'
#' @examples
#' clean_var_names(c("Sales Revenue", "cost-of-goods", " margin "))
#' # Returns: c("sales_revenue", "cost_of_goods", "margin")
#'
#' @export
clean_var_names <- function(text) {
  text <- stri_trans_general(text, "ascii; lower")        # Convert to ASCII lowercase
  text <- stri_replace_all_regex(text, "[^a-z0-9]", "_")  # Non-alphanumeric -> underscore
  text <- stri_replace_all_regex(text, "_+", "_")         # Collapse multiple underscores
  text <- stri_replace_all_regex(text, "^_+|_+$", "")     # Trim leading/trailing underscores
  text
}

#' Clean Firm Names for Matching
#'
#' Normalizes firm names by converting to uppercase ASCII, removing common
#' suffixes (Corp, LLC, Inc, etc.), and standardizing whitespace. Useful for
#' fuzzy matching or deduplication of company names.
#'
#' @param text Character vector of firm names to clean.
#'
#' @return Character vector of cleaned firm names.
#'
#' @examples
#' clean_firm_name(c("Apple, Inc.", "MICROSOFT CORP.", "Alphabet LLC"))
#'
#' @importFrom stringi stri_trans_nfkc stri_split_fixed stri_paste
#'
#' @export
clean_firm_name <- function(text) {
  x <- as.character(text)
  na_idx <- is.na(x)
  x[na_idx] <- ""

  # Unicode normalization + ASCII transliteration + uppercase
  x <- stri_trans_nfkc(x)
  x <- stri_trans_general(x, "Any-Latin; Latin-ASCII")
  x <- stri_trans_toupper(x)

  # Normalize Unicode punctuation (dashes, ellipses, slashes)
  x <- stri_replace_all_regex(x, "[\\u2026\\u22EF]", " ")
  x <- stri_replace_all_regex(x, "\\p{Pd}+", " ")
  x <- stri_replace_all_regex(x, "[/\\\\]+", " ")

  # Normalize ampersand to AND
  x <- stri_replace_all_regex(x, "\\s*&\\s*", " AND ")


  # Remove apostrophes, punctuation (using Unicode escapes for non-ASCII)
  x <- stri_replace_all_regex(x, "[\u2018\u2019`\u00b4\\.,;]", "")

  # Replace remaining symbols to spaces
  x <- stri_replace_all_regex(x, "[^A-Z0-9 ]+", " ")

  # Collapse whitespace
  x <- stri_trim_both(stri_replace_all_regex(x, "\\s+", " "))

  stop_words <- c("OF", "THE")
  corp_designations <- c(
    "CO", "COMPANY", "CORP", "CORPORATION", "INC", "INCORPORATED",
    "LTD", "LIMITED", "LLC", "LLP", "LP", "PLLC", "LLLP", "PLC", "PC", "PA",
    "GMBH", "BV", "SA", "SAS"
  )

  # Remove stop words and corporate designations
  tokens <- stri_split_fixed(x, " ", omit_empty = TRUE)
  cleaned <- vapply(tokens, function(tok) {
    if (!length(tok)) return("")
    tok <- tok[!tok %in% stop_words]
    tok <- tok[!tok %in% corp_designations]
    if (!length(tok)) return("")
    stri_paste(tok, collapse = " ")
  }, FUN.VALUE = character(1))

  cleaned[na_idx] <- NA_character_
  cleaned
}
