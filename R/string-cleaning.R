#' Clean Variable Names
#'
#' Standardizes variable names by trimming whitespace, converting to lowercase
#' ASCII, replacing all non-alphanumeric characters with underscores, and
#' removing leading/trailing underscores.
#'
#' @param text Character vector of variable names to clean.
#'
#' @returns Character vector of cleaned variable names containing only lowercase
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
#' @returns Character vector of cleaned firm names.
#'
#' @examples
#' clean_firm_name(c("Apple, Inc.", "MICROSOFT CORP.", "Alphabet LLC"))
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


# ============================================================================
# DIAGNOSTIC FUNCTIONS
# ============================================================================

#' Diagnose String Column Quality
#'
#' Audits a character vector for common data quality issues including missing
#' values, empty strings, whitespace problems, non-ASCII characters, and case
#' inconsistencies. Useful for understanding string data before cleaning.
#'
#' @param x Character vector to diagnose.
#' @param name Optional name for the variable (used in output). If NULL,
#'   attempts to capture the variable name from the call.
#'
#' @returns An S3 object of class `diagnose_strings` containing:
#' \describe{
#'   \item{name}{Name of the variable}
#'   \item{n_total}{Total number of elements}
#'   \item{n_na}{Count of NA values}
#'   \item{n_empty}{Count of empty strings ("")}
#'   \item{n_whitespace_only}{Count of strings containing only whitespace}
#'   \item{n_leading_ws}{Count of non-empty strings with leading whitespace}
#'   \item{n_trailing_ws}{Count of non-empty strings with trailing whitespace}
#'   \item{n_non_ascii}{Count of strings containing non-ASCII characters}
#'   \item{n_case_variants}{Number of unique values that have case variants}
#'   \item{case_variant_groups}{Number of groups of case-insensitive duplicates}
#'   \item{case_variant_examples}{data.table with examples of case variants}
#' }
#'
#' @seealso [audit_clean()] for auditing the effect of cleaning functions,
#'   [diagnose_nas()] for missing value diagnostics
#'
#' @examples
#' library(data.table)
#' firms <- c("Apple", "APPLE", "apple", "  Microsoft ", "Google", NA, "")
#' diagnose_strings(firms)
#'
#' @export
diagnose_strings <- function(x, name = NULL) {
  if (is.null(name)) {
    name <- deparse(substitute(x))
  }

  x <- as.character(x)
  n_total <- length(x)

  # NA values

  n_na <- sum(is.na(x))

  # Work with non-NA values for remaining checks
  non_na <- x[!is.na(x)]

  # Empty strings
  n_empty <- sum(non_na == "")

  # Whitespace-only strings
  n_whitespace_only <- sum(stri_detect_regex(non_na, "^\\s+$"))

  # Leading/trailing whitespace (excluding empty and whitespace-only)
  content_strings <- non_na[non_na != "" & !stri_detect_regex(non_na, "^\\s*$")]
  n_leading_ws <- sum(stri_detect_regex(content_strings, "^\\s"))
  n_trailing_ws <- sum(stri_detect_regex(content_strings, "\\s$"))


  # Non-ASCII characters
  n_non_ascii <- sum(stri_detect_regex(non_na, "[^\\x00-\\x7F]"))

  # Case variants: values that differ only by case
  # Group by lowercase version and find groups with multiple distinct original values
  if (length(content_strings) > 0L) {
    lower_versions <- stri_trans_tolower(content_strings)
    dt <- data.table(original = content_strings, lower = lower_versions)
    # Find groups where there are multiple distinct original values
    case_groups <- dt[, .(
      n_variants = uniqueN(original),
      examples = paste(unique(original)[1:min(3L, uniqueN(original))], collapse = ", ")
    ), keyby = lower]
    case_groups <- case_groups[n_variants > 1L]
    n_case_variant_groups <- nrow(case_groups)
    n_case_variants <- sum(case_groups$n_variants)
    case_variant_examples <- case_groups[, .(lower, n_variants, examples)]
  } else {
    n_case_variant_groups <- 0L
    n_case_variants <- 0L
    case_variant_examples <- data.table(
      lower = character(0),
      n_variants = integer(0),
      examples = character(0)
    )
  }

  out <- list(
    name = name,
    n_total = n_total,
    n_na = n_na,
    n_empty = n_empty,
    n_whitespace_only = n_whitespace_only,
    n_leading_ws = n_leading_ws,
    n_trailing_ws = n_trailing_ws,
    n_non_ascii = n_non_ascii,
    n_case_variants = n_case_variants,
    n_case_variant_groups = n_case_variant_groups,
    case_variant_examples = case_variant_examples
  )
  class(out) <- c("diagnose_strings", "list")
  out
}

#' Print Method for diagnose_strings Objects
#'
#' @param x A `diagnose_strings` object.
#' @param ... Additional arguments (ignored).
#'
#' @returns Invisibly returns the input object.
#'
#' @export
print.diagnose_strings <- function(x, ...) {
  fmt_int <- function(z) format(z, big.mark = ",", scientific = FALSE, trim = TRUE)
  fmt_pct <- function(n, total) {
    if (total == 0L) return("0.0%")
    sprintf("%.1f%%", 100 * n / total)
  }

  cat("\n=============== String Column Diagnosis ===============\n")
  cat("Variable: ", x$name, "\n", sep = "")
  cat("--------------------------------------------------------\n")

  cat("Total elements:        ", fmt_int(x$n_total), "\n", sep = "")
  cat("--------------------------------------------------------\n")
  cat("Missing & Empty:\n")
  cat("  NA values:           ", fmt_int(x$n_na),
      " (", fmt_pct(x$n_na, x$n_total), ")\n", sep = "")
  cat("  Empty strings:       ", fmt_int(x$n_empty),
      " (", fmt_pct(x$n_empty, x$n_total), ")\n", sep = "")
  cat("  Whitespace-only:     ", fmt_int(x$n_whitespace_only),
      " (", fmt_pct(x$n_whitespace_only, x$n_total), ")\n", sep = "")
  cat("--------------------------------------------------------\n")
  cat("Whitespace Issues:\n")
  cat("  Leading whitespace:  ", fmt_int(x$n_leading_ws), "\n", sep = "")
  cat("  Trailing whitespace: ", fmt_int(x$n_trailing_ws), "\n", sep = "")
  cat("--------------------------------------------------------\n")
  cat("Encoding:\n")
  cat("  Non-ASCII chars:     ", fmt_int(x$n_non_ascii), "\n", sep = "")
  cat("--------------------------------------------------------\n")
  cat("Case Inconsistencies:\n")
  cat("  Variant groups:      ", fmt_int(x$n_case_variant_groups), "\n", sep = "")
  cat("  Total variants:      ", fmt_int(x$n_case_variants), "\n", sep = "")

  if (x$n_case_variant_groups > 0L) {
    cat("\nCase variant examples (up to 5 groups):\n")
    print(head(x$case_variant_examples, 5L), row.names = FALSE)
  }

  invisible(x)
}


#' Audit String Cleaning Operation
#'
#' Applies a cleaning function to a character vector and reports what changed.
#' Provides transparency about the cleaning operation by showing counts and
#' before/after examples.
#'
#' @param x Character vector to clean.
#' @param clean_fn A function that takes a character vector and returns a
#'   cleaned character vector of the same length.
#' @param name Optional name for the variable (used in output). If NULL,
#'   attempts to capture the variable name from the call.
#'
#' @returns An S3 object of class `audit_clean` containing:
#' \describe{
#'   \item{name}{Name of the variable}
#'   \item{clean_fn_name}{Name of the cleaning function used}
#'   \item{n_total}{Total number of elements}
#'   \item{n_changed}{Count of values that changed}
#'   \item{n_unchanged}{Count of values that stayed the same}
#'   \item{n_na}{Count of NA values (unchanged by definition)}
#'   \item{pct_changed}{Percentage of non-NA values that changed}
#'   \item{change_examples}{data.table with sample before/after pairs}
#'   \item{cleaned}{The cleaned character vector}
#' }
#'
#' @seealso [diagnose_strings()] for string quality diagnostics,
#'   [clean_var_names()] and [clean_firm_name()] for built-in cleaning functions
#'
#' @examples
#' library(data.table)
#' firms <- c("Apple Inc.", "MICROSOFT CORP", "Alphabet LLC", NA)
#' result <- audit_clean(firms, clean_firm_name)
#' result$cleaned
#'
#' @export
audit_clean <- function(x, clean_fn, name = NULL) {
  if (is.null(name)) {
    name <- deparse(substitute(x))
  }
  clean_fn_name <- deparse(substitute(clean_fn))

  x <- as.character(x)
  n_total <- length(x)
  n_na <- sum(is.na(x))

  # Apply cleaning function
  cleaned <- clean_fn(x)

  # Compare original vs cleaned (NA == NA is considered unchanged)
  is_na <- is.na(x)
  # For non-NA values, check if they changed
  changed <- !is_na & (x != cleaned)
  # Handle case where cleaned might introduce NA
  changed[!is_na & is.na(cleaned)] <- TRUE

  n_changed <- sum(changed)
  n_unchanged <- n_total - n_changed

  # Percentage of non-NA values that changed
  n_non_na <- n_total - n_na
  pct_changed <- if (n_non_na > 0L) 100 * n_changed / n_non_na else 0

  # Collect examples of changes (up to 10)
  if (n_changed > 0L) {
    changed_idx <- which(changed)
    sample_idx <- changed_idx[seq_len(min(10L, length(changed_idx)))]
    change_examples <- data.table(
      before = x[sample_idx],
      after = cleaned[sample_idx]
    )
  } else {
    change_examples <- data.table(
      before = character(0),
      after = character(0)
    )
  }

  out <- list(
    name = name,
    clean_fn_name = clean_fn_name,
    n_total = n_total,
    n_changed = n_changed,
    n_unchanged = n_unchanged,
    n_na = n_na,
    pct_changed = pct_changed,
    change_examples = change_examples,
    cleaned = cleaned
  )
  class(out) <- c("audit_clean", "list")
  out
}

#' Print Method for audit_clean Objects
#'
#' @param x An `audit_clean` object.
#' @param ... Additional arguments (ignored).
#'
#' @returns Invisibly returns the input object.
#'
#' @export
print.audit_clean <- function(x, ...) {
  fmt_int <- function(z) format(z, big.mark = ",", scientific = FALSE, trim = TRUE)

  cat("\n=============== String Cleaning Audit ===============\n")
  cat("Variable: ", x$name, "\n", sep = "")
  cat("Function: ", x$clean_fn_name, "\n", sep = "")
  cat("-----------------------------------------------------\n")

  cat("Total elements:  ", fmt_int(x$n_total), "\n", sep = "")
  cat("  NA values:     ", fmt_int(x$n_na), "\n", sep = "")
  cat("  Changed:       ", fmt_int(x$n_changed),
      " (", sprintf("%.1f%%", x$pct_changed), " of non-NA)\n", sep = "")
  cat("  Unchanged:     ", fmt_int(x$n_unchanged), "\n", sep = "")

  if (x$n_changed > 0L) {
    n_examples <- nrow(x$change_examples)
    cat("-----------------------------------------------------\n")
    cat("Examples of changes (showing ", n_examples,
        " of ", fmt_int(x$n_changed), "):\n", sep = "")
    print(x$change_examples, row.names = FALSE)
  }

  cat("\nAccess cleaned vector with: result$cleaned\n")

  invisible(x)
}
