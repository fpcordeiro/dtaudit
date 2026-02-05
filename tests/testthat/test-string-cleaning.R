test_that("clean_var_names converts to lowercase", {
  result <- clean_var_names("sales revenue")
  expect_equal(result, "sales_revenue")
})

test_that("clean_var_names removes non-alphanumerical", {
  result <- clean_var_names("Cost & Other $ Expenses")
  expect_equal(result, "cost_other_expenses")
})

test_that("clean_var_names handles dashes", {
  result <- clean_var_names("cost-of-goods")
  expect_equal(result, "cost_of_goods")
})

test_that("clean_var_names trims whitespace", {
  result <- clean_var_names("  margin  ")
  expect_equal(result, "margin")
})

test_that("clean_var_names removes excessive underscores", {
  result <- clean_var_names("sales___revenue")
  expect_equal(result, "sales_revenue")
})

test_that("clean_firm_name removes corporation suffixes", {
  expect_equal(clean_firm_name("Apple Inc."), "APPLE")
  expect_equal(clean_firm_name("Microsoft Corp"), "MICROSOFT")
  expect_equal(clean_firm_name("Alphabet LLC"), "ALPHABET")
})

test_that("clean_firm_name removes CORPORATION and INCORPORATED", {
  expect_equal(clean_firm_name("Test Corporation"), "TEST")
  expect_equal(clean_firm_name("Test Incorporated"), "TEST")
})

test_that("clean_firm_name removes common words", {
  expect_equal(clean_firm_name("Bank of America"), "BANK AMERICA")
  expect_equal(clean_firm_name("The Coca-Cola Company"), "COCA COLA")
})

test_that("clean_firm_name normalizes ampersands to AND", {
  expect_equal(clean_firm_name("Johnson & Johnson"), "JOHNSON AND JOHNSON")
  expect_equal(clean_firm_name("AT&T Inc."), "AT AND T")
})

test_that("clean_firm_name normalizes whitespace", {
  result <- clean_firm_name("  Apple   Inc.  ")
  expect_equal(result, "APPLE")
})

# Tests for diagnose_strings()

test_that("diagnose_strings returns correct class", {
  result <- diagnose_strings(c("test", "value"))
  expect_s3_class(result, "diagnose_strings")
})

test_that("diagnose_strings counts NA values", {
  result <- diagnose_strings(c("a", NA, "b", NA, NA))
  expect_equal(result$n_total, 5L)
  expect_equal(result$n_na, 3L)
})

test_that("diagnose_strings counts empty strings", {
  result <- diagnose_strings(c("a", "", "b", ""))
  expect_equal(result$n_empty, 2L)
})

test_that("diagnose_strings counts whitespace-only strings", {
  result <- diagnose_strings(c("a", "   ", "\t", "b"))
  expect_equal(result$n_whitespace_only, 2L)
})

test_that("diagnose_strings detects leading/trailing whitespace", {
  result <- diagnose_strings(c("  leading", "trailing  ", "  both  ", "clean"))
  expect_equal(result$n_leading_ws, 2L)  # "  leading" and "  both  "
  expect_equal(result$n_trailing_ws, 2L)  # "trailing  " and "  both  "
})

test_that("diagnose_strings detects non-ASCII characters", {
  result <- diagnose_strings(c("cafe", "caf\u00e9", "r\u00e9sum\u00e9", "normal"))
  expect_equal(result$n_non_ascii, 2L)  # café and résumé
})

test_that("diagnose_strings detects case variants", {
  result <- diagnose_strings(c("Apple", "APPLE", "apple", "Google", "GOOGLE"))
  expect_equal(result$n_case_variant_groups, 2L)  # apple and google groups
  expect_equal(result$n_case_variants, 5L)  # total variants
})

test_that("diagnose_strings accepts custom name", {
  result <- diagnose_strings(c("a", "b"), name = "firm_names")
  expect_equal(result$name, "firm_names")
})

test_that("diagnose_strings print method returns invisibly", {
  result <- diagnose_strings(c("a", "b"))
  expect_invisible(print(result))
})

# Tests for audit_clean()

test_that("audit_clean returns correct class", {
  result <- audit_clean(c("test", "value"), toupper)
  expect_s3_class(result, "audit_clean")
})

test_that("audit_clean counts changed values", {
  result <- audit_clean(c("apple", "BANANA", "cherry"), toupper)
  expect_equal(result$n_changed, 2L)  # apple and cherry change
  expect_equal(result$n_unchanged, 1L)  # BANANA stays same
})

test_that("audit_clean handles NA values correctly", {
  result <- audit_clean(c("apple", NA, "cherry"), toupper)
  expect_equal(result$n_na, 1L)
  expect_equal(result$n_changed, 2L)
  expect_true(is.na(result$cleaned[2]))
})

test_that("audit_clean provides change examples", {
  result <- audit_clean(c("Apple Inc.", "Test Corp"), clean_firm_name)
  expect_true(nrow(result$change_examples) > 0L)
  expect_true("before" %in% names(result$change_examples))
  expect_true("after" %in% names(result$change_examples))
})

test_that("audit_clean returns cleaned vector", {
  input <- c("Apple Inc.", "Microsoft Corp")
  result <- audit_clean(input, clean_firm_name)
  expect_equal(result$cleaned, c("APPLE", "MICROSOFT"))
})

test_that("audit_clean calculates percentage correctly", {
  # 2 out of 3 non-NA values change
  result <- audit_clean(c("apple", "BANANA", "cherry", NA), toupper)
  expect_equal(result$pct_changed, 100 * 2 / 3)
})

test_that("audit_clean accepts custom name", {
  result <- audit_clean(c("a", "b"), toupper, name = "test_var")
  expect_equal(result$name, "test_var")
})

test_that("audit_clean print method returns invisibly", {
  result <- audit_clean(c("a", "b"), toupper)
  expect_invisible(print(result))
})
