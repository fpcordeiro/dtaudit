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

test_that("extract_initials_charclass extracts first word", {
  result <- extract_initials_charclass("Apple Inc.")
  expect_equal(result, "APPLE")
})

test_that("extract_initials_charclass truncates to max_length", {
  result <- extract_initials_charclass("Microsoft Corporation", max_length = 5)
  expect_equal(result, "MICRO")
})

test_that("extract_initials_charclass removes special characters", {
  result <- extract_initials_charclass("AT&T Services")
  expect_equal(result, "ATT")
})

test_that("extract_initials_charclass converts to uppercase", {
  result <- extract_initials_charclass("apple")
  expect_equal(result, "APPLE")
})
