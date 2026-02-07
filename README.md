# dtaudit

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

Audit and Diagnostic Tools for `data.table` Workflows

## Overview

`dtaudit` helps analysts validate data operations, compare datasets, and diagnose data quality issues. It provides diagnostic output (receipts) at each step, making it easier to catch data issues early in your pipeline.

## Installation

```r
# Using `remotes`
remotes::install_github("fpcordeiro/dtaudit")

# Or using `pak`
pak::pkg_install("fpcordeiro/dtaudit")
```

## Features

### Join Validation

Analyze potential joins before performing them. Understand relationship types, match rates, and identify unmatched keys.

```r
library(dtaudit)
library(data.table)

orders <- data.table(
  order_id = 1:5,
  customer_id = c(101, 102, 101, 103, 104)
)

customers <- data.table(
  customer_id = c(101, 102, 105),
  name = c("Alice", "Bob", "Charlie")
)

result <- validate_join(orders, customers, by = "customer_id")
print(result)
#> ============== Join Validation Summary ==============
#> Tables: orders <--> customers
#> Keys in orders: customer_id
#> Keys in customers: customer_id
#>   Relationship                                : many-to-one
#>   Key(s) in orders   [customer_id]            : (1 col)
#>   Key(s) in customers   [customer_id]         : (1 col)
#>   Rows in orders                              : 5
#>   Distinct key combos in orders               : 4
#>   Rows in customers                           : 3
#>   Distinct key combos in customers            : 3
#>   Overlapping distinct key combos             : 2
#>   Matched row pairs (cartesian)               : 3
#>   Match rate from orders                      : 60
#>   Match rate from customers                   : 66.66667
#>   Rows only in orders (no match in customers) : 2
#>   Rows only in customers (no match in orders) : 1
#> ------------------------------------
#> Duplicates: orders=yes  customers=no
```

### Primary Key Validation

Test whether a set of columns uniquely identifies every row in a data.table.

```r
employees <- data.table(
  dept = c("Sales", "Sales", "Engineering", "Engineering"),
  emp_id = c(1, 2, 1, 1),
  name = c("Alice", "Bob", "Charlie", "Diana")
)

validate_primary_keys(employees, c("dept", "emp_id"))
#> ============== Primary Key Validation ==============
#> Table: employees
#> Key column(s): dept, emp_id
#> -----------------------------------------------------
#>   Total rows:              4
#>   Unique key combinations: 3
#>   Duplicate key combos:    1
#> -----------------------------------------------------
#> Result: NO - Keys do NOT uniquely identify all rows.
#>
#> Duplicate keys (showing up to 10):
#> Key: <dept, emp_id>
#>           dept emp_id     N
#>         <char>  <num> <int>
#> 1: Engineering      1     2
```

### Variable Relationship Validation

Determine the relationship type between two variables in a data.table.

```r
dt <- data.table(
  student = c("Alice", "Alice", "Bob", "Bob"),
  course = c("Math", "English", "Math", "Science")
)

validate_var_relationship(dt, "student", "course")
#> ============== Variable Relationship Validation ==============
#> Table: dt
#> Variables: student <--> course
#> --------------------------------------------------------------
#>   Unique values in student: 2
#>   Unique values in course: 3
#>   Unique (student, course) pairs: 4
#> --------------------------------------------------------------
#>   student -> course: one-to-many
#>   course -> student: one-to-many
#> --------------------------------------------------------------
#> Relationship: MANY-TO-MANY
```

### Filter with Diagnostics

Filter data while tracking what gets dropped---including optional statistics like revenue or counts.

```r
sales <- data.table(
  region = c("East", "West", "East", "West"),
  revenue = c(1000, 500, 2000, 300),
  valid = c(TRUE, FALSE, TRUE, TRUE)
)

# Keep rows where valid == TRUE
clean_sales <- filter_keep(sales, valid == TRUE, stat = revenue)
#> filter_keep(sales, valid == TRUE)
#>   Dropped 1 of 4 rows (25.00%).
#>   Dropped 500 of 3,800 for revenue (13.16%).

# Or equivalently, drop rows where valid == FALSE
clean_sales <- filter_drop(sales, valid == FALSE, stat = revenue)
#> filter_drop(sales, valid == FALSE)
#>   Dropped 1 of 4 rows (25.00%).
#>   Dropped 500 of 3,800 for revenue (13.16%).
```

### Compare Data Tables

Compare two data.tables by structure, keys, and numeric values.

```r
dt_v1 <- data.table(id = 1:3, value = c(10.0, 20.0, 30.0))
dt_v2 <- data.table(id = 1:3, value = c(10.1, 20.0, 30.5), extra = "new")

compare_datatables(dt_v1, dt_v2)
#> 1. Number of rows
#>    dt_v1: 3 rows
#>    dt_v2: 3 rows
#>    Difference (dt_v1 - dt_v2): 0
#>
#> 2. Column names
#>    Matching column names : 2
#>    Only in dt_v1: 0
#>    Only in dt_v2: 1 (extra)
#>    Type mismatches  : 0
#>
#> 3. Key columns used for matching
#>    Key columns: id (auto-detected)
#>    Distinct key combinations in dt_v1: 3
#>    Distinct key combinations in dt_v2: 3
#>    Matching key combinations: 3
#>    Only in dt_v1: 0
#>    Only in dt_v2: 0
#>
#> 4. Numeric column discrepancies (absolute differences)
#>    Comparing numeric columns after merging on keys.
#>    Rows matched on keys: 3
#>    column     n   min   q25 median   q75   max
#>    <char> <int> <num> <num>  <num> <num> <num>
#> 1:  value     3     0  0.05    0.1   0.3   0.5
```

### Diagnose Missing Values

Report NA counts and percentages for each column.

```r
dt <- data.table(
  id = 1:6,
  name = c("Alice", NA, "Charlie", "Diana", NA, "Frank"),
  score = c(85, 90, NA, NA, NA, 70),
  grade = c("A", "A", "B", NA, NA, "C")
)

diagnose_nas(dt)
#> 3 of 4 columns have missing values
#>   variable                           n_na   pct_na
#>   score                                 3    50.0%
#>   name                                  2    33.3%
#>   grade                                 2    33.3%
```

### Diagnose String Columns

Audit character vectors for data quality issues: NAs, empty strings, whitespace problems, non-ASCII characters, and case inconsistencies.

```r
x <- c("Apple", "apple", "APPLE", "  banana  ", "", NA, "caf\u00e9", "na\u00efve")

diagnose_strings(x, name = "fruits")
#> =============== String Column Diagnosis ===============
#> Variable: fruits
#> --------------------------------------------------------
#> Total elements:        8
#> --------------------------------------------------------
#> Missing & Empty:
#>   NA values:           1 (12.5%)
#>   Empty strings:       1 (12.5%)
#>   Whitespace-only:     0 (0.0%)
#> --------------------------------------------------------
#> Whitespace Issues:
#>   Leading whitespace:  1
#>   Trailing whitespace: 1
#> --------------------------------------------------------
#> Encoding:
#>   Non-ASCII chars:     2
#> --------------------------------------------------------
#> Case Inconsistencies:
#>   Variant groups:      1
#>   Total variants:      3
#>
#> Case variant examples (up to 5 groups):
#> Key: <lower>
#>   lower n_variants            examples
#>  <char>      <int>              <char>
#>   apple          3 Apple, apple, APPLE
```

### Audit Cleaning Operations

Apply a cleaning function and see what changed---before committing to the transformation.

```r
x <- c("Apple, Inc.", "MICROSOFT CORP.", "google llc", "Amazon.com Inc")

result <- audit_clean(x, clean_firm_name, name = "company")
#> =============== String Cleaning Audit ===============
#> Variable: company
#> Function: clean_firm_name
#> -----------------------------------------------------
#> Total elements:  4
#>   NA values:     0
#>   Changed:       4 (100.0% of non-NA)
#>   Unchanged:     0
#> -----------------------------------------------------
#> Examples of changes (showing 4 of 4):
#>           before     after
#>           <char>    <char>
#>      Apple, Inc.     APPLE
#>  MICROSOFT CORP. MICROSOFT
#>       google llc    GOOGLE
#>   Amazon.com Inc AMAZONCOM
#>
#> Access cleaned vector with: result$cleaned
```

### Data Summaries

Generate comprehensive column summaries with type detection, missing value counts, and descriptive statistics.

```r
summary_table <- get_summary_table(my_data)
```

Check date coverage for time series data:

```r
dates <- as.IDate(c("2023-01-15", "2023-02-20", "2023-03-10", "2023-04-05",
                    "2023-05-12", "2023-06-18", "2023-09-22", "2023-10-30",
                    "2023-11-14", "2023-12-25"))
check_months_coverage(dates, "2023-01-01", "2023-12-31")
#> Checking dates between 2023-01-01 and 2023-12-31 (by month )
#> There are 2 month periods missing. These are:
#> Jul-2023, Aug-2023
```

### String Cleaning

Standardize variable names and firm names for matching.

```r
clean_var_names(c("Sales Revenue", "cost-of-goods"))
#> [1] "sales_revenue" "cost_of_goods"

clean_firm_name(c("Apple, Inc.", "MICROSOFT CORP."))
#> [1] "APPLE"     "MICROSOFT"
```

## Function Reference

| Function | Purpose |
|----------|---------|
| `validate_join()` | Analyze joins before merging |
| `validate_primary_keys()` | Test if columns uniquely identify rows |
| `validate_var_relationship()` | Determine relationship between two variables |
| `filter_keep()` | Filter with diagnostic output |
| `filter_drop()` | Drop rows with diagnostic output |
| `compare_datatables()` | Compare two data.tables |
| `diagnose_nas()` | Report missing values by column |
| `diagnose_strings()` | Audit string columns for quality issues |
| `audit_clean()` | Audit a cleaning function's effect |
| `get_summary_table()` | Generate column summaries |
| `check_date_coverage()` | Validate date coverage (flexible granularity) |
| `check_months_coverage()` | Validate monthly date coverage |
| `embed_into_cartesian()` | Expand to cartesian product |
| `clean_var_names()` | Standardize variable names |
| `clean_firm_name()` | Normalize firm names |

## Dependencies

**Required:**
- [data.table](https://rdatatable.gitlab.io/data.table/)
- [stringi](https://stringi.gagolewski.com/)

**Optional:**
- [pbapply](https://github.com/psolymos/pbapply) - for progress bars

## License

LGPL (>= 3)
