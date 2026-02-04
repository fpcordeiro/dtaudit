# dtaudit: `data.table` audit tools

Audit and diagnostic tools for data analysis workflows using `data.table`.

## Overview

dtaudit helps analysts validate data operations, compare datasets, and export results. It provides diagnostic output (receipts) at each step, making it easier to catch data issues early in your pipeline.

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
#> Keys: x[customer_id] <--> y[customer_id]
#>   Relationship                    : many-to-one
#>   Key(s) in x   [customer_id]     : (1 col)
#>   Key(s) in y   [customer_id]     : (1 col)
#>   Rows in x                       : 5
#>   Distinct key combos in x        : 4
#>   Rows in y                       : 3
#>   Distinct key combos in y        : 3
#>   Overlapping distinct key combos : 2
#>   Matched row pairs (cartesian)   : 3
#>   Match rate from x               : 60
#>   Match rate from y               : 66.66667
#>   Rows only in x (no match in y)  : 2
#>   Rows only in y (no match in x)  : 1
#> -----------------------
#> Duplicates: x=yes  y=no
```

### Filter with Diagnostics

Filter data while tracking what gets dropped—including optional statistics like revenue or counts.

```r
sales <- data.table(
  region = c("East", "West", "East", "West"),
  revenue = c(1000, 500, 2000, 300),
  valid = c(TRUE, FALSE, TRUE, TRUE)
)

# Filter and see what's dropped
clean_sales <- filter_keep(sales, valid == TRUE, stat = revenue)
#> Dropped 1 of 4 rows (25.00%).
#> Dropped 500 of 3,800 for revenue (13.16%).
```

### Compare Data Tables

Compare two data.tables by structure, keys, and numeric values.

```r
dt_v1 <- data.table(id = 1:3, value = c(10.0, 20.0, 30.0))
dt_v2 <- data.table(id = 1:3, value = c(10.1, 20.0, 30.5), extra = "new")

compare_datatables(dt_v1, dt_v2)
#> 1. Number of rows
#>    dt1: 3 rows
#>    dt2: 3 rows
#>    Difference (dt1 - dt2): 0
#>
#> 2. Column names
#>    Matching column names : 2
#>    Only in dt1      :0
#>    Only in dt2      :1 (extra)
#>    Type mismatches  : 0
#>
#> 3. Key columns used for matching
#>    Key columns: id (auto-detected)
#>    Distinct key combinations in dt1: 3
#>    Distinct key combinations in dt2: 3
#>    Matching key combinations      : 3
#>    Only in dt1                    : 0
#>    Only in dt2                    : 0
#>
#> 4. Numeric column discrepancies (absolute differences)
#>    Comparing numeric columns after merging on keys.
#>    Rows matched on keys: 3
#>    column     n   min   q25 median   q75   max
#>    <char> <int> <num> <num>  <num> <num> <num>
#> 1:  value     3     0  0.05    0.1   0.3   0.5
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
#> Checking dates between 2023-01-01 and 2023-12-31
#> There are 2 months missing. These are:
#> Jul-2023, Aug-2023
```

### String Cleaning

Standardize variable names and firm names for matching.

```r
clean_var_names(c("Sales Revenue", "cost-of-goods"))
#> [1] "SALES_REVENUE" "COST_OF_GOODS"

clean_firm_name(c("Apple, Inc.", "MICROSOFT CORP."))
#> [1] "APPLE"     "MICROSOFT"
```

### Cartesian Expansion

Expand data to all combinations of grouping variables (useful for balanced panels).

```r
dt <- data.table(
  year = c(2020, 2020, 2021),
  region = c("A", "B", "A"),
  value = c(10, 20, 30)
)

embed_into_cartesian(dt, c("year", "region"))
#>    year region value
#>    <num> <char> <num>
#> 1:  2020      A    10
#> 2:  2020      B    20
#> 3:  2021      A    30
#> 4:  2021      B    NA
```

### Regression Tables

Format fixest regression output for publication.

```r
library(fixest)

m1 <- feols(mpg ~ hp, data = mtcars)
m2 <- feols(mpg ~ hp + wt, data = mtcars)

clean_etable(m1, m2)
export_coef_table(list(m1, m2))
```

### Excel Export

Write results to Excel with sheet management.

```r
save_to_excel(results, "output.xlsx", "Summary")
```

## Function Reference

| Function | Purpose |
|----------|---------|
| `validate_join()` | Analyze joins before merging |
| `filter_keep()` | Filter with diagnostic output |
| `compare_datatables()` | Compare two data.tables |
| `get_summary_table()` | Generate column summaries |
| `check_months_coverage()` | Validate date coverage |
| `embed_into_cartesian()` | Expand to cartesian product |
| `clean_var_names()` | Standardize variable names |
| `clean_firm_name()` | Normalize firm names |
| `clean_etable()` | Clean fixest output |
| `export_coef_table()` | Publication-ready regression tables |
| `save_to_excel()` | Write to Excel |

## Dependencies

**Required:**
- [data.table](https://rdatatable.gitlab.io/data.table/)
- [stringi](https://stringi.gagolewski.com/)

**Optional:**
- [fixest](https://lrberge.github.io/fixest/) - for regression table functions
- [modelsummary](https://modelsummary.com/) - for `export_coef_table()`
- [openxlsx](https://ycphs.github.io/openxlsx/) - for Excel export
- [pbapply](https://github.com/psolymos/pbapply) - for progress bars

## License

GPL-3
