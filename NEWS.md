# dtaudit 0.1.0

* Initial CRAN submission.
* Core diagnostic functions: `validate_join()`, `validate_primary_keys()`,
  `validate_var_relationship()`, `compare_datatables()`.
* Filter diagnostics: `filter_keep()`, `filter_drop()` with automatic
  reporting of dropped rows and values.
* Data quality tools: `diagnose_nas()`, `diagnose_strings()`, `audit_clean()`,
  `get_summary_table()`, `summarize_vector()`, `check_date_coverage()`.
* String cleaning utilities: `clean_var_names()`, `clean_firm_name()`.
* Data manipulation: `embed_into_cartesian()` for balanced panel creation.
