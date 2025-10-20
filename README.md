
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `{BioStatsSuite}`

<!-- badges: start -->

<!-- badges: end -->

## Installation

You can install the development version of `{BioStatsSuite}` like so:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```

## Run

You can launch the application by running:

``` r
BioStatsSuite::run_app()
```

## About

You are reading the doc about version : 0.0.0.9000

This README has been compiled on the

``` r
Sys.time()
#> [1] "2025-09-25 13:46:43 CST"
```

Here are the tests results and package coverage:

``` r
devtools::check(quiet = TRUE)
#> ℹ Loading BioStatsSuite
#> ── R CMD check results ─────────────────────────── BioStatsSuite 0.0.0.9000 ────
#> Duration: 1m 33.2s
#> 
#> ❯ checking code files for non-ASCII characters ... WARNING
#>   Found the following files with non-ASCII characters:
#>     R/app_server.R
#>     R/app_ui.R
#>     R/mod_analyze.R
#>     R/mod_c_describe.R
#>     R/mod_c_srt.R
#>     R/mod_covancova.R
#>     R/mod_crosstable.R
#>     R/mod_dataUpload.R
#>     R/mod_data_filter.R
#>     R/mod_lifetest.R
#>     R/mod_q_describe.R
#>     R/mod_q_param.R
#>     R/utils_c_describe.R
#>     R/utils_c_srt.R
#>     R/utils_covancova.R
#>     R/utils_crosstable.R
#>     R/utils_data_reader.R
#>     R/utils_lifetest.R
#>     R/utils_q_describe.R
#>     R/utils_q_param.R
#>   Portable packages must use only ASCII characters in their R code and
#>   NAMESPACE directives, except perhaps in comments.
#>   Use \uxxxx escapes for other characters.
#>   Function 'tools::showNonASCIIfile' can help in finding non-ASCII
#>   characters in files.
#> 
#> ❯ checking for missing documentation entries ... WARNING
#>   Undocumented code objects:
#>     'adcrslb' 'adhj' 'adsl' 'adts' 'cov_adur' 'tyypspa'
#>   Undocumented data sets:
#>     'adcrslb' 'adhj' 'adsl' 'adts' 'cov_adur' 'tyypspa'
#>   All user-level objects in a package should have documentation entries.
#>   See chapter 'Writing R documentation files' in the 'Writing R
#>   Extensions' manual.
#> 
#> ❯ checking dependencies in R code ... NOTE
#>   Namespaces in Imports field not imported from:
#>     'shinydashboard' 'shinyjs'
#>     All declared Imports should be used.
#> 
#> ❯ checking R code for possible problems ... [18s] NOTE
#>   c_crosstable: no visible binding for global variable 'rowvarcd_'
#>   c_crosstable: no visible binding for global variable 'Freq'
#>   c_crosstable: no visible binding for global variable 'grpcd_'
#>   c_crosstable: no visible binding for global variable 'colvarcd_'
#>   c_crosstable: no visible binding for global variable 'np_total'
#>   c_describe: no visible binding for global variable 't.x'
#>   c_describe: no visible binding for global variable 't.y'
#>   c_describe: no visible binding for global variable 't.Freq'
#>   c_describe: no visible binding for global variable 'BREAK_'
#>   c_describe: no visible binding for global variable 'n999_'
#>   c_describe: no visible binding for global variable 'grpcd_'
#>   c_describe: no visible binding for global variable 'catorder_'
#>   c_describe: no visible binding for global variable 'np999_'
#>   c_describe: no visible binding for global variable 'grp_n'
#>   c_srt: no visible binding for global variable 't.x'
#>   c_srt: no visible binding for global variable 't.y'
#>   c_srt: no visible binding for global variable 't.Freq'
#>   c_srt: no visible binding for global variable 'grpcd_'
#>   c_srt: no visible binding for global variable 'catorder_'
#>   c_srt: no visible binding for global variable 'BREAK_'
#>   c_srt: no visible binding for global variable 'np999_'
#>   c_srt: no visible binding for global variable 'grp_n'
#>   covancova: no visible binding for global variable 'Sum.Sq'
#>   covancova: no visible binding for global variable 'Df'
#>   covancova: no visible binding for global variable 'F.value'
#>   covancova: no visible binding for global variable 'Pr..F.'
#>   covancova: no visible binding for global variable '指标'
#>   covancova: no visible binding for global variable '因素'
#>   covancova: no visible binding for global variable 'P值'
#>   covancova: no visible binding for global variable '治疗水平及差值'
#>   covancova: no visible binding for global variable 'LSMean'
#>   covancova: no visible binding for global variable '95% CIL'
#>   covancova: no visible binding for global variable '95% CIU'
#>   lifetest: no visible binding for global variable 'grpcd_'
#>   lifetest: no visible binding for global variable 'grp_n'
#>   lifetest: no visible binding for global variable 'fit.time'
#>   lifetest: no visible binding for global variable 'grp_cd'
#>   lifetest: no visible binding for global variable 'grp_name'
#>   lifetest: no visible binding for global variable 'surv'
#>   lifetest: no visible binding for global variable 'lower'
#>   lifetest: no visible binding for global variable 'upper'
#>   lifetest: no visible binding for global variable 'surv_0'
#>   q_describe: no visible binding for global variable 'group_0'
#>   q_describe: no visible binding for global variable 'grp_n'
#>   q_describe: no visible binding for global variable 'var_0'
#>   q_describe: no visible binding for global variable 'Q1'
#>   q_describe: no visible binding for global variable 'Q3'
#>   q_describe: no visible binding for global variable 'N_Missing'
#>   q_describe: no visible binding for global variable 'Mean_SD'
#>   q_describe: no visible binding for global variable 'Median_Q1_Q3'
#>   q_describe: no visible binding for global variable 'Min_Max'
#>   q_param: no visible binding for global variable 'group_0'
#>   q_param: no visible binding for global variable 'grp_n'
#>   q_param: no visible binding for global variable 'var_0'
#>   q_param: no visible binding for global variable 'Q1'
#>   q_param: no visible binding for global variable 'Q3'
#>   q_param: no visible binding for global variable 'N_Missing'
#>   q_param: no visible binding for global variable 'Mean_SD'
#>   q_param: no visible binding for global variable 'Median_Q1_Q3'
#>   q_param: no visible binding for global variable 'Min_Max'
#>   q_param: no visible binding for global variable 'grpcd_'
#>   read_data_file: no visible global function definition for 'read.csv'
#>   Undefined global functions or variables:
#>     95% CIL 95% CIU BREAK_ Df F.value Freq LSMean Mean_SD Median_Q1_Q3
#>     Min_Max N_Missing Pr..F. P值 Q1 Q3 Sum.Sq catorder_ colvarcd_
#>     fit.time group_0 grp_cd grp_n grp_name grpcd_ lower n999_ np999_
#>     np_total read.csv rowvarcd_ surv surv_0 t.Freq t.x t.y upper var_0
#>     因素 指标 治疗水平及差值
#>   Consider adding
#>     importFrom("utils", "read.csv")
#>   to your NAMESPACE file.
#> 
#> 0 errors ✔ | 2 warnings ✖ | 2 notes ✖
#> Error: R CMD check found WARNINGs
```

``` r
covr::package_coverage()
#> BioStatsSuite Coverage: 16.07%
#> R/app_config.R: 0.00%
#> R/app_server.R: 0.00%
#> R/app_ui.R: 0.00%
#> R/mod_analyze.R: 0.00%
#> R/mod_data_filter.R: 0.00%
#> R/mod_dataUpload.R: 0.00%
#> R/run_app.R: 0.00%
#> R/utils_c_describe.R: 0.00%
#> R/utils_c_srt.R: 0.00%
#> R/utils_covancova.R: 0.00%
#> R/utils_crosstable.R: 0.00%
#> R/utils_data_reader.R: 0.00%
#> R/utils_lifetest.R: 0.00%
#> R/utils_q_describe.R: 0.00%
#> R/utils_q_param.R: 0.00%
#> R/mod_q_param.R: 44.08%
#> R/mod_c_srt.R: 46.45%
#> R/mod_c_describe.R: 47.50%
#> R/mod_lifetest.R: 47.77%
#> R/mod_crosstable.R: 56.93%
#> R/mod_covancova.R: 57.59%
#> R/mod_q_describe.R: 57.80%
```
