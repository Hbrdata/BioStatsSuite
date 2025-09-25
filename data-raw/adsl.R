## code to prepare `adsl` dataset goes here
adsl <- readxl::read_excel("data-raw/adsl.xlsx")

usethis::use_data(adsl, overwrite = TRUE)
