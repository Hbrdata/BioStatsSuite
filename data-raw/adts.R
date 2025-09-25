## code to prepare `adts` dataset goes here
adts <- readxl::read_excel("data-raw/adts.xlsx")

usethis::use_data(adts, overwrite = TRUE)
