## code to prepare `adcrslb` dataset goes here
adcrslb <- readxl::read_excel("data-raw/adcrslb.xlsx")

usethis::use_data(adcrslb, overwrite = TRUE)
