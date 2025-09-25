## code to prepare `cov_adur` dataset goes here
cov_adur <- readxl::read_excel("data-raw/cov_adur.xlsx")

usethis::use_data(cov_adur, overwrite = TRUE)
