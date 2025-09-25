## code to prepare `adhj` dataset goes here
adhj <- readxl::read_excel("data-raw/adhj.xlsx")

usethis::use_data(adhj, overwrite = TRUE)
