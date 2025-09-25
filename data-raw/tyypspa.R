## code to prepare `tyypspa` dataset goes here
tyypspa <- haven::read_sas("data-raw/tyypspa.sas7bdat", NULL)

usethis::use_data(tyypspa, overwrite = TRUE)
