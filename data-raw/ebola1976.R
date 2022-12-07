## code to prepare `ebola1976` dataset goes here

ebola1976 <- data.table::fread(
  system.file(
    "extdata", "ebola_1976.csv",
    package = "datadelay",
    mustWork = TRUE
  )
)
ebola1976$date <- data.table::as.IDate(ebola1976$date)

usethis::use_data(ebola1976, overwrite = TRUE)
