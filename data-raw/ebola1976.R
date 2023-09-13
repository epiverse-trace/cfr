## code to prepare `ebola1976` dataset goes here

ebola1976 <- read.csv(
  system.file(
    "extdata", "ebola_1976.csv",
    package = "cfr",
    mustWork = TRUE
  )
)
ebola1976$date <- as.Date(ebola1976$date)

usethis::use_data(ebola1976, overwrite = TRUE)
