## code to prepare `ebola1976` dataset
# An example epidemic outbreak dataset for use with the `cfr` package.
# This dataset comes from the first Ebola outbreak in Zaire in 1976 as analysed
# in Camacho et al. (2014).
#
# Citation:
# Camacho, A., Kucharski, A. J., Funk, S., Breman, J., Piot, P., &
# Edmunds, W. J. (2014). Potential for large outbreaks of Ebola virus disease.
# Epidemics, 9, 70–78. \doi{10.1016/j.epidem.2014.09.003}
ebola1976 <- read.csv(
  system.file(
    "extdata", "ebola_1976.csv",
    package = "cfr",
    mustWork = TRUE
  )
)
ebola1976$date <- as.Date(ebola1976$date)

usethis::use_data(ebola1976, overwrite = TRUE)
