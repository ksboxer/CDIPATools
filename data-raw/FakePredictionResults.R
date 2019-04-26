library(readr)
FakePredictionResults <- readr::read_csv("data-raw/FakePredictionResults.csv")
usethis::use_data(FakePredictionResults)
