bundesland <- readr::read_csv("data-raw/bundesland.csv")

usethis::use_data(bundesland, internal = FALSE)
