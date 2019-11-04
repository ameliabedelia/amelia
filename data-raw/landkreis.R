landkreis <- read_csv("data-raw/landkreise_sf.csv") %>%
     select(-order, -piece) %>%
     janitor::clean_names() %>%
     rename_all(tolower) %>%
     mutate(
          rs = parse_double(rs),
          bundesland = case_when(
               between(rs, 1000, 1999) ~ "Schleswig-Holstein",
               between(rs, 2000, 2999) ~ "Hamburg",
               between(rs, 3000, 3999) ~ "Niedersachen",
               between(rs, 4000, 4999) ~ "Bremen",
               between(rs, 5000, 5999) ~ "Nordrhein-Westfalen",
               between(rs, 6000, 6999) ~ "Hessen",
               between(rs, 7000, 7999) ~ "Rheinland-Pfalz",
               between(rs, 8000, 8999) ~ "Baden-Wuerttemberg",
               between(rs, 9000, 9999) ~ "Bayern",
               between(rs, 10000, 10999) ~ "Saarland",
               between(rs, 11000, 11999) ~ "Berlin",
               between(rs, 12000, 12999) ~ "Brandenburg",
               between(rs, 13000, 13999) ~ "Mecklenburg-Vorpommern",
               between(rs, 14000, 14999) ~ "Sachsen",
               between(rs, 15000, 15999) ~ "Sachsen-Anhalt",
               between(rs, 16000, 16999) ~ "Thueringen")
     )

usethis::use_data(landkreis, internal = FALSE)
