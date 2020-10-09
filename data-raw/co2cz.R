## code to prepare `co2cz` dataset here
# adapts county xwalk from https://github.com/walkerke/us-boundaries/blob/master/cz_1990_v2_sp.R
library(dplyr)
rm(list = ls())

# FIPS codes for counties deleted since 1990 census
counties_deleted <- c(
  "02201", "02231", "02270", "02280", "12025", "30113", "46113", "51515",
  "51560", "51780"
)
# FIPS codes for counties added since 1990 census with 1990 commuting zone
counties_added <- tribble(
  ~fips_county, ~cz_1990,
  "02068", "34115",
  "02105", "34109",
  "02158", "34112",
  "02195", "34110",
  "02198", "34111",
  "02230", "34109",
  "02275", "34111",
  "02282", "34109",
  "08014", "28900",
  "12086", "07000",
  "46102", "27704"
)
# URL for commuting zone county partition using 1990 counties
url_cz <- "https://www.ers.usda.gov/webdocs/DataFiles/48457/czlma903.xls?v=68.4"
### older link from walkerke code: "https://www.ers.usda.gov/webdocs/DataFiles/Commuting_Zones_and_Labor_Market_Areas__17970/czlma903.xls"
cz_loc <- "~/R/dblinkr/.tmp/czlma903.csv" #.xls
# Read commuting zone county partition, add place and state variables
library(stringr)
co2cz <-
  read.csv(cz_loc) %>%
  tibble() %>%
  select(
    fips_county = contains("FIPS"),
    cz_1990 = CZ90,
    place_state = contains("largest.place")
  ) %>%
  mutate_at(c(1,2),
            ~stringr::str_pad(., 5, side= "left", "0")) %>%
  mutate(
    place =
      place_state %>%
      str_replace(" borough.*| CDP.*| city.*| town.*| \\(rem.*|,.*", ""),
    state = place_state %>% str_sub(start = -2L)
  ) %>%
  select(-place_state) %>%
  rename( cz_name = place)
co2cz
# Adjust county partition for counties added and deleted since 1990
v <-
  counties_added %>%
  left_join(co2cz %>% select(-fips_county) %>% distinct(), by = "cz_1990")
co2cz <-
  bind_rows(co2cz, v) %>%
  filter(!(fips_county %in% counties_deleted))
rm(v)

# end adaptation from walkerke code ---------------------------------------
co2cz <- co2cz %>%
  select( countyfp = 1
         ,cz = 2
         ,cz_name = 3
         ,4)

co2cz

usethis::use_data(co2cz, overwrite = TRUE)
