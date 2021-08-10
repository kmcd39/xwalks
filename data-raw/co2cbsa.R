#' bundles selected xwalks
#'
library(sf)
library(tidyverse)

# option setting
sf_use_s2(F)
options(tigris_use_cache = TRUE)
Sys.setenv("VROOM_SHOW_PROGRESS"="false")
# co2cbsa ----------------------------------------------------------------------

cbsas <- tigris::core_based_statistical_areas(year = 2019)
counties <- tigris::counties(year = 2019)

cbsas <- cbsas %>% select(cbsa = CBSAFP, cbsa_name = NAME, geometry)
counties <- counties %>% select(statefp = STATEFP, county = COUNTYFP, geometry)
co2cbsa <- xwalks::generate.coterminous.xwalk(counties, cbsas, keep.geometry = F)

co2cbsa$countyfp = with(co2cbsa,
                        paste0(statefp, county))


co2cbsa <- co2cbsa %>%
  filter(!is.na(cbsa)) %>%
  select(-county) %>%
  distinct() %>%
  select(statefp, countyfp, cbsa, cbsa_name) %>%
  arrange(cbsa)

co2cbsa

# write ------------------------------------------------------------------------

usethis::use_data(co2cbsa,
                  overwrite = T)
