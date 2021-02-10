library(sf)
library(tidyverse)
rm(list = ls())
# co2cbsa --------------------------------------------------------------------


co2cbsa <- xwalks::ctx %>%
  select(-c(tractce, geoid,
            contains("cz"))) %>%
  filter(!is.na(cbsa)) %>%
    distinct() %>%
  arrange(cbsa)

co2cbsa$countyfp <- paste0(co2cbsa$statefp,
                         co2cbsa$countyfp)

co2cbsa


# write ------------------------------------------------------------------------

usethis::use_data(co2cbsa,
                  overwrite = T)
