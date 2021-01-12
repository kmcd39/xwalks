library(sf)
library(tidyverse)

# co2cbsa --------------------------------------------------------------------


tmp <- xwalks::ctx %>%
  select(-c(tractce, geoid)) %>%
  distinct() %>%
  filter(!is.na(cbsa)) %>%
  arrange(cbsa)

# confirmign cbsas coterminous w counties
tmp %>%
  count(statefp, countyfp) %>%
  arrange(desc(n))

tmp
