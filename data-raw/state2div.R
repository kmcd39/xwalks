## code to prepare `state2div` xwalk.

library(sf)
library(dplyr)
rm(list=ls())
# get all states + territories ------------------------------------------
states = tigris::states()
states$GEOID
tibble(states)
colnames(states)
# take format i want
states <- states %>%
  tibble() %>%
  select(1:3, STUSPS, NAME)
colnames(states) <- tolower(colnames(states))
colnames(states)[4] <- "abv"

states
states$statefp
# add div/region names
# Uses transcription from census bureau
# pdf here:
# https://github.com/cphalpert/census-regions/blob/master/us%20census%20bureau%20regions%20and%20divisions.csv
# transcription on khealy's github https://github.com/kjhealy
divtranscription <- read.csv("https://raw.githubusercontent.com/kjhealy/fips-codes/master/state_fips_master.csv")

divtranscription <- divtranscription %>%
  select(contains("region"), contains("division")) %>%
  mutate_all(as.character)  %>%
  distinct()
  #mutate(fips = stringr::str_pad(fips, 2, "left", "0"))
tibble(divtranscription)

state2div <- states %>%
  left_join(divtranscription
            , by=c("region", "division"))

state2div %>% count(division, division_name)
state2div %>% filter(statefp > 55)
state2div %>% filter(statefp == 11)

# name non-DC territories
state2div$region_name <- tidyr::replace_na(state2div$region_name, "territory")
state2div$division_name <- tidyr::replace_na(state2div$division_name, "territory")


# arrange -----------------------------------------------------------------
state2div <- state2div %>%
  arrange(statefp)

# write -------------------------------------------------------------------

usethis::use_data(state2div, overwrite = TRUE)
