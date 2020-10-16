## code to prepare `state2div` xwalk.


library(dplyr)
# get all states + territories ------------------------------------------
states = tigris::states()
states$GEOID
tibble(states)
colnames(states)
# take format i want
state2div <- states %>%
  tibble() %>%
  select(1:3, STUSPS, NAME)
colnames(state2div) <- tolower(colnames(state2div))
colnames(state2div)[4] <- "abv"

# add div/region names
# Uses transcription from census bureau
# pdf here:
# https://github.com/cphalpert/census-regions/blob/master/us%20census%20bureau%20regions%20and%20divisions.csv
# transcription on khealy's github https://github.com/kjhealy
divtranscription <- read.csv("https://raw.githubusercontent.com/kjhealy/fips-codes/master/state_fips_master.csv")
divtranscription <- divtranscription %>%
  select(contains("region"), contains("division")) %>%
  mutate_all(as.character)
tibble(divtranscription)

state2div <- state2div %>%
  left_join(divtranscription)

state2div %>% count(division, division_name)
state2div %>% filter(statefp > 55)
state2div %>% filter(statefp == 11)

# name non-DC territories
state2div$region_name <- tidyr::replace_na(state2div$region_name, "territory")
state2div$division_name <- tidyr::replace_na(state2div$division_name, "territory")

# write -------------------------------------------------------------------

usethis::use_data(state2div, overwrite = TRUE)
