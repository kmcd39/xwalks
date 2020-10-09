## code to prepare `state2div` xwalk.

# Uses transcription from census bureau
# pdf here:
# https://github.com/cphalpert/census-regions/blob/master/us%20census%20bureau%20regions%20and%20divisions.csv
# transcription on khealy's github https://github.com/kjhealy
library(dplyr)
state2div <- read.csv("https://raw.githubusercontent.com/kjhealy/fips-codes/master/state_fips_master.csv")

# take format i want
state2div <- state2div %>%
  select(1
         , abv = 2
         , statefp = 4
         , contains("region")
         , contains("division") )

# add Wash DC manually
state2div
state2div <- state2div %>%
  rbind(c("Washington DC", "DC", 11, 3,"South", 5, "South Atlantic"))

state2div <- state2div %>% arrange(as.numeric(statefp)) %>% tibble()
state2div

usethis::use_data(state2div, overwrite = TRUE)
