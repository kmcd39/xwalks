library(sf)
library(tidyverse)

state_list <- xwalks::state2div$statefp %>% unique()
place_list <-
  map(state_list,
      ~tigris::places(.,
                      year = 2019,
                      cb= T))

place_list <- place_list %>%
  map( ~tibble(.) ) %>%
  map( ~select(.,
               STATEFP,
               GEOID, NAME, LSAD)) %>%
  do.call("rbind", .)

# write
usethis::use_data(place_list )

