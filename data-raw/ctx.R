library(lwgeom)
library(tidyverse)
library(sf)
rm(list = ls())

# state list
stateL <- xwalks::state2div %>% pull(statefp)

# generate by using tract IDs to get counties and then matching those to co2cz and
# co2cbsa
cts <- purrr::map_dfr( stateL
                       , ~tigris::tracts( state = .
                                          , cb = F
                                          , year = 2015 )
                       )

backup <- cts

cts <- tibble(cts)
colnames(cts) <- tolower(colnames(cts))
cts <- cts %>% select(1,2, geoid)
cts$countyfp <- with(cts, paste0(statefp, countyfp))

co2cz <- xwalks::co2cz %>% select(-statefp)
co2cbsa <- xwalks::co2cbsa %>% select(-statefp)


ctx <-
  list(cts, co2cz, co2cbsa) %>%
  purrr::reduce(left_join)

ctx

# write -------------------------------------------------------------------

usethis::use_data(ctx
                  ,overwrite = T)
