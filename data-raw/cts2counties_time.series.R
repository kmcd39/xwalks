library(sf)
library(dplyr)

# https://www.census.gov/programs-surveys/geography/technical-documentation/county-changes.2010.html
stateL = xwalks::state2div$statefp


# get cts from 2010/2015 ; census bureau has official county correspond --------
library(purrr)
cts_2010 = purrr::map(as.numeric(stateL),
                           possibly( ~tigris::tracts(state = .,
                                          cb = T,
                                          year = 2010)
                                     , otherwise = NA)
                          ) # they added more colonies since 2010 i guess? so i need the possibly
cts_2010 <- cts_2010[!is.na(cts_2010)] %>% do.call('rbind', .)

cts_2015 = purrr::map_dfr(stateL,
                          ~tigris::tracts(state = .,
                                          cb = T,
                                          year = 2015)
)



# compare ----------------------------------------------------------------------
# insane that the 2010 one is so disorganized! both state and statefp columns, smh

tibble(cts_2010)

cts_2010$GEOID.2010 = paste0(cts_2010$STATEFP,
                        cts_2010$COUNTYFP,
                        cts_2010$TRACT )
cts_2015$GEOID.2015 = cts_2015$GEOID
'sum(duplicated(cts_2010$TRACT))
tibble(cts_2015)

cts_2010[! cts_2010$GEOID %in% cts_2015$GEOID,] %>% tibble() %>% count(STATEFP)
cts_2010[! cts_2010$GEO_ID %in% cts_2015$AFFGEOID,] %>% tibble()
'

# just generate spatial overlap xwalk ------------------------------------------

ten2fifteen =
  xwalks::get.spatial.overlap(cts_2010, cts_2015,
                              "GEOID.2010" , "GEOID.2015"
                              )
