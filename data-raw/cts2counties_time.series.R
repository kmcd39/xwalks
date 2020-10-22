library(sf)
library(dplyr)
rm(list=ls())
# https://www.census.gov/programs-surveys/geography/technical-documentation/county-changes.2010.html
stateL = xwalks::state2div$statefp

# 2015 cts generated elsewhere in pkg
#cts.2015 = divDat::cts
cts.2015 = purrr::map_dfr(stateL,
                          ~tigris::tracts(state = .,
                                          cb = F,
                                          year = 2015))


# get counties for 2010 and 2015 -----------------------------------------------
counties.2015 = tigris::counties(year = 2015)
counties.2010 = tigris::counties(year = 2010)


# differentiate colnames -------------------------------------------------------
colnames(cts.2015)[1:4] = paste0(colnames(cts.2015)[1:4], ".2015")
colnames(cts.2015)[1:4] = tolower(colnames(cts.2015)[1:4])
counties.2010 <- counties.2010 %>% select(1,2,3,4,5,6, geometry)


# check method by comparing 2015 counties bundled w/ cts and generated  --------
check.2015 = xwalks::generate.coterminous.xwalk(cts.2015, counties.2015)

check.2015[,c("countyfp.2015", "COUNTYFP")]
sum(check.2015$countyfp.2015 != check.2015$COUNTYFP)
# check
check.2015 <- check.2015 %>% select(1:4, NAMELSAD.y)
# generate 2015-2010 xwalk -----------------------------------------------------

ts10_15 = xwalks::generate.coterminous.xwalk(cts.2015, counties.2010)

colnames(ts10_15)
ts10_15 <- ts10_15 %>% select(1:6,13:18)

ts10_15 %>% filter(countyfp.2015 != COUNTYFP10)
# yay exactly the expected ones, as per
# https://www.census.gov/programs-surveys/geography/technical-documentation/county-changes.2010.html



cts2counties_time.series <-
  ts10_15 %>%
  select(
    statefp = statefp.2015,
    countyfp.2015,
    counfyfp.2010 = COUNTYFP10,
    tractce = tractce.2015,
    namelsad10 = NAMELSAD10
  )

# add 2015 county names too; why not
colnames(check.2015) = tolower(colnames(check.2015))
check.2015 <- check.2015 %>% select(1:4, namelsad15 = "namelsad.y")
check.2015$namelsad15

cts2counties_time.series <-
  cts2counties_time.series %>%
  left_join(check.2015,
            by = c("statefp" = "statefp.2015",
                   "countyfp.2015",
                   "tractce" = "tractce.2015")
            )

# rearrange cols and organize
cts2counties_time.series <- cts2counties_time.series %>%
  select(1,2,3,4,6,5,7)


# write ------------------------------------------------------------------------

usethis::use_data(cts2counties_time.series
                  ,overwrite = T)
