library(sf)
library(dplyr)
library(purrr)
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
    countyfp.2010 = COUNTYFP10,
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


# also check another way ------------------------------------------------------

# ct2ct xwalk
# it seems like some tract names change as well, which is annoying, so I generate
# ct2ct xwalk to see if this works better or can be more straightforward
cts.2015 = purrr::map_dfr(stateL,
                          ~tigris::tracts(state = .,
                                          cb = T,
                                          year = 2015))
colnames(cts.2015)[1:4] = paste0(colnames(cts.2015)[1:4], ".2015")
colnames(cts.2015)[1:4] = tolower(colnames(cts.2015)[1:4])

cts.2010 = purrr::map(stateL,
                        possibly( ~tigris::tracts(state = .,
                                                  cb = T,
                                                  year = 2010),
                                  otherwise = NA))
cts.2010 <- cts.2010[!is.na(cts.2010)]
cts.2010 <- do.call('rbind', cts.2010)
tibble(cts.2010)
cts.2010 <-cts.2010 %>% select(state10=2, county10=3, tract10=4,
                               NAME10 = NAME) %>%
  mutate(geoid10 = paste0(state10, county10, tract10))

tibble(cts.2015)
cts.2015 <- cts.2015 %>% select(1:3,geoid.2015 = GEOID, NAME15 = NAME, ALAND15=ALAND)

# it seems like census bureau broke out water and other special areas differently in
# between censuses, even though census tracts aren't supposed to change...
# from reference:
#The Census Bureau assigned a census tract code of 9900 to represent census tracts delineated to cover
#large bodies of water. In addition, census tract codes in the 9400s represent American Indian Areas and
#codes in the 9800s represent special land use areas.
#  drop those
cts.2015 %>% arrange(ALAND15)
cts.2015 <- cts.2015 %>% filter(ALAND15 > 0)
# cts.2010 %>% filter(as.numeric(NAME10) >= 9800 ) %>% mapview()
# cts.2010 %>% filter(as.numeric(NAME10) >= 9900 ) %>% mapview()
cts.2010 <- cts.2010 %>% filter(as.numeric(NAME10) < 9900 )
#cts.2010 <- cts.2010 %>% filter(ALAND15 > 0)
ct2ct = xwalks::get.spatial.overlap(cts.2010, cts.2015,
                                    "geoid10", "geoid.2015")

duplicated.geods10 = ct2ct$geoid10[duplicated(ct2ct$geoid10)]

sum(ct2ct$geoid10 != ct2ct$geoid.2015)

tmp = ct2ct %>%
  tibble() %>%
  group_by(geoid10) %>%
  filter(round(perc.area,2) ==
           round(max(perc.area),2))# %>%
  #ungroup() %>% group_by(geoid.2015) %>%
  #filter(round(perc.area,2) ==
  # round(max(perc.area),2))

cts.2010[!cts.2010$geoid10 %in% tmp$geoid10, ] %>% mapview()
# some territories and some additional "special land use areas" are new from 2010 definitions..
cts.2015[!cts.2015$geoid.2015 %in% tmp$geoid.2015, ] %>% mapview()

ct2ct <- ct2ct %>%
  tibble() %>%
  select(1:2) %>%
  left_join(select(tibble(cts.2010),
                   county10, geoid10)) %>%
  left_join(select(tibble(cts.2015),
                   county15 = COUNTYFP, geoid.2015)) %>%
  arrange(geoid10)


# write ------------------------------------------------------------------------

cts2counties_time.series <- ct2ct

usethis::use_data(cts2counties_time.series
                  ,overwrite = T)
