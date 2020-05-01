rm(list = ls())
library(dplyr)
library(sf)
library(data.table)
source("./R/spatial-fcns.R")

czs <- st_read("/scratch/network/km31/1990 commuting zones/cz1990.shp")
regions <- st_read("/scratch/network/km31/2018_us_regions/cb_2018_us_region_500k.shp")
counties <- readRDS("/scratch/network/km31/2018-counties/with-water/simplified/simplified-shp.RDS")

st_crs(czs)
st_crs(regions)

# get an opportunity insights table just for the cz code to czname correspondence
cz_outcomes <- read.csv("https://opportunityinsights.org/wp-content/uploads/2018/12/cz_covariates.csv")
# add cz names
czs <- czs %>%
  left_join(select(cz_outcomes, c(cz, czname)),
            by = c("cz"))

# use previously written spatial overlap fcn to create xwalk between counties and czs, after setting to same crs ----
# first set same CRS
czs <- st_transform(czs, st_crs(regions))
counties <- counties %>% rename(county = GEOID) %>% mutate(county = as.numeric(as.character(county))) %>% appHelpers::spatialize(st_crs(regions))

xwalk <- get.spatial.overlap(counties, czs,
                                     shp1.title = "county", shp2.title = "cz",
                                     filter.threshold = .05) # set a low filter threshold to catch islands where the county shpfile includes a lot of water


# check inclusion
head(xwalk)
nrow(xwalk)
nrow(counties)
# colonies are missing from cz table (expected)
counties[ !(counties$county %in% xwalk$county), c(1:5)]

# add cz names back into xwalks
xwalk <- xwalk %>%
  left_join(select(cz_outcomes, c(cz, czname)),
            by = c("cz")) %>%
  select(1,2,5,3,4)

# check for duplicates
count(data.frame(xwalk), cz) # you expect czs to contain multiple counties
count(data.frame(xwalk), county) %>% arrange(desc(n)) # a few cases where counties matched multiple czs
duplicates <- count(data.frame(xwalk), county) %>% arrange(desc(n)) %>% filter(n > 1) %>% pull(county)

# check duplicates
map_bordercases <- function(county.id, cz.id) {
  require(ggplot2)
  ggplot() +
    geom_sf(data = counties[counties$county == county.id,], color = "#0000EE") + # blue is county (incl. water)
    geom_sf(data = czs[czs$cz %in% cz.id, ], aes(color = factor(cz)), fill = NA) + # red is commuting zone (only land)
    scale_color_discrete()
}

xwalk[xwalk$county %in% duplicates, ] %>% arrange(county)

# juneau / sitka -- St PeterBourg Borough, AK spans between them. THe "county equivalent" w/ population 3,266 was created after CZs (in 2019)
map_bordercases(2195, c(34109, 34110))
# similar story here; Prince of Wales-Hyder Census Area, Alaska
map_bordercases(2198, c(34110, 34111))
map_bordercases(2282, c(34103, 34109)) # and Yukutat, also alaska. I think these will all be trimmed b/c no data regardless
# last one in coloroado different; Broomfield county--- belongs with boulder (also created after CZs)
map_bordercases(8014, c(28900, 28800))

# filter out the broomfield duplicate ( so remaining case still in )
# ( this also just apportions new alaska census areas to one cz based on which contains majority; excepted 2198 /St Peterburg is dropped)
xwalk <- xwalk %>%
  filter(!(county %in% duplicates &
           perc.area < .4))

# note, if filter threshold too high for initial generation, some islands dont match due to water area included in county shpfile
sum(!xwalk$cz %in% cz_outcomes$cz)
unmatched <- cz_outcomes[(!cz_outcomes$cz %in%  xwalk$cz ), c("cz", "czname")]
cz_outcomes[(!cz_outcomes$cz %in% czs$cz ),]
czs[czs$cz %in% unmatched$cz,] %>%
  ggplot() + geom_sf(aes(fill = cz))


counties[grepl("Bonner", counties$NAME),]
xwalk[xwalk$county == 16017, ]

map_bordercases(53055, 39301) # island
map_bordercases(36117, 18000) # on water
map_bordercases(8014, c(28900, 28800))
map_bordercases(16017, c(38601, 34104)) # Bonner County/bonner cz (shpfile has bonner as tiny cz in Alaska) - bonner county in ID near Spokane; bonner cz in alaska. TBH seems slightly wrong but it''s what shpfile has


# save xwalk as index as .csv to merge back with full app df
'data.frame(xwalk) %>%
  select(c("county", "cz", "czname")) %>%
  write.csv(file = "cz-county-xwalk.csv"
            , row.names = F)'


