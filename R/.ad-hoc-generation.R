devtools::load_all(export_all = FALSE)


# create ct_cz xwalk
ct_county_xwalk
library(sf)
czs <- st_read("./../shapefiles/1990 commuting zones/cz1990.shp")
cts <- readRDS("./../shapefiles/2010 CTs/simplified-shp.RDS")

head(czs)
head(cts)

czs <- czs %>% st_transform(4326) %>% st_make_valid()
cts <- cts %>% st_transform(4326) %>% st_make_valid()

ct_cz_overlap <- xwalks::get.spatial.overlap(cts, czs,
                                             "GEOID10", "cz")


ct_cz_overlap
library(dplyr)
tmp <- ct_cz_overlap %>%
  data.frame() %>%
  count(GEOID10) %>%
  arrange(desc(n)) %>%
  filter(n > 1) %>%
  pull(GEOID10)


library(ggplot2)

ct_cz_overlap %>%
  filter(GEOID10 %in% tmp)

cts %>%
  filter(GEOID10 %in% tmp[1]) %>%
  ggplot() +
    geom_sf(aes(color = "#339944")) +
    geom_sf(data = filter(czs, cz %in% c(37200, 37000)),
          aes(color = factor(cz)),
          fill = NA) +
  scale_color_discrete()

cts %>%
  filter(GEOID10 %in% tmp[2]) %>%
  ggplot() +
  geom_sf(aes(color = "#339944")) +
  geom_sf(data = filter(czs, cz %in% c(28900, 28800)),
          aes(color = factor(cz)),
          fill = NA) +
  scale_color_discrete()

# two tracts that were made in places incorporated or altered 1990

# first one 06047002402 is in Merced, CA; i assign it to 37000 because Merced is
# in that CZ sources: https://usa.ipums.org/usa/volii/1990lma.shtml &
# https://censusreporter.org/profiles/14000US06047002402-census-tract-2402-merced-ca/

# the second one  08014031400 in Broomfield, CO; i assign this to the
# Denver-Aurora area 28900 because broomfield was assigned to that metro area


# assign split CTs -------------------------------------------------------

ct_cz_overlap %>%
  filter(GEOID10 %in% tmp)

glimpse(ct_cz_overlap)

ct_cz_overlap <- ct_cz_overlap[ !(ct_cz_overlap$GEOID10 == "06047002402" & ct_cz_overlap$cz == 37200), ]
ct_cz_overlap <- ct_cz_overlap[ !(ct_cz_overlap$GEOID10 == "08014031400" & ct_cz_overlap$cz == 28800), ]

ct_cz_overlap %>%
  filter(GEOID10 %in% tmp)

# drop geometry and add to package
ct_cz_xwalk <- ct_cz_overlap %>% data.frame() %>% select(tract.geoid = 1, 2)
usethis::use_data(ct_cz_xwalk)
