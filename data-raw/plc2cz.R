library(sf)
library(tidyverse)


# get all places ---------------------------------------------------------------

state_list <- xwalks::state2div$statefp %>% unique()
plcs <-
  map(state_list,
      ~tigris::places(.,
                      year = 2019,
                      cb= T))

# place_list[1]

# trim cols and rbind ----------------------------------------------------------
plcs <- plcs %>%
  map( ~select(.,
               STATEFP,
               GEOID, NAME, LSAD)) %>%
  do.call("rbind", .)

colnames(plcs) <-
  tolower(colnames(plcs))

# planar crs
plcs <-
  divM::conic.transform(plcs)

# get cbgs ---------------------------------------------------------------------

counties <- xwalks::co2cz$countyfp
geo.cbgs <-
  purrr::map2_dfr(substr(counties, 1, 2),
                  substr(counties, 3, 5),
                  ~tigris::block_groups(state = .x,
                                        county = .y,
                                        year = 2019,
                                        cb = T)
                  )

# pointilize
geo.cbgs <-
  divM::conic.transform(geo.cbgs)

geo.cbgs <-
  st_point_on_surface(geo.cbgs)

geo.cbgs <- geo.cbgs %>%
  select(cbg.geoid = GEOID)

# add xwalk to cbgs ------------------------------------------------------------

cbg2plc <- st_join(plcs,
                   geo.cbgs)

cbg2plc %>% filter(name == "New York")
cbg2plc %>%
  tibble() %>%
  count(name, geoid) %>%
  arrange(n)


cbg2plc %>% filter(name == "Aaronsburg")


# add xwalk to county ----------------------------------------------------------

plcs <- divM::conic.transform(plcs)

plc2cz <- st_join(plcs,
               divDat::czs_simplified)

plc2cz


# spot check a couple ----------------------------------------------------------
library(mapview)
plcs %>% filter(geoid %in% "0163288") %>%
  mapview() +
  mapview(divDat::czs %>%
            filter(cz %in% c("06100", "10700")) %>%
            st_boundary())

plc2cz %>% filter(name %in% !unique(name))

plcs %>%
  filter(statefp == "36") %>%
  ggplot() +
  geom_sf(aes(fill = name)) +
  geom_sf(data =
            st_boundary(
          filter(divDat::czs_simplified,
                 cz %in%
                   xwalks::co2cz[substr(xwalks::co2cz$countyfp, 1, 2) == "36", ]$cz
                 )),
          aes(color = cz)) +
  scale_fill_discrete(guide = F)

plcs["name"] %>% plot()

plc.pts <- st_point_on_surface(plcs)



# ------------------------------------------------------------------------------

# write
usethis::use_data(place_list )

