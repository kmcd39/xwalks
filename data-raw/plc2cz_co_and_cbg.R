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

# transform
geo.cbgs <-
  divM::conic.transform(geo.cbgs)


# trim colms & differntiate for after join
geo.cbgs <- geo.cbgs %>%
  select(cbg.geoid = GEOID)

# link cbgs 2 places -----------------------------------------------


plcs <- plcs %>% rename(plc.geoid = geoid)

cbg.plc <- xwalks::get.spatial.overlap(
  geo.cbgs,
  plcs,
  "cbg.geoid",
  "plc.geoid"
)


cbg.plc$perc.area %>% summary()

# add demographics ------------------------------------------------------
cbg.plc <- cbg.plc %>%
  arrange(cbg.geoid)

cbg.plc <- cbg.plc %>%
  tibble() %>%
  select(1:3)

abv.demos <- safegraphSeg::demos.2019 %>%
  select(1,2)

cbg.plc <- cbg.plc %>%
  left_join(abv.demos,
             by = c("cbg.geoid" = "geoid"))


# selected places -- those with min population >=100k
sel <- tibble(cbg.plc) %>%
  group_by(plc.geoid) %>%
  summarise(pop = sum(population * perc.area))


# plc geoids where pop >= 100k
sel <- sel %>%
  filter(pop >= 1e5)

sel.plcs <- plcs %>%
  filter(plc.geoid %in% sel$plc.geoid) %>%
  rename(geoid = plc.geoid)


sel.plcs

cbg.plc

# use pop-filtered plcs to generate a cbg2selectedplc xwalk --------------------
cbg2plc <- cbg.plc %>%
  filter(plc.geoid %in% sel$plc.geoid)

cbg2plc <-
  tibble(cbg2plc) %>%
  select(
    cbg = cbg.geoid,
    plc = plc.geoid,
    perc.overlap = perc.area
    )


# write
usethis::use_data(cbg2plc, overwrite=T)


# add xwalk to county & cz ----------------------------------------------------------

# many places ARE coterminous with counties, but not all of them
# ( https://www2.census.gov/geo/pdfs/reference/GARM/Ch9GARM.pdf )

# I make a xwalk with % overlaps, and filter only those that have <1% overlap of a
# Place in a given CZ
sel2cz_aw <- xwalks::get.spatial.overlap(
  sel.plcs,
  divDat::czs_simplified,
  "geoid",
  "cz"
)

# check
sel2cz_aw$perc.area %>% summary()
sel2cz_aw %>%
  filter(geoid %in%
           geoid[duplicated(geoid)]) %>%
  arrange(geoid)
# spot check map
library(mapview)
sel.plcs %>%
  filter(geoid %in% "0454050") %>%
  mapview() +
  mapview(divDat::czs %>%
            filter(cz %in%
                     c("35001",
                       "35401")) %>%
            st_boundary())


# clean clms -------------------------------------------------------------------

pop.plc2cz <-
  sel2cz_aw %>%
  tibble() %>%
  select(plc = 1,
         cz = 2,
         plc2cz.perc.overlap = 3)

pop.plc2cz


# add counties -----------------------------------------------------------------

plc.co.cz <-
  pop.plc2cz %>%
  left_join(xwalks::co2cz)


# last reorg -------------------------------------------------------------------

plc.co.cz <- plc.co.cz %>%
  select(1,2,cz_name, 3, 4 ,5)

plc.co.cz$plc2cz.perc.overlap <-
  round(plc.co.cz$plc2cz.perc.overlap * 100) %>% as.integer()

# ------------------------------------------------------------------------------

# write
usethis::use_data(plc.co.cz, overwrite = T)

