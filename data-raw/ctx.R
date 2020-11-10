library(lwgeom)
library(dplyr)
library(sf)
rm(list = ls())

cbsas = divDat::cbsas

counties = divDat::counties

stateL = xwalks::state2div %>% pull(statefp)

cts <- purrr::map_dfr( stateL
                       , ~tigris::tracts( state = .
                                          , cb = F
                                          , year = 2015 )
                       )

cts <- cts %>% divFcns::conic.transform()


# for visual --------------------------------------------------------------
'library(mapview)
tmp = cbsas %>%
  filter(grepl("Boston", cbsa_name)) %>%
  st_boundary()
tmp <- divFcns::conic.transform(tmp)

mapview(st_crop(cts
                  ,st_bbox(tmp))) +
  mapview(tmp, color = "white", lwd=2)'
#   -----------------------------------------------------------------------

head(cts)
cts <- cts %>% select(
  c(1,2,3,4, geometry) ) %>%
  st_make_valid()

ct2cbsa = xwalks::generate.coterminous.xwalk(
    cts
   , cbsas[,c("cbsa", "cbsa_name")]
   , F)

ct2cbsa

check = geoseg::cts
check= check %>% tibble()

nchar(unique(check$county))
check$tract = paste0( stringr::str_pad(check$state, 2, "left", "0")
                     ,stringr::str_pad(check$county, 3, "left", "0")
                     ,stringr::str_pad(check$tract, 6, "left", "0"))
check[ ,c("tract", "cbsa_id")]
ct2cbsa[ ,c("GEOID", "cbsa")]

v1 = paste(check$tract, check$cbsa_id, sep="-")
v2 = paste(ct2cbsa$GEOID, ct2cbsa$cbsa, sep="-")

v1[! v1 %in% v2 ]
v2[! v2 %in% v1 ]


# years must be different
# or newer method better
library(mapview)
'
check[! v1 %in% v2 ,] %>%
  count(cbsa_id)

check[! v1 %in% v2 ,] %>%
  filter(cbsa_id %in% c(10260
                        , 10500) ) %>%
  left_join(cts,
            by=c("tract" = "GEOID")) %>%
  st_sf() %>%
  #select(1:7)
  mapview(zcol = "cbsa_id") +
  mapview(st_boundary(
    filter(
    cbsas, cbsa %in% c(10260
                       , 10500)
    ))
    )
ct2cbsa %>% filter(GEOID %in% 72001956300)

cts %>%
  filter(STATEFP == "72") %>%
  left_join(ct2cbsa) %>%
  mapview(zcol = "cbsa_name", lwd=1, color = "white")+
  mapview(st_boundary(
    filter(cbsas,
           grepl(", PR", cbsa_name)))
    )

check %>%
  filter(cbsa_id == 10260) %>%
  left_join(cts,
            by=c("geoid" = "GISJOIN")) %>%
  st_sf() %>%
  mapview(zcol = "cbsa_id")

ct2cbsa %>%
  filter(cbsa == 10260)
'

ct2cbsa



# get co2cz xwalk to add CZs  ---------------------------------------------
colnames(cts) <- tolower(colnames(cts))
co2cz.disag <- tibble(
   statefp = substr(xwalks::co2cz$countyfp, 1,2)
  ,countyfp = substr(xwalks::co2cz$countyfp, 3,5)
  ,cz = xwalks::co2cz$cz
  ,cz_name = xwalks::co2cz$cz_name
)

ctx <- cts %>%
  divFcns::abv_out() %>%
  left_join(co2cz.disag)

ctx <- ctx %>% tibble()


# # check NA czs ----------------------------------------------------------
purrr::map(ctx, ~sum(is.na(.)))
ctx %>% filter(is.na(cz)) %>% count(statefp) # ( all in the territories )


# add cbsas ---------------------------------------------------------------
head(ctx)
colnames(ct2cbsa) <- tolower(colnames(ct2cbsa))

ctx <- ctx %>%
  left_join(ct2cbsa)
purrr::map(ctx, ~sum(is.na(.)))

ctx

# ensure convenient organization -----------------------------------------------



# write -------------------------------------------------------------------

usethis::use_data(ctx
                  ,overwrite = T)
