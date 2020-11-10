# 2010 CTs to 1990 CZs

#rm(list = ls())
# dyn.load("/usr/local/gdal/2.2.4/lib64/libgdal.so.20.3.3") # necessary to do the spatial work thru server
suppressMessages(library(sf))
suppressMessages(library(dplyr))
suppressMessages(library(data.table))


# just get directly from opp insights ---------------------------------------
# I.e., table 4 --
# https://opportunityinsights.org/wp-content/uploads/2019/07/Codebook-for-Table-9.pdf




# download & extract compressed .csv from their site:
temp <- tempfile()

# (take a minute to run: )
download.file("https://opportunityinsights.org/wp-content/uploads/2018/10/tract_outcomes.zip"
              ,temp)
opp_cts <- read.csv(unz(temp, "tract_outcomes_early.csv"))

unlink(temp)

tibble(opp_cts)

# drop unneeded cols -----------------------------------------------------------

id_cols <- c("state",  # all areas are 2010 FIPS codes, except 1990 for CZs
             "county",
             "tract",
             "cz", "czname")

ct2czs <- opp_cts %>%
  select(tidyselect::all_of(id_cols))

(ct2czs <- tibble(ct2czs))

nchar(ct2czs$tract) %>% unique()
nchar(ct2czs$cz) %>% unique()

ct2czs$statefp = stringr::str_pad(ct2czs$state, 2, "left", "0")
ct2czs$countyfp = stringr::str_pad(ct2czs$county, 3, "left", "0")
ct2czs$tractce = stringr::str_pad(ct2czs$tract, 6, "left", "0")
ct2czs$cz = stringr::str_pad(ct2czs$cz, 5, "left", "0")
ct2czs$geoid = paste0(ct2czs$statefp,
                      ct2czs$countyfp,
                      ct2czs$tractce)

ct2czs <- ct2czs %>%
  select(statefp, countyfp, tractce,
         geoid10 = geoid,
         cz, czname)


# write ------------------------------------------------------------------------

usethis::use_data(ct2czs,
                  overwrite = T)
