#' state2div
#'
#' Unique row/state w/ xwalk to larger areas: divisions, regions. Census Bureau
#' definition:
#' https://www2.census.gov/geo/pdfs/maps-data/maps/reference/us_regdiv.pdf
"state2div"

#' ctx
#'
#' Xwalk from 2010 census tracts to 2015 counties, 2019 cbsas, and 1990 czs
"ctx"


#' co2cz
#'
#' 2015 counties w/ unique row/county with xwalk to cz and state.
"co2cz"

#' cts2counties_time.series
#'
#' Correspondence from census tracts to counties at different years. Important note:
#' Two census tracts were split from 2010-2015 (in Alaska). The tract geoid for these
#' for 2015 are: 02195000200 & 02198000100. If merging, this will cause duplicates
#' for information pertaining to these two tracts.
#'
#' From Census Bureau, The part of 02105000300 that became 02195000200 has estimated
#' detached population of 1; and the part of 02195000200 that became 02198000100 has
#' estimated added population of 613.
#'
#' https://www.census.gov/programs-surveys/geography/technical-documentation/county-changes.2010.html
"cts2cts_time.series"


#' czip
#'
#' Xwalk between ZIPs and CZs. They're not co-terminous so % overlap is
#' included. For ZIPs that span multiple CZs, this xwalk will have multiple
#' rows/ZIP
"czip"

#' czip_no_secondary
#'
#' Unique row/ZIP with Xwalk to CZs. They're not co-terminous so % overlap is
#' included. For ZIPs that span multiple CZs, this xwalk will only contain a
#' xwalk to the CZ that contains the greatest share of the ZIP.
"czip_no_secondary"

