#' state2div
#'
#' Unique row/state w/ xwalk to larger areas: divisions, regions. Census Bureau
#' definition:
#' https://www2.census.gov/geo/pdfs/maps-data/maps/reference/us_regdiv.pdf
"state2div"


#' ct_cbsa_xwalk
#'
#' Unique row/ct with xwalk to cbsas **i should double-check this one
"ct_cbsa_xwalk"

#' co2cz
#'
#' Unique row/county with xwalk to cz and state.
"co2cz"

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

