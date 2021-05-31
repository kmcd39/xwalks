


#' x2cos
#'
#' Given a cz,cbsa, or place identifier/geoid, gets all counties (5-character fp codes)
#' that overlap given region. countyfps correspond with 2015 counties.
#'
#' @param cz_name,cz,countyfp,cbsa_id,plc_id Identifier for region to get water for.
#'
#' @return List of 5-char county fp codes overlapping with region.
#'
#' @export
x2cos <- function(cz_name = NULL, cz = NULL,
                  countyfp = NULL,
                  cbsa_id = NULL, plc_id = NULL) {

  co2cz <- xwalks::co2cz

  if( is.null(c(cz_name, cz, countyfp, cbsa_id, plc_id)) )
    stop("No subset parameter provided -- supply a cz/county to subset to.")

  # if cz_name/_id is provided, parse out list of counties included in that cz_name.
  if (!is.null(cz_name))
    .countyfp <- co2cz[co2cz$cz_name %in% cz_name, ]$countyfp
  else if (!is.null(cz))
    .countyfp <- co2cz[co2cz$cz %in% cz, ]$countyfp
  else if (!is.null(cbsa_id))
    .countyfp <- xwalks::co2cbsa %>% filter(cbsa == cbsa_id) %>% pull(countyfp)
  else if(!is.null(plc_id))
    return( cbgs_from_plc(sfg, subset.cols, plc_id) ) # places are managed differently w/ helper fcn

  return(.countyfp)
}



#' plc2co
#'
#' Helper fcn called from `x2cos` when a plc_id is provided
#'
#' @inheritParams x2cos
#' @param overlap.threshold If a CBG has >= this % inside a Place boundaries, it will
#'   be included.
#'
plc2co <- function(plc_id,
                   overlap.threshold = 50) {

  cbg2plc <- xwalks::cbg2plc

  cbgs_in_place <- cbg2plc %>%
    filter(plc %in% plc_id &
             perc.overlap >=
             overlap.threshold) %>%
    pull(cbg)

  .countyfp <- substr(cbgs_in_place, 1, 5)

  return(.countyfp)
}
