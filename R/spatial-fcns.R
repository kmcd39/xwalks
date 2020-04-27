#' Generate xwalk
#'
#' This function takes intersections and then trims out areas that intersect a
#' percentage area below \code{filter.threshold}. Generates a xwalk given two
#' shapefiles and colnames for unique identifiers for respective shapefiles.
#' Returns a table with the percent intersection of each area. Filters out
#' boundary cases where the intersection is a fraction of a percent of total
#' area This filter threshold takes out edge cases (when they touch but don't
#' overlap or where low res shapefile would lead to mis-read). .4 may seem high,
#' but there a couple jigsaw CBSAs in the country that (with a low-res
#' shapefile) cause duplicate cbsa-ct combos if the filter threshold is lower.
#' --See Boulder+Denver as example
#' @import sf dplyr
#' @export
get.spatial.overlap <- function(shp1, shp2,
                                shp1.title = "GEOID10", shp2.title = "CBSAFP"
                                , filter.threshold = 0.4) {
  # add areas to each shpfile
  shp1$area_1 <- st_area(shp1)
  shp2$area_2 <- st_area(shp2)
  # get intersecting areas
  intersection <- st_intersection(shp1,
                                  shp2)
  # calculate areas
  intersection$int.area <-
    st_area(intersection$geometry)

  # group appropriately & calculate intersections / whole
  intersection %>%
    mutate(perc.area = # % shpfile 1 contained in intersection area
             as.numeric(int.area) /
             as.numeric(area_1)) %>%
    filter(perc.area > filter.threshold) %>%
    select(c(all_of(shp1.title),
             all_of(shp2.title),
             "perc.area"))
}


