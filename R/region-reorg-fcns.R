#' These functions just manipulate dataframes that contain multiple
#' regions/regiontypes (i.e., CZs and CBSAs). Some may have general broad use, others
#' may just help me shift quickly between conventions that I've developed for this
#' sort of data.
#'


#' abv.rcols
#'
#' Shortens identifier column names.
#'  region.name -> rn
#'
#'  region.id -> rid
#'
#'  region.type -> rt
#'
#' @param x df with columns with name to possibly abbreviate
#'
#' @export abv.rcols
abv.rcols <- function(x) {

  x %>%
    rename_with( ~gsub('region.name', 'rn', .x )) %>%
    rename_with( ~gsub('region.id', 'rid', .x )) %>%
    rename_with( ~gsub('region.type', 'rt', .x ))
}

#' add.rns
#'
#' Add region names to a data.frame long by region.type/region.id (and abbreviates
#' colnames by default).
#'
#' @param x df to add region names to
#' @param abvcols whether to abbreviate region id columns
#'
#' @export add.rns
add.rns <- function(x, abvcols=T) {
  xwn <- xwalks::ctx %>%
    select(matches('cz|cbsa')) %>%
    distinct()

  cbn <- region.reorg(na.omit(select(xwn, matches('cbsa'))), 'cbsa') %>%
    rename(region.name = cbsa_name)
  czn <- region.reorg(select(xwn, matches('cz')), 'cz') %>%
    rename(region.name = cz_name)
  rns <- rbind(czn,cbn) %>% distinct()

  if(abv.colnames) {
    x <- x %>% abv.rcols()
    rns <- rns %>% abv.rcols()
  }
  x <- x %>% left_join(rns)

  return(x)
}

#' fix.geoid
#'
#' Adds leading 0s if necessary and ensures that a given vector is a character
#' vector. Fixes geoids of any type that may have been read as numeric and turns into
#' valid identifiers.
#'
#' @param x a vector of identifiers that can be coerced to character
#' @param width If NULL, pads until all elements have width equal to the maximum
#'   width in vector. Otherwise, a numeric specifying width
#'
#' @export fix.geoid
fix.geoid <- function(x, width = NULL) {
  require(tidyverse)

  if(is.null(width))
    width <- max(nchar(x), na.rm = T)

  x %>%
    as.character() %>%
    stringr::str_pad(., width = width,
                     side = "left", "0")
}


#' region.reorg
#'
#' Reorganizes by region. Shifts from
#' cz/cbsa/county identifier columns to region.type/region.id identifier columns.
#'
#' @param x df to reorg
#' @param region.str string identifier existing region id column
#' @inheritParams add.rns
#'
#' @export region.reorg
region.reorg <- function(x, region.str, abvcols = T) {

  require(tidyverse)

  out <- x %>%
    mutate(region.type = region.str
           ,.before = region.str) %>%
    rename(region.id = region.str)

  if(abvcols)
    out <- out %>% abv.rcols()
  return(out)

}



