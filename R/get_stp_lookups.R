#' Get STP Lookups
#'
#' Get's the LSOA/CCG/LAD to STP lookups for 2020
#'
#' @return tibble
#'
#' @export
get_stp_lookups <- function() {
  readr::read_csv("https://opendata.arcgis.com/datasets/1631beea57ff4e9fb90d75f9c764ce26_0.csv",
                  col_types = "_c_ccccc__cc") %>%
    janitor::clean_names()
}
