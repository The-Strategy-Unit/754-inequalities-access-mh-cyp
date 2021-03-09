#' Get STP to NHS region Lookups
#'
#' Get's the STP to NHS region lookups for 2020
#'
#' @return tibble
#'
#' @export
get_stp_nhs_region_lookups <- function() {
  readr::read_csv("https://opendata.arcgis.com/datasets/00613813dd4b4f2dba16268720b50bd4_0.csv") %>%
    janitor::clean_names()
}
