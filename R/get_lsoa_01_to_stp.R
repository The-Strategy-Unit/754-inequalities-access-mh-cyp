#' Get LSOA 2001 to STP lookup
#'
#' Creates a lookup from the 2001 lsoa's to STP
#'
#' @return a tibble
#' @export
get_lsoa_01_to_stp <- function() {
  lsoa_bfl <- "https://opendata.arcgis.com/datasets/cb316960f04c4a659ddf0dff6510eb3e_0.csv"
  lsoa_stp <- "https://opendata.arcgis.com/datasets/c7e5f64bf2a7447c92db212f27093230_0.csv"

  bfl <- readr::read_csv(lsoa_bfl, col_types = "c_c______")
  stp <- readr::read_csv(lsoa_stp, col_types = "_c____c___")

  df <- bfl %>%
    dplyr::inner_join(stp, by = "LSOA11CD") %>%
    janitor::clean_names() %>%
    dplyr::select(lsoa = .data$lsoa01cd, stp = .data$stp21cd) %>%
    dplyr::distinct(lsoa, stp)

  stopifnot("duplicate lsoa's found" = df %>%
              group_by(lsoa) %>%
              filter(n() > 1) %>%
              nrow() == 0)

  df
}
