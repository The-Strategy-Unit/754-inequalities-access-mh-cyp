#' Get LAD Successors
#'
#' Maps LAD's from old codes to new codes where a LAD has been succeeded. Also includes a row for each of the current
#' LAD's from itself to itself. Succession is determined by comparing the 2016 list of LSOA's to LAD's to the 2020
#' list (`stp_lookups`) and finding LAD's that don't apppear in 2020 that appeared in 2016. We then join the two
#' lists on LSOA. If a LAD is split in two we chose the greatest match, based on which of the "new" LAD's has the
#' most LSOA's from the "old" LAD.
#'
#' @param stp_lookups the results of the stp_lookups target
#'
#' @return tibble
#'
#' @export
get_lad_successors <- function(stp_lookups) {
  sl <- dplyr::select(stp_lookups, .data$lsoa11cd, to = .data$lad20cd)

  url <- "https://opendata.arcgis.com/datasets/540454dbf5af4b3ab7d654eb68bae6ce_0.csv"

  unmatched <- url %>%
    # load the 2016 data
    readr::read_csv(col_types = readr::cols(.default = readr::col_skip(),
                                            LSOA11CD = readr::col_character(),
                                            LAD16CD = readr::col_character())) %>%
    # rename the columns
    dplyr::select(lsoa11cd = .data$LSOA11CD, from = .data$LAD16CD) %>%
    # find the lad's that are in the 2016 file that aren't in the 2020 file
    dplyr::anti_join(sl, by = c("from" = "to")) %>%
    # join the 2020 data to the 2016 data by lsoa
    dplyr::inner_join(sl, by = "lsoa11cd") %>%
    # no longer need the lsoa column
    dplyr::select(-.data$lsoa11cd) %>%
    # distinct may work here, this is to ensure there are no cases where a LAD splits in two, it will select the
    # "larger" area, based on which new area has the most of the original lsoa's
    dplyr::group_by(.data$from) %>%
    dplyr::slice_max(order_by = dplyr::n(), n = 1)

  # bind the lad's that haven't changed to those that have
  dplyr::bind_rows(
    dplyr::distinct(sl, from = .data$to, .data$to),
    unmatched
  )
}
