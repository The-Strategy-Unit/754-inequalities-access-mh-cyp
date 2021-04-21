#' Get IMD
#'
#' Download the 2015 Indices of Deprivation
#'
#' @return a tibble
#'
#' @export
get_imd <- function() {
  url <- paste("https://assets.publishing.service.gov.uk/government",
               "uploads/system/uploads/attachment_data/file/467764",
               "File_1_ID_2015_Index_of_Multiple_Deprivation.xlsx",
               sep = "/")

  withr::local_file("imd.xlsx", {
    utils::download.file(url, "imd.xlsx", mode = "wb")

    imd <- readxl::read_excel("imd.xlsx", sheet = "IMD 2015") %>%
      dplyr::select(lsoa = 1, imd = 6)
  })

  imd
}
