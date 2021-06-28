#' Get IMD
#'
#' Download the 2015 Indices of Deprivation
#'
#' @return a tibble
#'
#' @export
get_imd <- function() {
  url <- paste("https://assets.publishing.service.gov.uk/government",
               "uploads/system/uploads/attachment_data/file/6872",
               "1871524.xls",
               sep = "/")

  withr::local_file("imd.xls", {
    utils::download.file(url, "imd.xls", mode = "wb")

    imd <- readxl::read_excel("imd.xls", sheet = "IMD 2010") %>%
      dplyr::select(lsoa = 1, imd = 7) %>%
      dplyr::mutate(dplyr::across(imd, dplyr::ntile, 10))
  })

  imd
}
