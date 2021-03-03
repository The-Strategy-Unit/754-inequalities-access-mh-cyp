#' Get CCG GSS to ODS code lookup
#'
#' Gets the lookup between GSS (E38) and ODS codes
#'
#' @return tibble
#'
#' @export
get_ccg_gss_to_ods <- function() {
  chd_url <- "https://www.arcgis.com/sharing/rest/content/items/2dc879e2df52482ba764d3c11cc3150b/data"

  tmp_dir <- tempdir()
  chd_zip <- file.path(tmp_dir, "chd.zip")
  chd_csv <- file.path(tmp_dir, "Equivalents_V2.csv")

  withr::with_file(chd_zip, {
    utils::download.file(chd_url, chd_zip, mode = "wb")
    withr::with_file(chd_csv, {
      utils::unzip(chd_zip, files = "Equivalents_V2.csv", exdir = tmp_dir)
      chd <- readr::read_csv(chd_csv, col_types = readr::cols(.default = readr::col_character()))
    })
  })

  chd %>%
    dplyr::filter(.data$ENTITYCD == "E38") %>%
    dplyr::distinct(ccgcd = .data$GEOGCD, ccgcdh = .data$GEOGCDH)
}
