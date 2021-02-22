# note: if you get the error
#   Error in curl::ie_get_proxy_for_url() : ERROR_WINHTTP_UNRECOGNIZED_SCHEME
# then prese the windows button, type proxy, and disable "Automatically Detect Settings"
# see https://github.com/ropensci/fingertipsR/issues/56


#' Get Fingertips Data
#'
#' Function to load the required data from the fingertip API.
#'
#' @return tibble
#'
#' @export
get_fingertips_data <- function() {
  # Profile 133: Children and Young People's Mental Health and Wellbeing
  fingertipsR::fingertips_data(ProfileID = 133, AreaTypeID = "All") %>%
    dplyr::as_tibble() %>%
    janitor::clean_names()
}
