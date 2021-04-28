#' Process CYPMH
#'
#' Prepares the raw CYPMH data ready for use
#'
#' @param raw_cypmh results from get_cypmh
#' @param imd tibble containing columns lsoa and imd
#' @param ethnicity tibble containing columns ethnic_code and ethnicity, to remap
#' @param source_referral tibble containing columns source_ref_code and source_referral, to remap
#'
#' @return a tibble
#'
#' @export
process_cypmh <- function(raw_cypmh,
                          imd,
                          ethnicity,
                          source_referral) {
  df <- raw_cypmh %>%
    dplyr::mutate(
      dplyr::across(.data$ethnic_code, stringr::str_trim),
      dplyr::across(
        c(.data$el_spells, .data$nel_spells, .data$ae_attends),
        tidyr::replace_na,
        0
      ),
      dplyr::across(
        c(.data$source_ref_code, .data$employment_status, .data$accomodation_status),
        tidyr::replace_na,
        "99"
      ),
      dplyr::across(
        .data$util_description,
        forcats::fct_reorder,
        .data$util_class
      ),
      dplyr::across(c(.data$ethnic_code, .data$employment_status), stringr::str_sub, 1, 1),
      dplyr::across(.data$accomodation_status, ~ifelse(.x == "OC", "99", .x))
    ) %>%
    dplyr::inner_join(imd, by = "lsoa") %>%
    dplyr::left_join(ethnicity, by = "ethnic_code") %>%
    dplyr::left_join(source_referral, by = "source_ref_code") %>%
    dplyr::select(-.data$lsoa, -.data$ethnic_code, -.data$source_ref_code, -.data$util_class) %>%
    dplyr::mutate(dplyr::across(ethnicity, tidyr::replace_na, "Unknown"))

  split <- sample(1:3, nrow(df), TRUE, c(.6, .2, .2))

  purrr::map(
    c("train" = 1, "test" = 2, "validation" = 3),
    ~dplyr::slice(df, which(split == .x))
  )
}
