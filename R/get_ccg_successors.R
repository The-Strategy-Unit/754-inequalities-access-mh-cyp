#' Get CCG Successors
#'
#' Maps CCG's from old codes to new codes where a CCG has been succeeded. Also includes a row for each of the current
#' CCG's from itself to itself
#'
#' @return tibble
#'
#' @export
get_ccg_successors <- function() {
  # load list off ccg's, keep only true ccg's (drops commissioning hubs)
  ccgs <- NHSRtools::ods_get_ccgs() %>%
    dplyr::filter(stringr::str_ends(name, "CCG"))

  # load ccg successors, make sure to filter both the old and new code columns
  successors <- NHSRtools::ods_get_successors() %>%
    dplyr::semi_join(ccgs, by = c("old_code" = "org_id")) %>%
    dplyr::semi_join(ccgs, by = c("new_code" = "org_id")) %>%
    dplyr::select(old_code, new_code)

  # take all of the ccg's that haven't been succeeded and add them in to list
  ccgs %>%
    dplyr::anti_join(successors, by = c("org_id" = "old_code")) %>%
    dplyr::transmute(old_code = org_id, new_code = org_id) %>%
    dplyr::bind_rows(successors)
}
