#' Get CYPMH
#'
#' Gets the Children's and Young Persons Mental Health Dataset from the MLCSU datawarehouse
#'
#' @param con_str the connection string
#' @param table_name the table name
#' @param ethnicity tibble containing columns ethnic_code and ethnicity, to remap
#' @param source_referral tibble containing columns source_ref_code and source_referral, to remap
#' @param last_update_cypmh results of last_update_cypmh.txt ref file
#'
#' @return a tibble
#'
#' @export
get_cypmh <- function(con_str,
                      table_name,
                      ethnicity,
                      source_referral,
                      last_update_cypmh = Sys.time()) {
  force(last_update_cypmh) # allows data to be reloaded without target changing

  con <- DBI::dbConnect(odbc::odbc(), .connection_string = con_str, timeout = 10)
  withr::defer( DBI::dbDisconnect(con) )

  dplyr::tbl(con, table_name) %>%
    dplyr::filter(.data$`Death<10yrs` == 0) %>%
    dplyr::transmute(
      lsoa = .data$LSOA_of_residence,
      gender = .data$MHD_Gender,
      age = as.numeric(.data$MHD_Age_Start_Reporting_Period),
      has_diagnosis = as.logical(.data$DiagnosisRecord),
      marital_status = .data$MHD_Marital_Status,
      ethnic_code = .data$MHD_Ethnicity_der,
      source_ref_code = .data$MHD_Source_of_Referral,
      employment_status = .data$MHD_Employment_Status,
      accomodation_status = .data$MHD_Accomodation_Status_2,
      spell_days = as.numeric(.data$MHD_Spell_Days_in_Reporting_Period),
      cpa_standard_days = as.numeric(.data$MHD_CPA_Standard_Days),
      cpa_enhanced_days = as.numeric(.data$MHD_CPA_Enhanced_Days),
      cpa_reviews = as.numeric(.data$MHD_Care_Programme_Approach_Reviews),
      days_detention = as.numeric(.data$MHD_Days_Liable_For_Detention),
      bed_days = as.numeric(.data$MHD_Bed_Days_Mental_Health),
      bed_days_ms = as.numeric(.data$MHD_Bed_Days_Mental_Health_Medium_Secure),
      bed_days_intensive = as.numeric(.data$MHD_Bed_Days_Mental_Health_Intensive),
      opa_consultant = as.numeric(.data$MHD_Out_Patient_Attendance_Consultant),
      opa_dna = as.numeric(.data$MHD_OutPatient_Did_Not_Attend_Mental_Health),
      con_community_psychiatric_nurse = as.numeric(.data$MHD_Contacts_Community_Psychiatric_Nurse),
      con_clinical_psychologist = as.numeric(.data$MHD_Contacts_Clinical_Psychologist),
      con_occupational_therapist = as.numeric(.data$MHD_Contacts_Occupational_Therapist),
      con_physiotherapist = as.numeric(.data$MHD_Contacts_Physiotherapist),
      con_consultant_pscycotherapy = as.numeric(.data$MHD_Contacts_Consultant_Psycotherapy),
      con_contacts_social_worker = as.numeric(.data$MHD_Contacts_Social_Worker),
      social_worker = as.logical(.data$MHD_Social_Worker_Involvement),
      admissions = as.numeric(.data$MHD_Admissions),
      days_prior = .data$DaysPrior,
      age_first_contact = as.numeric(.data$AgeFirstContact_der2),
      el_spells = .data$`EL Spells`,
      nel_spells = .data$`NEL Spells`,
      ae_attends = .data$`AE attends`,
      contact_y1 = as.logical(.data$ContactY1),
      contact_y2 = as.logical(.data$ContactY2),
      contact_y3 = as.logical(.data$ContactY3),
      contact_y4 = as.logical(.data$ContactY4),
      contact_y5 = as.logical(.data$ContactY5),
      contact_y6 = as.logical(.data$ContactY6),
      contact_y7 = as.logical(.data$ContactY7),
      contact_y8 = as.logical(.data$ContactY8),
      contact_y9 = as.logical(.data$ContactY9),
      contact_y10 = as.logical(.data$ContactY10),
      util_class = .data$UtilClass,
      util_description = .data$UtilDescription
    ) %>%
    dplyr::collect() %>%
    dplyr::filter(
      .data$gender %in% c("1", "2") # only complete genders
    ) %>%
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
      is_male = ifelse(.data$gender == "1", 1, 0),
      dplyr::across(c(.data$ethnic_code, .data$employment_status), stringr::str_sub, 1, 1)
    ) %>%
    dplyr::select(-.data$gender, -.data$util_class) %>%
    dplyr::left_join(ethnicity, by = "ethnic_code") %>%
    dplyr::left_join(source_referral, by = "source_ref_code") %>%
    dplyr::select(-.data$ethnic_code, -.data$source_ref_code) %>%
    dplyr::mutate(dplyr::across(ethnicity, tidyr::replace_na, "Unknown")) %>%
    dplyr::mutate(accomodation_status = dplyr::case_when(
      accomodation_status == "OC" ~ "99",
      TRUE ~ accomodation_status
    ))
}
