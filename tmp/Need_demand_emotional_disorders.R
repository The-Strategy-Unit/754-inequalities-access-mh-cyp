# Demand for CYP Mental Health services

library(tidyverse)
library(janitor)

# 1. Emotional disorders
# Referrals
emotional_disorder_referrals <-
read_csv(
  "C:/Users/alexander.lawless/OneDrive - Midlands and Lancashire CSU/1. Projects/2020_21/DSU_Inequalities_in_access_to_MH_CYP/Need and demand/Emotional_disorder_MHSDS_referrals.csv",
  na = c("NULL", "NA")) %>%
  clean_names() %>%
  left_join(stp_lookups, by = c("lsoa2011" = "lsoa11cd"))

# SUS admissions
emotional_disorder_sus_admissions <-
  read_csv(
    "C:/Users/alexander.lawless/OneDrive - Midlands and Lancashire CSU/1. Projects/2020_21/DSU_Inequalities_in_access_to_MH_CYP/Need and demand/Emotional_disorder_SUS_admissions.csv",
    na = c("NULL", "NA")) %>%
  clean_names() %>%
  left_join(stp_lookups, by = c("der_postcode_lsoa_2011_code" = "lsoa11cd"))

#emotional_disorder_sus_opa <-
#  read_csv(
#    "C:/Users/alexander.lawless/OneDrive - Midlands and Lancashire CSU/1. Projects/2020_21/DSU_Inequalities_in_access_to_MH_CYP/Need and demand/Emotional_disorder_SUS_admissions.csv",
#    na = c("NULL", "NA")) %>%
#  clean_names() %>%
#  left_join(stp_lookups, by = c("der_postcode_lsoa_2011_code" = "lsoa11cd"))


# PbR cluster


# IAPT referrals





# Inner join 3x patient cohorts to identify distinct number of patients with Emotional disorders
test <-
emotional_disorder_referrals %>%
  filter(der_ref_calender_year == 2020) %>%
  select(der_pseudo_nhs_number) %>%
  distinct() %>% #72,891
  union(
    emotional_disorder_sus_admissions %>%
      mutate(calendar_year = substr(discharge_date, 1,4)) %>%
      filter(calendar_year == '2020') %>%
      select(der_pseudo_nhs_number) %>%
      distinct() #24,613
  ) #96,624



# Apply prevalence indicator to 2020 population estimates

