library(tidyverse)
library(targets)
library(janitor)

## Import fingertips data + lookups ####
fingertips_data <- tar_read(fingertips_data)
ccg_successors <- tar_read(ccg_successors)
stp_lookups <- tar_read(stp_lookups)
ccg_gss_lookup <- tar_read(ccg_gss_to_ods)
stp_nhs_region_lookups <- tar_read(stp_nhs_region_lookups)

#download.file("https://opendata.arcgis.com/datasets/888dc5cc66ba4ad9b4d935871dcce251_0.csv", "tmp/ccg20_stp_region.csv")
ccg20_stp_region <- read_csv("tmp/ccg20_stp_region.csv") %>% clean_names()

#download.file("https://opendata.arcgis.com/datasets/520e9cd294c84dfaaf97cc91494237ac_0.csv", "tmp/lsoa_ccg19_stp19.csv")
lsoa_ccg19_stp19 <- read_csv("tmp/lsoa_ccg19_stp19.csv") %>% clean_names()

library(readxl)
la_successors <- read_excel("C:/Users/alexander.lawless/OneDrive - Midlands and Lancashire CSU/Data_management/Local_authority_structural_changes_2017_20.xlsx",
                           na = "NA") %>%
 clean_names() %>%
 mutate(old_code = case_when(!is.na(lad17cd) ~ lad17cd,
                             !is.na(lad18cd) ~ lad18cd,
                             TRUE ~ lad19cd))

fingertips_data_need <-
  fingertips_data %>%
  filter(indicator_id %in% c(93587, 91137,91138, 91139, 91146, 91145, 92315, 90813, 92796, 91871, 91578, 91579, 91580),
         area_type != "England",
         sex == "Persons") %>%
  select(indicator_id, indicator_name, area_type, area_code, area_name, sex, age, timeperiod_sortable,
         timeperiod, value, lower_ci95_0limit, upper_ci95_0limit, count, denominator) %>%
  group_by(indicator_id, area_code) %>%
  mutate(time_period_rank = rank(-timeperiod_sortable)) %>%
  filter(time_period_rank == min(time_period_rank, na.rm = T))%>% # filter on min(time_period_rank) to select most recent value
  ungroup()


## Organise fingertips data into CCG dataset and LA dataset ####
# 1. CCG's
ft_need_ccg <-
fingertips_data_need %>%
  filter(area_type != "County & UA (pre 4/19)") %>%
  left_join(ccg_gss_lookup, by= c("area_code" = "ccgcd")) %>% # 0 na's
  left_join(ccg_successors, by = c("ccgcdh" = "old_code")) %>%
  left_join(ccg20_stp_region, by = c("new_code" = "ccg20cdh")) %>%
  mutate(midlands_flag = case_when(nhser20nm == "Midlands" ~ "Midlands",
                                   TRUE ~ "External"))

# 2. LA's
ft_need_la <-
  fingertips_data_need %>%
  filter(area_type == "County & UA (pre 4/19)") %>%

  left_join(select(la_successors, old_code, lad20cd, lad20nm),
            by = c("area_code" = "old_code"), keep = TRUE) %>% # join 2020 LA codes
  mutate(recent_area_code = case_when(is.na(lad20cd) ~ area_code,
                                      TRUE ~ lad20cd)) %>% # if different to 2020 code, use 2020 code
  left_join(select(la_to_ccg_lookup, LAD20CD, CCG20CD),
            by = c("recent_area_code" = "LAD20CD")) %>% # LA_CCG look up
  left_join(select(UA_CCG_lookup, ctyua19cd, ccg20cd), by = c("recent_area_code" = "ctyua19cd")) %>% # UA_CCG look up
  mutate(der_ccg = if_else(!is.na(CCG20CD), CCG20CD, ccg20cd)) %>%  # Prioritises LA derived CCG, otherwise look for UA derived
  left_join(stp_lookups %>%
              select(ccg20cd, ccg20nm, stp20cd) %>%
              distinct(), by = c("der_ccg" = "ccg20cd")) %>% # Assign STP to CCG codes
  left_join(stp_nhs_region_lookups, by = c("stp20cd" = "stp20cd"), keep = FALSE) %>%
  mutate(midlands_flag = case_when(nhser20nm == "Midlands" ~ "Midlands",
                                   TRUE ~ "External"))

#### Original approach - Check if this is still needed
#fingertips_data_need %>%
#  filter(area_type == "County & UA (pre 4/19)") %>%
#  mutate(area_code = if_else(area_code == "E10000019", "E07000138",
#                             if_else(area_code == "E10000021", "E07000154",
#                                     area_code))) %>%  # fudge number 1: replacing Lincolnshire & Northamptonshire unitary authority with LA codes
#  left_join(stp_lookups %>%
#              select(stp20cd, stp20nm, lad20cd, lad20nm) %>%
#              distinct(), by = c("area_code" = "lad20cd")) %>%
#  left_join(stp_nhs_region_lookups, by = c("stp20cd" = "stp20cd")) %>%
#  mutate(midlands_flag = case_when(nhser20nm == "Midlands" ~ "Midlands",
#                                   TRUE ~ "External")) %>%
#  filter(!(area_name == "Birmingham" & stp20nm.y  == "The Black Country and West Birmingham")) # remove double counting of Birmingham LA (contains 2x CCG's)

# E10000019 UA - Lincolnshire ---- LA E07000138	Lincoln
# E10000021 UA - Northamptonshire ----- LA E07000154	Northampton



## PHE fingertips indicators by STP ####
# CCG indicators by STP
CCG_indicators_STP_summary <-
ft_need_ccg %>%
  filter(indicator_id == 91578) %>% # Prevalence of emotional disorders (GP registered pop aged 5-16) (2015)
  group_by(stp20nm, stp20cd) %>%
  summarise(emot_dis_count_sum = sum(count),
            emot_dis_denom_sum = sum(denominator)) %>%
  mutate(emot_dis_value = emot_dis_count_sum/emot_dis_denom_sum*100) %>%

  left_join(stp_nhs_region_lookups %>%
              select(stp20cd, nhser20nm) %>%
              distinct(),
            by = c("stp20cd" = "stp20cd")) %>%
  mutate(midlands_flag = case_when(nhser20nm == "Midlands" ~ "Midlands",
                                   TRUE ~ "External")) %>%
  select(1,2,7,3:5) %>%
  #filter(midlands_flag == "Midlands") %>%

  left_join(ft_need_ccg %>%
              filter(indicator_id == 91579) %>% # Prevalence of conduct disorders (2015)
              group_by(stp20nm) %>%
              summarise(cond_dis_count_sum = sum(count),
                        cond_dis_denom_sum = sum(denominator)) %>%
              mutate(cond_dis_value = cond_dis_count_sum/cond_dis_denom_sum*100),
            by = "stp20nm") %>%

  left_join(ft_need_ccg %>%
              filter(indicator_id == 91580) %>% # Prevalence of hyperkinetic disorders (2015)
              group_by(stp20nm) %>%
              summarise(hyperkin_dis_count_sum = sum(count),
                        hyperkin_dis_denom_sum = sum(denominator)) %>%
              mutate(hyperkin_dis_value = hyperkin_dis_count_sum/hyperkin_dis_denom_sum*100),
            by = "stp20nm") %>%

  left_join(ft_need_ccg %>%
              filter(indicator_id == 90813) %>% # Self harm (2014/15)
              group_by(stp20nm) %>%
              summarise(self_harm_count_sum = sum(count),
                        self_harm_denom_sum = sum(denominator)) %>%
              mutate(self_harm_value = self_harm_count_sum/self_harm_denom_sum*10000),
            by = "stp20nm")

# LA indicators by STP
LA_indicators_STP_summary <-
  ft_need_la %>%
  filter(indicator_id == 91145) %>%
  group_by(stp20nm, stp20cd) %>%
  summarise(adhd_count_sum = sum(count),
            adhd_denom_sum = sum(denominator)) %>%
  ungroup() %>%
  mutate(adhd_prev = adhd_count_sum/adhd_denom_sum*100) %>%

  left_join(
    ft_need_la %>%
      filter(indicator_id == 91146) %>%
      group_by(stp20cd) %>%
      summarise(eating_dis_count_sum = sum(count),
                eating_dis_denom_sum = sum(denominator)) %>%
      ungroup() %>%
      mutate(eating_dis_prev = eating_dis_count_sum/eating_dis_denom_sum*100),
    by = c("stp20cd")) %>%

  left_join(
    ft_need_la %>%
      filter(indicator_id == 91871) %>%
      group_by(stp20cd) %>%
      summarise(pupil_dis_count_sum = sum(count),
                pupil_dis_denom_sum = sum(denominator)) %>%
      ungroup() %>%
      mutate(pupil_dis_prev = pupil_dis_count_sum/pupil_dis_denom_sum*100),
    by = c("stp20cd")) %>%

  left_join(
    ft_need_la %>%
      filter(indicator_id == 92315) %>%
      group_by(stp20cd) %>%
      summarise(lac_count_sum = sum(count),
                lac_denom_sum = sum(denominator)) %>%
      ungroup() %>%
      mutate(lac_prev = lac_count_sum/lac_denom_sum*100),
    by = c("stp20cd")) %>%

  left_join(
    ft_need_la %>%
      filter(indicator_id == 93587) %>%
      group_by(stp20cd) %>%
      summarise(childen_mh_count_sum = sum(value),
                children_mh_denom_sum = sum(denominator)) %>%
      ungroup() %>%
      mutate(childen_mh_prev = childen_mh_count_sum/children_mh_denom_sum*100),
    by = c("stp20cd")
  )

 #write_csv(LA_STP_summary, "C:/Users/alexander.lawless/OneDrive - Midlands and Lancashire CSU/1. Projects/2020_21/DSU_Inequalities_in_access_to_MH_CYP/Need and demand/LA_STP_summary.csv")

## PHE fingertips indicators by CCG ####

CCG_indicators_CCG_summary <-
  ft_need_ccg %>%
  filter(indicator_id == 91578) %>% # Prevalence of emotional disorders (GP registered pop aged 5-16) (2015)
  group_by(ccg20cd, ccg20nm, stp20cd, stp20nm) %>%
  summarise(emot_dis_count_sum = sum(count),
            emot_dis_denom_sum = sum(denominator)) %>%
  mutate(emot_dis_value = emot_dis_count_sum/emot_dis_denom_sum*100) %>%

  left_join(stp_nhs_region_lookups %>%
              select(stp20cd, nhser20nm) %>%
              distinct(),
            by = c("stp20cd" = "stp20cd")) %>%
  mutate(midlands_flag = case_when(nhser20nm == "Midlands" ~ "Midlands",
                                   TRUE ~ "External")) %>%
  left_join(ft_need_ccg %>%
              filter(indicator_id == 91579) %>% # Prevalence of conduct disorders (2015)
              group_by(ccg20cd, stp20nm) %>%
              summarise(cond_dis_count_sum = sum(count),
                        cond_dis_denom_sum = sum(denominator)) %>%
              mutate(cond_dis_value = cond_dis_count_sum/cond_dis_denom_sum*100),
            by = "ccg20cd") %>%
  left_join(ft_need_ccg %>%
              filter(indicator_id == 91580) %>% # Prevalence of hyperkinetic disorders (2015)
              group_by(ccg20cd, stp20nm) %>%
              summarise(hyperkin_dis_count_sum = sum(count),
                        hyperkin_dis_denom_sum = sum(denominator)) %>%
              mutate(hyperkin_dis_value = hyperkin_dis_count_sum/hyperkin_dis_denom_sum*100),
            by = "ccg20cd") %>%
  left_join(ft_need_ccg %>%
              filter(indicator_id == 90813) %>% # Self harm (2014/15)
              group_by(ccg20cd, stp20nm) %>%
              summarise(self_harm_count_sum = sum(count),
                        self_harm_denom_sum = sum(denominator)) %>%
              mutate(self_harm_value = self_harm_count_sum/self_harm_denom_sum*10000),
            by = "ccg20cd") %>%
  ungroup() %>%
  select(everything(),-contains("stp20nm"), - nhser20nm)

LA_indicators_CCG_summary <-
  ft_need_la %>%
  filter(indicator_id == 91145) %>%
  group_by(der_ccg, ccg20nm) %>%
  summarise(adhd_count_sum = sum(count),
            adhd_denom_sum = sum(denominator)) %>%
  ungroup() %>%
  mutate(adhd_prev = adhd_count_sum/adhd_denom_sum*100) %>%

  left_join(
    ft_need_la %>%
      filter(indicator_id == 91146) %>%
      group_by(der_ccg) %>%
      summarise(eating_dis_count_sum = sum(count),
                eating_dis_denom_sum = sum(denominator)) %>%
      ungroup() %>%
      mutate(eating_dis_prev = eating_dis_count_sum/eating_dis_denom_sum*100),
    by = c("der_ccg")) %>%

  left_join(
    ft_need_la %>%
      filter(indicator_id == 91871) %>%
      group_by(der_ccg) %>%
      summarise(pupil_dis_count_sum = sum(count),
                pupil_dis_denom_sum = sum(denominator)) %>%
      ungroup() %>%
      mutate(pupil_dis_prev = pupil_dis_count_sum/pupil_dis_denom_sum*100),
    by = c("der_ccg")) %>%

  left_join(
    ft_need_la %>%
      filter(indicator_id == 92315) %>%
      group_by(der_ccg) %>%
      summarise(lac_count_sum = sum(count),
                lac_denom_sum = sum(denominator)) %>%
      ungroup() %>%
      mutate(lac_prev = lac_count_sum/lac_denom_sum*100),
    by = c("der_ccg")) %>%

  left_join(
    ft_need_la %>%
      filter(indicator_id == 93587) %>%
      group_by(der_ccg) %>%
      summarise(childen_mh_count_sum = sum(value),
                children_mh_denom_sum = sum(denominator)) %>%
      ungroup() %>%
      mutate(childen_mh_prev = childen_mh_count_sum/children_mh_denom_sum*100),
    by = c("der_ccg")
  )


## Need summary ####
STP_need_summary <-
  CCG_indicators_STP_summary %>%
  #select(everything(), -contains("denom"), -contains("value")) %>%
  left_join(LA_indicators_STP_summary %>%
               #select( -contains("denom"), -contains("prev")) %>%
               select(-stp20cd), by = c("stp20nm" = "stp20nm")) %>%
  clean_names() %>%
  ungroup()

CCG_need_summary <-
  CCG_indicators_CCG_summary %>%
  left_join(select(LA_indicators_CCG_summary, -ccg20nm), by = c("ccg20cd" = "der_ccg"), keep = FALSE)

## Import demand data ####
STP_demand <-
  read_csv("C:/Users/alexander.lawless/OneDrive - Midlands and Lancashire CSU/1. Projects/2020_21/DSU_Inequalities_in_access_to_MH_CYP/Need and demand/STP_demand_summary.csv") %>%
  select(-1) %>%
  clean_names() %>%
  rename(stp20nm = stp20nm_x)

CCG_demand <-
  read_csv("C:/Users/alexander.lawless/OneDrive - Midlands and Lancashire CSU/1. Projects/2020_21/DSU_Inequalities_in_access_to_MH_CYP/Need and demand/CCG_demand_summary.csv") %>%
  select(-1) %>%
  clean_names() %>%
  rename(stp20nm = stp20nm_x)


## Merge need and demand ####

STP_need_demand <-
STP_need_summary %>%
  left_join(STP_demand %>% select(-stp20nm),
            by = c("stp20cd"), keep = FALSE) %>%
  select(everything(), -contains("denom"), -contains("value"), -contains("prev")) %>%
  mutate(emot_dis_ratio =     n_emotional_dis / emot_dis_count_sum,
         cond_ratio =         n_conduct_dis   / cond_dis_count_sum,
         hyperkin_ratio =     n_hyperkin_dis  / hyperkin_dis_count_sum,
         eating_dis_ratio =   n_eating_dis    / eating_dis_count_sum,
         self_harm_ratio =    self_harm_count_sum / n_self_harm     # Switch self-harm measures around
         )

CCG_need_demand <-
  CCG_need_summary %>%
  left_join(CCG_demand %>%
              select(-stp20cd), by = c("ccg20cd")) %>%
  mutate(emot_dis_ratio =     n_emotional_dis / emot_dis_count_sum,
         cond_ratio =         n_conduct_dis   / cond_dis_count_sum,
         hyperkin_ratio =     n_hyperkin_dis  / hyperkin_dis_count_sum,
         eating_dis_ratio =   n_eating_dis    / eating_dis_count_sum, # Local authority derived .........
         self_harm_ratio =    self_harm_count_sum / n_self_harm     # Switch self-harm measures around
  )





