library(tidyverse)
library(targets)
library(janitor)

fingertips_data <- tar_read(fingertips_data)
ccg_successors <- tar_read(ccg_successors)
stp_lookups <- tar_read(stp_lookups)
ccg_gss_lookup <- tar_read(ccg_gss_to_ods)
stp_nhs_region_lookups <- tar_read(stp_nhs_region_lookups)


download.file("https://opendata.arcgis.com/datasets/888dc5cc66ba4ad9b4d935871dcce251_0.csv", "tmp/ccg20_stp_region.csv")
ccg20_stp_region <- read_csv("tmp/ccg20_stp_region.csv") %>% clean_names()

download.file("https://opendata.arcgis.com/datasets/520e9cd294c84dfaaf97cc91494237ac_0.csv", "tmp/lsoa_ccg19_stp19.csv")
lsoa_ccg19_stp19 <- read_csv("tmp/lsoa_ccg19_stp19.csv") %>% clean_names()


## Import fingertips data ####
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
  mutate(area_code = if_else(area_code == "E10000019", "E07000138",
                             if_else(area_code == "E10000021", "E07000154",
                                     area_code))) %>%  # fudge number 1: replacing Lincolnshire & Northamptonshire unitary authority with LA codes
  left_join(stp_lookups %>%
              select(stp20cd, stp20nm, lad20cd, lad20nm) %>%
              distinct(), by = c("area_code" = "lad20cd")) %>%
  left_join(stp_nhs_region_lookups, by = c("stp20cd" = "stp20cd")) %>%
  mutate(midlands_flag = case_when(nhser20nm == "Midlands" ~ "Midlands",
                                   TRUE ~ "External")) %>%
  filter(!(area_name == "Birmingham" & stp20nm.y  == "The Black Country and West Birmingham")) # remove double counting of Birmingham LA (contains 2x CCG's)

# E10000019 UA - Lincolnshire ---- LA E07000138	Lincoln
# E10000021 UA - Northamptonshire ----- LA E07000154	Northampton


## PHE fingertips indicators by STP ####
# CCG indicators by STP
CCG_indicators_STP_summary <-
ft_need_ccg %>%
  filter(indicator_id == 91578) %>% # Prevalence of emotional disorders (GP registered pop aged 5-16) (2015)
  group_by(stp20nm, stp20cd) %>%
  summarise(Emot_dis_count_sum = sum(count),
            Emot_dis_denom_sum = sum(denominator)) %>%
  mutate(Emot_dis_value = Emot_dis_count_sum/Emot_dis_denom_sum*100) %>%

  left_join(stp_nhs_region_lookups %>%
              select(stp20cd, nhser20nm) %>%
              distinct(),
            by = c("stp20cd" = "stp20cd")) %>%
  mutate(midlands_flag = case_when(nhser20nm == "Midlands" ~ "Midlands",
                                   TRUE ~ "External")) %>%
  select(1,2,7,3:5) %>%
  filter(midlands_flag == "Midlands") %>%

  left_join(ft_need_ccg %>%
              filter(indicator_id == 91579) %>% # Prevalence of conduct disorders (2015)
              group_by(stp20nm) %>%
              summarise(Cond_dis_count_sum = sum(count),
                        Cond_dis_denom_sum = sum(denominator)) %>%
              mutate(Cond_dis_value = Cond_dis_count_sum/Cond_dis_denom_sum*100),
            by = "stp20nm") %>%

  left_join(ft_need_ccg %>%
              filter(indicator_id == 91580) %>% # Prevalence of hyperkinetic disorders (2015)
              group_by(stp20nm) %>%
              summarise(hyperkin_dis_count_sum = sum(count),
                        Hyperkin_dis_denom_sum = sum(denominator)) %>%
              mutate(Hyperkin_dis_value = hyperkin_dis_count_sum/Hyperkin_dis_denom_sum*100),
            by = "stp20nm") %>%

  left_join(ft_need_ccg %>%
              filter(indicator_id == 90813) %>% # Self harm (2014/15)
              group_by(stp20nm) %>%
              summarise(self_harm_count_sum = sum(count),
                        self_harm_denom_sum = sum(denominator)) %>%
              mutate(self_harm_value = self_harm_count_sum/self_harm_denom_sum*10000),
            by = "stp20nm")

# write_csv(CCG_STP_summary, "C:/Users/alexander.lawless/OneDrive - Midlands and Lancashire CSU/1. Projects/2020_21/DSU_Inequalities_in_access_to_MH_CYP/Need and demand/CCG_STP_summary.csv")

# LA indicators by STP
LA_indicators_STP_summary <-
  ft_need_la %>%
  filter(indicator_id == 91145) %>%
  group_by(stp20nm.y, stp20cd) %>%
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
      summarise(lAC_count_sum = sum(count),
                LAC_denom_sum = sum(denominator)) %>%
      ungroup() %>%
      mutate(lAC_prev = lAC_count_sum/LAC_denom_sum*100),
    by = c("stp20cd")) %>%

  left_join(
    ft_need_la %>%
      filter(indicator_id == 93587) %>%
      group_by(stp20cd) %>%
      summarise(Childen_mh_count_sum = sum(value),
                Children_mh_denom_sum = sum(denominator)) %>%
      ungroup() %>%
      mutate(Childen_mh_prev = Childen_mh_count_sum/Children_mh_denom_sum*100),
    by = c("stp20cd")
  )

 #write_csv(LA_STP_summary, "C:/Users/alexander.lawless/OneDrive - Midlands and Lancashire CSU/1. Projects/2020_21/DSU_Inequalities_in_access_to_MH_CYP/Need and demand/LA_STP_summary.csv")

## PHE fingertips indicators by CCG ####

CCG_indicators_CCG_summary <-
  ft_need_ccg %>%
  filter(indicator_id == 91578) %>% # Prevalence of emotional disorders (GP registered pop aged 5-16) (2015)
  group_by(ccg20cd, ccg20nm, stp20cd, stp20nm) %>%
  summarise(Emot_dis_count_sum = sum(count),
            Emot_dis_denom_sum = sum(denominator)) %>%
  mutate(Emot_dis_value = Emot_dis_count_sum/Emot_dis_denom_sum*100) %>%

  left_join(stp_nhs_region_lookups %>%
              select(stp20cd, nhser20nm) %>%
              distinct(),
            by = c("stp20cd" = "stp20cd")) %>%
  mutate(midlands_flag = case_when(nhser20nm == "Midlands" ~ "Midlands",
                                   TRUE ~ "External")) %>%
  filter(midlands_flag == "Midlands") %>%
  left_join(ft_need_ccg %>%
              filter(indicator_id == 91579) %>% # Prevalence of conduct disorders (2015)
              group_by(ccg20cd, stp20nm) %>%
              summarise(Cond_dis_count_sum = sum(count),
                        Cond_dis_denom_sum = sum(denominator)) %>%
              mutate(Cond_dis_value = Cond_dis_count_sum/Cond_dis_denom_sum*100),
            by = "ccg20cd") %>%
  left_join(ft_need_ccg %>%
              filter(indicator_id == 91580) %>% # Prevalence of hyperkinetic disorders (2015)
              group_by(ccg20cd, stp20nm) %>%
              summarise(hyperkin_dis_count_sum = sum(count),
                        Hyperkin_dis_denom_sum = sum(denominator)) %>%
              mutate(Hyperkin_dis_value = hyperkin_dis_count_sum/Hyperkin_dis_denom_sum*100),
            by = "ccg20cd") %>%
  left_join(ft_need_ccg %>%
              filter(indicator_id == 90813) %>% # Self harm (2014/15)
              group_by(ccg20cd, stp20nm) %>%
              summarise(self_harm_count_sum = sum(count),
                        self_harm_denom_sum = sum(denominator)) %>%
              mutate(self_harm_value = self_harm_count_sum/self_harm_denom_sum*10000),
            by = "ccg20cd") %>%
  ungroup() %>%
  select(everything(),-contains("stp20nm"), -midlands_flag, - nhser20nm)

LA_indicators_CCG_summary <-

############### Not sure about this part ...

## Need summary ####
STP_need_summary <-
  CCG_indicators_STP_summary %>%
  #select(everything(), -contains("denom"), -contains("value")) %>%
  left_join(LA_indicators_STP_summary %>%
               #select( -contains("denom"), -contains("prev")) %>%
               select(-stp20cd), by = c("stp20nm" = "stp20nm.y")) %>%
  select(-midlands_flag) %>%
  clean_names()


#write_csv(STP_need_summary,"C:/Users/alexander.lawless/OneDrive - Midlands and Lancashire CSU/1. Projects/2020_21/DSU_Inequalities_in_access_to_MH_CYP/Need and demand/STP_need_summary.csv")

CCG_need_summary <-


