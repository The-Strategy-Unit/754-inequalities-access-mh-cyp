library(tidyverse)
library(targets)

fingertips_data <- tar_read(fingertips_data)
ccg_successors <- tar_read(ccg_successors)
stp_lookups <- tar_read(stp_lookups)
ccg_gss_lookup <- tar_read(ccg_gss_to_ods)
stp_nhs_region_lookups <- tar_read(stp_nhs_region_lookups)


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


# Assign STP's to each CCG/LA
# 1. Where old CCG is used, join 2020 CCG - and bring in STP
# 2. Where LA is used, join 2020 STP from STP lookup


# 1. CCG's
ft_need_ccg <-
fingertips_data_need %>%
  filter(area_type != "County & UA (pre 4/19)") %>%
  left_join(ccg_gss_lookup, by= c("area_code" = "ccgcd")) %>% # 0 na's
  left_join(ccg_successors, by = c("ccgcdh" = "old_code")) %>%
  left_join(stp_lookups %>%
              select(ccg20cdh, ccg20cd, ccg20nm, stp20cd, stp20nm) %>%
              distinct(), by = c("new_code" = "ccg20cdh")) %>%
  left_join(stp_nhs_region_lookups, by = c("stp20cd" = "stp20cd")) %>%
  mutate(midlands_flag = case_when(nhser20nm == "Midlands" ~ "Midlands",
                                   TRUE ~ "External"))

# 2. LA's
ft_need_la <-
  fingertips_data_need %>%
  filter(area_type == "County & UA (pre 4/19)") %>%
  #left_join(ccg_gss_lookup, by = c("area_code" = "ccgcdh")) %>%

  left_join(stp_lookups %>%
              select(ccg20cdh, ccg20cd, ccg20nm, stp20cd, stp20nm, lad20cd, lad20nm) %>%
              distinct(), by = c("area_code" = "lad20cd")) %>%
  group_by(stp20nm) %>% summarise(n = n()) %>%  arrange(desc(n)) # 406 NA's
  left_join(stp_nhs_region_lookups, by = c("stp20cd" = "stp20cd")) %>%
  mutate(midlands_flag = case_when(nhser20nm == "Midlands" ~ "Midlands",
                                 TRUE ~ "External"))


# CCG indicators graph function
ft_need_ccg_vis <- function(indicator, mean, title, subtitle) {

  subdata <-
    ft_need_ccg %>%
    filter(indicator_id == indicator) %>%
    filter(time_period_rank == max(time_period_rank)) %>%
    drop_na(stp20nm.x) %>%
    mutate(area_name = fct_reorder(area_name, value))

  subdata %>%
    ggplot(aes(x = area_name, y = value)) +
    geom_col(alpha = 0.5, fill = "#bdbdbd") +
    geom_col(data = subdata %>% filter(midlands_flag == "Midlands"), aes(fill = stp20nm.x)) +
    geom_errorbar(aes(ymin = lower_ci95_0limit, ymax = upper_ci95_0limit), width = 0.4, colour = "#636363" ) +
    geom_hline(yintercept = mean, linetype = 'longdash') +
    scale_x_discrete(labels = function(x) {
      ifelse(x %in% unique(filter(subdata, midlands_flag == "Midlands")$area_name),
             str_remove_all(x, "NHS | CCG"),
             "")
    }) +
    theme(axis.text.x = element_text(size = 7, angle = 90, vjust = 0.5, hjust=1)) +
    labs(title = title , #indicator_name
         subtitle = subtitle,
         x = "Area Name",
         y = "Value",
         fill = "STP Name")

}


# Calculate means
ft_need_ccg %>%
  group_by(indicator_id) %>%
  filter(time_period_rank == max(time_period_rank)) %>%
  #filter(area_type == "CCGs (2018/19)") %>%
  summarise(mean = mean(value))

## A tibble: 5 x 2
#indicator_id    mean
#<int>   <dbl>
#1        90813  404.
#2        91578    3.57
#3        91579    5.59
#4        91580    1.51
#5        93587 4979.

# Self harm
ft_need_ccg_vis(90813, 406, "Hospital admissions as a result of self-harm (10-24 years)", "CCG's (2018/19)")
# Prevalence of emotional disorders (GP registered pop aged 5-16)
ft_need_ccg_vis(91578, 3.57, "Estimated prevalence of emotional disorders: % GP registered population aged 5-16", "CCGs (2017/18)")
# Prevalence of conduct disorders
ft_need_ccg_vis(91579, 5.59, "Estimated prevalence of conduct disorders: % GP registered population aged 5-16", "CCGs (2017/18)")
# Prevalence of hyperkinetic disorders
ft_need_ccg_vis(91580, 1.51, "Estimated prevalence of hyperkinetic disorders: % GP registered population aged 5-16", "CCGs (2017/18)")
# Estimated number of children and young people with mental disorders
ft_need_ccg_vis(93587, 4979, "Estimated number of children and young people with mental disorders  – aged 5 to 17", "CCGs (2017/18)")



# CCG indicators by STP
CCG_STP_summary <-
ft_need_ccg %>%
  filter(indicator_id == 91578) %>% # Prevalence of emotional disorders (GP registered pop aged 5-16) (2015)
  group_by(stp20nm.y, stp20cd) %>%
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
              group_by(stp20nm.y) %>%
              summarise(Cond_dis_count_sum = sum(count),
                        Cond_dis_denom_sum = sum(denominator)) %>%
              mutate(Cond_dis_value = Cond_dis_count_sum/Cond_dis_denom_sum*100),
            by = "stp20nm.y") %>%
  left_join(ft_need_ccg %>%
              filter(indicator_id == 91580) %>% # Prevalence of hyperkinetic disorders (2015)
              group_by(stp20nm.y) %>%
              summarise(hyperkin_dis_count_sum = sum(count),
                        Hyperkin_dis_denom_sum = sum(denominator)) %>%
              mutate(Hyperkin_dis_value = hyperkin_dis_count_sum/Hyperkin_dis_denom_sum*100),
            by = "stp20nm.y") %>%
  left_join(ft_need_ccg %>%
              filter(indicator_id == 90813) %>% # Self harm (2014/15)
              group_by(stp20nm.y) %>%
              summarise(self_harm_count_sum = sum(count),
                        self_harm_denom_sum = sum(denominator)) %>%
              mutate(self_harm_value = self_harm_count_sum/self_harm_denom_sum*10000),
            by = "stp20nm.y")


# write_csv(CCG_STP_summary, "C:/Users/alexander.lawless/OneDrive - Midlands and Lancashire CSU/1. Projects/2020_21/DSU_Inequalities_in_access_to_MH_CYP/Need and demand/CCG_STP_summary.csv")



# Local authorities indicators graph function
ft_need_la_vis <- function(indicator, mean, title, subtitle) {

  subdata <-
    ft_need_la %>%
    filter(indicator_id == indicator) %>%
    filter(time_period_rank == max(time_period_rank)) %>%
    mutate(area_name = fct_reorder(area_name, value))

  subdata %>%
    ggplot(aes(x = area_name, y = value)) +
    geom_col(alpha = 0.5, fill = "#bdbdbd") +
    geom_col(data = subdata %>% filter(midlands_flag == "Midlands"), aes(fill = stp20nm.x)) +
    geom_errorbar(aes(ymin = lower_ci95_0limit, ymax = upper_ci95_0limit), width = 0.4, colour = "#636363" ) +
    geom_hline(yintercept = mean, linetype = 'longdash') +
    scale_x_discrete(labels = function(x) {
      ifelse(x %in% unique(filter(subdata, midlands_flag == "Midlands")$area_name),
             str_remove_all(x, "NHS | CCG"),
             "")
    }) +
    theme(axis.text.x = element_text(size = 9, angle = 90, vjust = 0.5, hjust=1)) +
    labs(title = title , #indicator_name
         subtitle = subtitle,
         x = "Area Name",
         y = "Value",
         fill = "STP Name")

}

# Calculate means
ft_need_la %>%
  drop_na(value) %>%
  group_by(indicator_id) %>%
  summarise(mean = mean(value))

# # A tibble: 10 x 2
# indicator_id    mean
# <int>    <dbl>
# 90813   451.
# 91137     3.61
# 91138     5.66
# 91139     1.53
# 91145  5736.
# 91146  5427.
# 91871     2.37
# 92315    38.4
# 92796   441.
# 93587  6870.


# Self harm 1
ft_need_la_vis(90813, 451, "Hospital admissions as a result of self-harm (10-24 years)", "County & UA (pre 4/19)")
# Self harm 2
ft_need_la_vis(92796, 441, "Hospital admissions as a result of self-harm ", "County & UA (pre 4/19)")
# Estimated prevalence of emotional disorders: % population aged 5-16
ft_need_la_vis(91137, 3.61, "Estimated prevalence of emotional disorders: % population aged 5-16 ", "County & UA (pre 4/19)")
# Estimated prevalence of conduct disorders: % population aged 5-16
ft_need_la_vis(91138, 5.66, "Estimated prevalence of conduct disorders: % population aged 5-16 ", "County & UA (pre 4/19)")
# Estimated prevalence of hyperkinetic disorders: % population aged 5-16
ft_need_la_vis(91139, 1.53, "Estimated prevalence of hyperkinetic disorders: % population aged 5-16", "County & UA (pre 4/19)")
# Prevalence of ADHD among young people: estimated number aged 16 - 24
ft_need_la_vis(91145, 5736, "Prevalence of ADHD among young people: estimated number aged 16 - 24", "County & UA (pre 4/19)")
# Prevalence of potential eating disorders among young people: estimated number aged 16 - 24
ft_need_la_vis(91146, 5427., "Prevalence of potential eating disorders among young people: estimated number aged 16 - 24", "County & UA (pre 4/19)")
# School pupils with social, emotional and mental health needs: % of school pupils
ft_need_la_vis(91871, 2.37, "School pupils with social, emotional and mental health needs: % of school pupils", "County & UA (pre 4/19)")
# Percentage of looked after children whose emotional wellbeing is a cause for concern
ft_need_la_vis(92315, 3.84, "Percentage of looked after children whose emotional wellbeing is a cause for concern ", "County & UA (pre 4/19)")
# Estimated number of children and young people with mental disorders  – aged 5 to 17
ft_need_la_vis(93587, 6870, "Estimated number of children and young people with mental disorders  – aged 5 to 17 ", "County & UA (pre 4/19)")



