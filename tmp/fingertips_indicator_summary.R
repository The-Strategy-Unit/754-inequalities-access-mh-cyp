library(tidyverse)
library(targets)

fingertips_data <- tar_read(fingertips_data)
ccg_successors <- tar_read(ccg_successors)
stp_lookups <- tar_read(stp_lookups)
ccg_gss_lookup <- tar_read(ccg_gss_to_ods)

#stp_nhs_region_lookups <- tar_read(stp_nhs_region_lookups)


# Here temporarily
stp_nhs_region_lookups <-
  readr::read_csv("https://opendata.arcgis.com/datasets/00613813dd4b4f2dba16268720b50bd4_0.csv") %>%
  janitor::clean_names()



## Primary conditions to estimate:
# Depression,
# anxiety,
# trauma and distress (ACE?),
# conduct disorders (behavioural/anti-social),
# mood disorders,
# eating disorders,
# self-harm,
# hyperkinetic disorders (ADHD)


## Domain: Indicator of need (indicator_id, indicator_name)

# Local authorities
# 1, 93587, Estimated number of children and young people with mental disorders  â€“ aged 5 to 17
# 2, 91137, Estimated prevalence of emotional disorders: % population aged 5-16
# 3, 91138, Estimated prevalence of conduct disorders: % population aged 5-16
# 4, 91139, Estimated prevalence of hyperkinetic disorders: % population aged 5-16
# 5, 91146, Prevalence of potential eating disorders among young people: estimated number aged 16 - 24
# 6, 91145, Prevalence of ADHD among young people: estimated number aged 16 - 24
# 7, 92315, Percentage of looked after children whose emotional wellbeing is a cause for concern
# 8, 90813, Hospital admissions as a result of self-harm (10-24 years)
# 9, 92796, Hospital admissions as a result of self-harm
# 10, 91871, School pupils with social, emotional and mental health needs: % of school pupils with social, emotional and mental health needs

# CCG's
# 11, 91578, Estimated prevalence of emotional disorders: % GP registered population aged 5-16
# 12, 91579, Estimated prevalence of conduct disorders: % GP registered population aged 5-16
# 13, 91580, Estimated prevalence of hyperkinetic disorder: % GP registered population aged 5-16


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
  left_join(stp_lookups %>%
              select(ccg20cdh, ccg20cd, ccg20nm, stp20cd, stp20nm) %>%
              distinct(), by = c("ccgcdh" = "ccg20cdh")) %>%
  left_join(stp_nhs_region_lookups, by = c("stp20cd" = "stp20cd")) %>%
  mutate(midlands_flag = case_when(nhser20nm == "Midlands" ~ "Midlands",
                                   TRUE ~ "External"))


# 2. LA's
ft_need_la <-
  fingertips_data_need %>%
  filter(area_type == "County & UA (pre 4/19)") %>%
  left_join(select(stp_lookups, lad20cd, stp20cd, stp20nm), by = c("area_code" = "lad20cd")) %>%
  #group_by(stp20nm) %>%  summarise(n = n()) %>% arrange(desc(n)) %>%
  #filter(is.na(stp20nm)) %>%  select(area_code) %>%  distinct()




# Example visualisation
ft_need_ccg %>%
  filter(indicator_id == 90813) %>%
  filter(time_period_rank == max(time_period_rank)) %>%
  filter(area_type == "CCGs (2018/19)") %>%
  drop_na(stp20nm.x) %>%
  mutate(area_name = fct_reorder(area_name, value)) %>%

  ggplot(aes(x = area_name, y = value, fill = stp20nm.x, alpha = midlands_flag)) +
  geom_col() +
  theme(axis.text.x = element_text(size = 7, angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "Hospital admissions for self harm............" , #indicator_name
       subtitle = "Area type: CCG (2018/19)",
       x = "Area Name",
       y = "Value")


# Draft 2
draft <-
ft_need_ccg %>%
  filter(indicator_id == 90813) %>%
  filter(time_period_rank == max(time_period_rank)) %>%
  filter(area_type == "CCGs (2018/19)") %>%
  drop_na(stp20nm.x) %>%
  mutate(area_name = fct_reorder(area_name, value))

#draft_mean <-
  draft %>%
  summarise(mean = mean(value))


draft %>%
ggplot(aes(x = area_name, y = value)) +
  geom_col(alpha = 0.5, fill = "#bdbdbd") +
  geom_col(data = draft %>% filter(midlands_flag == "Midlands"), aes(fill = stp20nm.x)) +
  geom_errorbar(aes(ymin = lower_ci95_0limit, ymax = upper_ci95_0limit), width = 0.2, colour = "#636363" ) +
  #geom_pointrange(aes(ymin = lower_ci95_0limit, ymax = upper_ci95_0limit), width = 0.2, colour = "#636363" ) +
  geom_hline(yintercept = 406, linetype = 'longdash') +
  scale_x_discrete(labels = function(x) {
    ifelse(x %in% unique(filter(draft, midlands_flag == "Midlands")$area_name),
           str_remove_all(x, "NHS | CCG"),
           "")
  }) +
  theme(axis.text.x = element_text(size = 9, angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "Hospital admissions for self harm............" , #indicator_name
       subtitle = "Area type: CCG (2018/19)",
       x = "Area Name",
       y = "Value",
       fill = "STP Name")


geom_errorbar(aes(ymin = lower_ci95_0limit, ymax = upper_ci95_0limit), width = 0.2, position = position_dodge(0.9))






