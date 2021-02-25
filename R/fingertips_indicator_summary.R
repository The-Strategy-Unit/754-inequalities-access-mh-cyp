library(tidyverse)
library(targets)

fingertips_data <- tar_read(fingertips_data)

## Exploring fingertips data:
# Profile: Children and Young People's Mental Health and Wellbeing
# 59x indicators
# Area types include: England, County & UA, District & UA, CCG's
# Age varies from 0-24
# Time_period: 2011/12 - 2017/18


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
         timeperiod, value, lower_ci95_0limit, upper_ci95_0limit, count, denominator)

fingertips_data_need %>%
  group_by(indicator_id, area_code) %>%
  mutate(time_period_rank = rank(-timeperiod_sortable)) %>% # filter on min(time_period_rank) to select most recent value
  filter(time_period_rank == min(time_period_rank, na.rm = T))%>%
  select(1,3,4,5,8,9 ,15)





