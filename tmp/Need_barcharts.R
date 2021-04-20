## Ordered bar chart ####
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
# *        <int>   <dbl>
# 1        90813  454.
# 2        91137    3.60
# 3        91138    5.66
# 4        91139    1.53
# 5        91145 5654.
# 6        91146 5350.
# 7        91871    2.38
# 8        92315   38.5
# 9        92796  444.
# 10        93587 6781.


# Prevalence of ADHD among young people: estimated number aged 16 - 24
ft_need_la_vis(91145, 5654, "Prevalence of ADHD among young people: estimated number aged 16 - 24", "County & UA (pre 4/19)")
# Prevalence of potential eating disorders among young people: estimated number aged 16 - 24
ft_need_la_vis(91146, 5350, "Prevalence of potential eating disorders among young people: estimated number aged 16 - 24", "County & UA (pre 4/19)")
# School pupils with social, emotional and mental health needs: % of school pupils
ft_need_la_vis(91871, 2.38, "School pupils with social, emotional and mental health needs: % of school pupils", "County & UA (pre 4/19)")
# Percentage of looked after children whose emotional wellbeing is a cause for concern
ft_need_la_vis(92315, 38.5, "Percentage of looked after children whose emotional wellbeing is a cause for concern ", "County & UA (pre 4/19)")
# Estimated number of children and young people with mental disorders  – aged 5 to 17
ft_need_la_vis(93587, 6781, "Estimated number of children and young people with mental disorders  – aged 5 to 17 ", "County & UA (pre 4/19)")
