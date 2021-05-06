## PHE Fingertips indicators - Scatter plot ####

library(ggrepel)

# STP need:demand ratio
scatter_function_ratio <- function(var_need_count, var_need_denom, var_need_value,
                                   var_ratio, var_demand_count,
                                   title) {

  sub_data <-
  STP_need_summary %>%
    select(stp20nm, stp20cd, var_need_count, var_need_denom, var_need_value) %>%
    left_join(STP_need_demand %>% select(-stp20nm, -var_need_count), by = c("stp20cd")) %>%
    ungroup() %>%
    select(1:5, var_ratio, var_demand_count, midlands_flag) %>%
    rename(need_count = var_need_count,
           need_denom = var_need_denom,
           need_value = var_need_value,
           demand_count = var_demand_count,
           ratio = var_ratio)  %>%
    mutate(ratio = -ratio)

  grids <-
    sub_data %>%
    summarise(min_prev = min(need_value),
              max_prev = max(need_value),
              mean_prev = sum(need_count)/sum(need_denom)*100,
              min_ratio = min(ratio),
              max_ratio = max(ratio),
              mean_ratio = sum(demand_count)/ sum(need_count)
              ) %>%
    mutate(mean_ratio = -mean_ratio)

  sub_data %>%
    ggplot(aes(x = need_value, y = ratio )) +
    annotate("rect", xmin = grids$min_prev, xmax = grids$mean_prev, ymin = grids$min_ratio, ymax = grids$mean_ratio, fill= "#abdda4", alpha = 0.8) + #BL
    annotate("rect", xmin = grids$min_prev, xmax = grids$mean_prev, ymin = grids$mean_ratio, ymax = grids$max_ratio, fill= "#ffed6f", alpha = 0.8) +
    annotate("rect", xmin = grids$mean_prev, xmax = grids$max_prev, ymin = grids$min_ratio, ymax = grids$mean_ratio, fill= "#ffed6f", alpha = 0.8) +
    annotate("rect", xmin = grids$mean_prev, xmax = grids$max_prev, ymin = grids$mean_ratio, ymax = grids$max_ratio, fill= "#d73027", alpha = 0.8) + #TR

    annotate("text", x = (grids$min_prev*0.99), y = (grids$max_ratio*1.75), label = "High unmet need",
             size = 4, angle = 90) +
    annotate("text", x = (grids$min_prev*0.99), y = (grids$min_ratio*0.95), label = "Low unmet need",
             size = 4, angle = 90) +

    geom_point(data = sub_data %>% filter(midlands_flag != "Midlands"), size = 10, shape = 21, fill = "grey", colour = "black") +
    geom_point(data = sub_data %>% filter(midlands_flag == "Midlands"), size = 10, shape = 21, fill = "#f9bf07", colour = "black") +
    geom_label_repel(data = sub_data %>% filter(midlands_flag == "Midlands"),
                    aes(label = stringr::str_wrap(stp20nm,15)), box.padding = 2,
                    max.overlaps = getOption("ggrepel.max.overlaps", default = 15)) +
    scale_y_continuous(
      breaks = seq(from = 0, to = -1, by = -0.25), labels = c("0", "0.25", "0.50", "0.75", "1")
                       ) +
    theme(axis.text.y = element_blank()) +
    labs(title = title,
         fill = "STP Name",
         x = "Prevalence",
         y = "Need:Demand ratio")
  }

scatter_function_ratio("emot_dis_count_sum", "emot_dis_denom_sum", "emot_dis_value",
                       "emot_dis_ratio", "n_emotional_dis",
                       "Emotion disorders in CYP - Need:demand ratio")

scatter_function_ratio("cond_dis_count_sum", "cond_dis_denom_sum", "cond_dis_value",
                       "cond_ratio", "n_conduct_dis",
                       "Conduct disorders in CYP - Need:demand ratio")

scatter_function_ratio("hyperkin_dis_count_sum", "hyperkin_dis_denom_sum", "hyperkin_dis_value",
                       "hyperkin_ratio", "n_hyperkin_dis",
                       "Hyperkinetic disorders in CYP - Need:demand ratio")

scatter_function_ratio("self_harm_count_sum", "self_harm_denom_sum", "self_harm_value",
                       "self_harm_ratio", "n_self_harm",
                       "Self-harm in CYP - Need:demand ratio") # Add details here around prevalence value (rate per ?)


# CCG need:demand ratio
scatter_function_ratio_ccg <- function(var_need_count, var_need_denom, var_need_value,
                                   var_ratio, var_demand_count,
                                   title) {

  sub_data <-
    CCG_need_summary %>%
    select(ccg20cd, ccg20nm, var_need_count, var_need_denom, var_need_value) %>%
    left_join(CCG_need_demand %>% select(-ccg20nm, -var_need_count, -var_need_denom, -var_need_value), by = c("ccg20cd")) %>%
    ungroup() %>%
    select(1:5, var_ratio, var_demand_count, midlands_flag) %>%
    rename(need_count = var_need_count,
           need_denom = var_need_denom,
           need_value = var_need_value,
           demand_count = var_demand_count,
           ratio = var_ratio)  %>%
    mutate(ratio = -ratio)

  grids <-
    sub_data %>%
    summarise(min_prev = min(need_value),
              max_prev = max(need_value),
              mean_prev = sum(need_count)/sum(need_denom)*100,
              min_ratio = min(ratio),
              max_ratio = max(ratio),
              mean_ratio = sum(demand_count)/ sum(need_count)
    ) %>%
    mutate(mean_ratio = -mean_ratio)

  sub_data %>%
    ggplot(aes(x = need_value, y = ratio )) +
    annotate("rect", xmin = grids$min_prev, xmax = grids$mean_prev, ymin = grids$min_ratio, ymax = grids$mean_ratio, fill= "#abdda4", alpha = 0.8) + #BL
    annotate("rect", xmin = grids$min_prev, xmax = grids$mean_prev, ymin = grids$mean_ratio, ymax = grids$max_ratio, fill= "#ffed6f", alpha = 0.8) +
    annotate("rect", xmin = grids$mean_prev, xmax = grids$max_prev, ymin = grids$min_ratio, ymax = grids$mean_ratio, fill= "#ffed6f", alpha = 0.8) +
    annotate("rect", xmin = grids$mean_prev, xmax = grids$max_prev, ymin = grids$mean_ratio, ymax = grids$max_ratio, fill= "#d73027", alpha = 0.8) + #TR

    annotate("text", x = (grids$min_prev*0.99), y = (grids$max_ratio*1.75), label = "High unmet need",
             size = 4, angle = 90) +
    annotate("text", x = (grids$min_prev*0.99), y = (grids$min_ratio*0.95), label = "Low unmet need",
             size = 4, angle = 90) +

    geom_point(data = sub_data %>% filter(midlands_flag != "Midlands"), size = 6, shape = 21, fill = "grey", colour = "black") +
    geom_point(data = sub_data %>% filter(midlands_flag == "Midlands"), size = 8, shape = 21, fill = "#f9bf07", colour = "black") +
    geom_label_repel(data = sub_data %>% filter(midlands_flag == "Midlands"),
                     aes(label = stringr::str_wrap(ccg20nm,15)), box.padding = 1,
                     max.overlaps = getOption("ggrepel.max.overlaps", default = Inf)) +
    scale_y_continuous(
      breaks = seq(from = 0, to = -1, by = -0.25), labels = c("0", "0.25", "0.50", "0.75", "1")
    ) +
    theme(axis.text.y = element_blank()) +
    labs(title = title,
         fill = "CCG Name",
         x = "Prevalence",
         y = "Need:Demand ratio")
  }

scatter_function_ratio_ccg("emot_dis_count_sum", "emot_dis_denom_sum", "emot_dis_value",
                           "emot_dis_ratio", "n_emotional_dis",
                           "Emotion disorders in CYP - Need:demand ratio")

scatter_function_ratio_ccg("cond_dis_count_sum", "cond_dis_denom_sum", "cond_dis_value",
                           "cond_ratio", "n_conduct_dis",
                           "Conduct disorders in CYP - Need:demand ratio")

scatter_function_ratio_ccg("hyperkin_dis_count_sum", "hyperkin_dis_denom_sum", "hyperkin_dis_value",
                           "hyperkin_ratio", "n_hyperkin_dis",
                           "Hyperkinetic disorders in CYP - Need:demand ratio")

scatter_function_ratio_ccg("self_harm_count_sum", "self_harm_denom_sum", "self_harm_value",
                           "self_harm_ratio", "n_self_harm",
                           "Self-harm in CYP - Need:demand ratio")


# Plot relative vs absolute values for CYP MH indicators

# STP
scatter_function <- function(field_count, field_value, title) {

STP_need_summary %>%
  select(1,2, field_count, field_value) %>%
  rename(count = 3,
         value = 4) %>%

  ggplot(aes(x = value, y = count, label = stringr::str_wrap(stp20nm,15))) +
  geom_point(size = 10, colour = "#f9bf07") +
  geom_text_repel(box.padding = 2, check_overlap = TRUE) +
  # Add 2x2 reference grid #######
  labs(title = title,
       fill = "STP Name",
       x = "Value",
       y = "Count")
  }

scatter_function("emot_dis_count_sum", "emot_dis_value", "Count and Prevalence of Emotional disorders by CYP by STP")
scatter_function("cond_dis_count_sum", "cond_dis_value", "Count and Prevalence of Conduct disorders by CYP by STP")
scatter_function("hyperkin_dis_count_sum", "hyperkin_dis_value", "Count and Prevalence of Hyperkinetic disorders by CYP by STP")
scatter_function("self_harm_count_sum", "self_harm_value", "Count and Prevalence of Self harm admissions by CYP by STP") # check this!
scatter_function("adhd_count_sum", "adhd_prev", "Count and Prevalence of ADHD by CYP by STP")
scatter_function("eating_dis_count_sum", "eating_dis_prev", "Count and Prevalence of Eating disorder by CYP by STP") # Check title



# CCG
