## PHE Fingertips indicators - Scatter plot ####

library(ggrepel)

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
