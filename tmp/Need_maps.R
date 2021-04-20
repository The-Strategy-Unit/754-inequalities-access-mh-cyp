## Map PHE Fingertips indicators ####

library(sf)

# Import shape files
stps <- st_read("https://opendata.arcgis.com/datasets/24ebc8b4ff774c98b624072a898490bc_0.geojson")
nhse_regions <- st_read("https://opendata.arcgis.com/datasets/719d6d66a5fa4765aa4dc582571010ed_0.geojson")
ccgs <- st_read("https://opendata.arcgis.com/datasets/b30f6887746b4eff97d86866c9395082_1.geojson")


# PHE indicators by STP
stp_map_funct <- function(indicator, label) {
stps %>%
  inner_join(CCG_STP_summary, by = c("stp20cd")) %>%
    select(1:9, indicator, geometry) %>%
    rename(value = indicator) %>%

  ggplot() +
  geom_sf(data = nhse_regions %>% filter(nhser20nm == "Midlands"), fill = NA, colour = "black") +
  geom_sf(aes(fill = value)) +
  geom_sf_text(aes(label = stringr::str_wrap(stp20nm.x,15))) +
  scale_fill_distiller(type = "div", palette = "Spectral") +
  theme(axis.text = element_blank(),
        panel.grid.major = element_blank())+
  labs(x = "", y = "",
       title = "",
       subtitle = "",
       caption = "",
       fill = label)
}

################ Switch out CCG_STP_summary for STP_need_summary
#stps %>%
#  inner_join(STP_need_summary, by = c("stp20cd")) %>%
#  select(1:9, indicator, geometry) %>%
#  rename(value = indicator) %>%
#
#  ggplot() +
#  geom_sf(data = nhse_regions %>% filter(nhser20nm == "Midlands"), fill = NA, colour = "black") +
#  geom_sf(aes(fill = value)) +
#  geom_sf_text(aes(label = stringr::str_wrap(stp20nm.x,15))) +
#  scale_fill_distiller(type = "div", palette = "Spectral") +
#  theme(axis.text = element_blank(),
#        panel.grid.major = element_blank())+
#  labs(x = "", y = "",
#       title = "",
#       subtitle = "",
#       caption = "",
#       fill = label)

stp_map_funct("Emot_dis_value", 'Emotional disorder prevalence (%)')
stp_map_funct("Cond_dis_value", 'Conduct disorder prevalence (%)')
stp_map_funct("Hyperkin_dis_value", 'Hyperkinetic disorder prevalence (%)')
stp_map_funct("self_harm_value", 'Self harm prevalence (%)')



# PHE indicators by CCG
ccgs %>%
  inner_join(CCG_CCG_summary, by = c("ccg20cd")) %>%

  ggplot() +
  geom_sf(data = nhse_regions %>% filter(nhser20nm == "Midlands"), fill = NA, colour = "black") +
  geom_sf(aes(fill = Emot_dis_value)) +
  #geom_sf_text(aes(label = ccg20nm.x)) +
  scale_fill_distiller(type = "div", palette = "Spectral") +
  theme(axis.text = element_blank(),
        panel.grid.major = element_blank())+
  labs(x = "", y = "",
       title = "",
       subtitle = "",
       caption = "",
       fill = 'Emotional disorder prevalence (%)')


