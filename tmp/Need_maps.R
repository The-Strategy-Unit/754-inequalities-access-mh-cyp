## Map PHE Fingertips indicators ####

library(sf)
library(leaflet)
library(patchwork)
library(grid)
library(gridExtra)

# Import shape files
stps <- st_read("https://opendata.arcgis.com/datasets/24ebc8b4ff774c98b624072a898490bc_0.geojson")
nhse_regions <- st_read("https://opendata.arcgis.com/datasets/719d6d66a5fa4765aa4dc582571010ed_0.geojson")
ccgs <- st_read("https://opendata.arcgis.com/datasets/8e410b3299154bb3a93fae1a69ed4f2b_1.geojson")
county_uas <- st_read("https://opendata.arcgis.com/datasets/b216b4c8a4e74f6fb692a1785255d777_0.geojson")
la <- st_read("https://opendata.arcgis.com/datasets/db23041df155451b9a703494854c18c4_0.geojson")


## Flat maps ####
# PHE indicators by STP

# National and regional
stp_map_funct_nat_reg <- function(indicator, indicator_desc) {

  sub_data <-
    stps %>%
    inner_join(STP_need_demand, by = c("stp20cd")) %>%
    select(1:9, midlands_flag, indicator, geometry) %>%
    rename(value = indicator)

  national <-
    sub_data %>%
    ggplot() +
    geom_sf(aes(fill = value)) +
    geom_sf(data = nhse_regions %>% filter(nhser20nm == "Midlands"), fill = NA, colour = "black", size = 1.2) +
    scale_fill_distiller(type = "div", palette = "Spectral", direction = 1) +
    theme(axis.text = element_blank(),
          panel.grid.major = element_blank(),
          plot.title = element_text(hjust = 0.5))+
    labs(x = "", y = "",
         title = "National",
         subtitle = "",
         caption = "",
         fill = "Need:demand ratio")

  midlands <-
    sub_data %>%
    ggplot() +
    geom_sf(data = nhse_regions %>% filter(nhser20nm == "Midlands"), fill = NA, colour = "black") +
    geom_sf(data = sub_data %>%  filter(midlands_flag == "Midlands"), aes(fill = value)) +
    #geom_sf_label(data = sub_data %>%  filter(midlands_flag == "Midlands"),
    #             aes(label = stringr::str_wrap(stp20nm,15)),
    #             size = 3, label.padding = unit(0.15, "lines")) +
    ggrepel::geom_label_repel(
      data = sub_data %>%  filter(midlands_flag == "Midlands"),
      aes(label = stringr::str_wrap(stp20nm,15), geometry = geometry),
      size = 3,
      stat = "sf_coordinates" ,
      max.overlaps = getOption("ggrepel.max.overlaps", default = Inf),
      box.padding = 5,
      min.segment.length = 0) +
    scale_fill_distiller(type = "div", palette = "Spectral", direction = 1, limits = c(min(sub_data$value), max(sub_data$value))) +
    theme(axis.text = element_blank(),
          panel.grid.major = element_blank(),
          plot.title = element_text(hjust = 0.5))+
    labs(x = "", y = "",
         title = "NHSE Midlands region",
         subtitle = "",
         caption = "",
         fill = "Need:demand ratio")


  # Patch plot together
  national / midlands +
    plot_annotation(title = indicator_desc)
  }


stp_map_funct_nat_reg("emot_dis_ratio", "Emotional disorders in CYP - Need:demand ratio")
stp_map_funct_nat_reg("cond_ratio", "Conduct disorders in CYP - Need:demand ratio")
stp_map_funct_nat_reg("hyperkin_ratio", "Hyperkinetic disorders in CYP - Need:demand ratio")
stp_map_funct_nat_reg("self_harm_ratio", "Self harm in CYP - Need:demand ratio")

# National only
stp_map_funct_nat <- function(indicator, indicator_desc) {

  sub_data <-
    stps %>%
    inner_join(STP_need_demand, by = c("stp20cd")) %>%
    select(1:9, midlands_flag, indicator, geometry) %>%
    rename(value = indicator)

  key <-
    tableGrob(
      as_tibble(
        sub_data %>%
          filter(midlands_flag == "Midlands") %>%
          mutate(Label = rank(bng_e)) %>%
          select(Label, stp20nm) %>%
          rename(STP = stp20nm)) %>%
        select(-geometry) %>%
        arrange(Label),
      rows = NULL,
      theme = ttheme_default(
        base_size = 10,
        core=list(bg_params = list(fill = NA , col=NA),
                  fg_params=list(fontface=3)),
        colhead=list(fg_params=list(col="#2c2825", fontface=4L))
      )
    )

  national <-
    sub_data %>%
    ggplot() +
    geom_sf(aes(fill = value)) +
    geom_sf(data = nhse_regions %>% filter(nhser20nm == "Midlands"), fill = NA, colour = "black", size = 1.2) +
    #ggrepel::geom_label_repel(
    #  data = sub_data %>%  filter(midlands_flag == "Midlands"),
    #  aes(label = stringr::str_wrap(stp20nm,15), geometry = geometry),
    #  stat = "sf_coordinates" ,
    #  max.overlaps = getOption("ggrepel.max.overlaps", default = Inf),
    #  box.padding = 2 #, min.segment.length = 0
    #) +
    geom_sf_label(data = sub_data %>%
                    filter(midlands_flag == "Midlands") %>%
                    mutate(label = rank(bng_e)),
                  aes(label = label), alpha = 0.2) +

    scale_fill_distiller(type = "div", palette = "Spectral", direction = 1) +
    theme(axis.text = element_blank(),
          panel.grid.major = element_blank(),
          plot.title = element_text(hjust = 0.5))+
    labs(x = "", y = "",
         fill = "Need:Demand ratio")

  # Plot with Patchwork
  national +
    key +
    plot_layout(ncol=2,widths=c(2,1)) +
    plot_annotation(title = indicator_desc)

  }

stp_map_funct_nat("emot_dis_ratio", "Emotional disorders in CYP - Need:demand ratio")
stp_map_funct_nat("cond_ratio", "Conduct disorders in CYP - Need:demand ratio")
stp_map_funct_nat("hyperkin_ratio", "Hyperkinetic disorders in CYP - Need:demand ratio")
stp_map_funct_nat("self_harm_ratio", "Self harm in CYP - Need:demand ratio")



# PHE indicators by CCG
ccg_map <-
ccgs %>%
  inner_join(CCG_indicators_CCG_summary, by = c("ccg20cd")) %>%
  clean_names()

stp_map <-
stps %>%
  inner_join(STP_need_summary, by = c("stp20cd"))

ccg_map %>%
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

## Interactive maps ####
# Leaflet exploration

palette <- leaflet::colorNumeric(viridis::viridis(5), range(ccg_map$emot_dis_value))

leaflet() %>%
  #addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = ccg_map,
              weight = 1,
              color = "white",
              fillColor = ~palette(emot_dis_value),
              fillOpacity = 0.7,
              popup = ~ccg20nm_x,
              highlight = highlightOptions(weight = 3,
                                           #colour = "#666",
                                           bringToFront = TRUE,
                                           fillOpacity = 1
                                           )) %>%
  addPolylines(data = stp_map, color = "black", weight = 2)



create_map <- function(data, column) {
  data <- select(data, ccg20nm_x, col = {{ column }})

  palette <- leaflet::colorNumeric(viridis::viridis(5), range(data$col))

  leaflet() %>%
    #addTiles() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(data = data,
                weight = 1,
                color = "black",
                fillColor = ~palette(col),
                fillOpacity = 0.7,
                popup = ~ccg20nm_x,
                highlight = highlightOptions(weight = 3,
                                             #colour = "#666",
                                             bringToFront = TRUE,
                                             fillOpacity = 1
                ))
}

create_map(ccg_map, emot_dis_value)
create_map(ccg_map, cond_dis_value)
create_map(ccg_map, hyperkin_dis_value)
create_map(ccg_map, self_harm_value)
create_map(ccg_map, eating_dis_prev)
create_map(ccg_map, l_ac_prev)
create_map(ccg_map, childen_mh_prev)

