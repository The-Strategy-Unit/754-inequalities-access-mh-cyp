## Local authority and Unitary authority to CCG look up
library(tidyverse)
library(sf)

## Import geo-data ####
la_to_ccg <- "https://opendata.arcgis.com/datasets/f0b39d6872dd4533aaf3941846134a89_0.csv" %>%
  read_csv(col_types = "dccccccc")
lsoa_pop <- "https://gist.githubusercontent.com/tomjemmett/917d3a134b38218546ee3df4d43812c9/raw/081a0421fe2b578dea3009fbdde69652306af322/lsoa_pop.csv" %>%
  read_csv(col_types = "cd")

county_uas <- st_read("https://opendata.arcgis.com/datasets/b216b4c8a4e74f6fb692a1785255d777_0.geojson") #Most recent available
la <- st_read("https://opendata.arcgis.com/datasets/db23041df155451b9a703494854c18c4_0.geojson")
stp <- st_read("https://opendata.arcgis.com/datasets/24ebc8b4ff774c98b624072a898490bc_0.geojson")
nhse_regions <- st_read("https://opendata.arcgis.com/datasets/5e33fd1436114a19b335ed07076a4c5b_0.geojson")
ccg <- st_read("https://ons-inspire.esriuk.com/arcgis/rest/services/Health_Boundaries/Clinical_Commissioning_Groups_April_2020_EN_BUC/MapServer/1/query?outFields=*&where=1%3D1&f=geojson")

## Population derived look ups ####
# LA/UA -> CCG
la_to_ccg_lookup <-
  la_to_ccg %>%
  inner_join(lsoa_pop, by = "LSOA11CD") %>%
  group_by(LAD20CD, CCG20CD) %>%
  summarise(across(pop, sum), .groups = "drop_last") %>%
  mutate(pcnt = pop / sum(pop)) %>%
  filter(pcnt == max(pcnt))

## CCG -> LA/UA
#ccg_to_la_lookup <-
#la_to_ccg %>%
#  inner_join(lsoa_pop, by = "LSOA11CD") %>%
#  group_by(CCG20CD, LAD20CD) %>%
#  summarise(across(pop, sum), .groups = "drop_last") %>%
#  mutate(pcnt = pop / sum(pop))

## Land derived lookups ####
## Local authority to CCG look up - land derived
#LA_CCG_lookup_land <-
#  la %>%
#  st_transform(crs = 27700) %>%
#  select(LAD20CD, LAD20NM) %>%
#  st_set_agr(c("LAD20CD" = "identity", "LAD20NM", "identity")) %>%
#  st_intersection(ccg %>%
#                    select(CCG19CD, CCG19NM) %>%
#                    st_set_agr(c("CCG19NM" = "identity", "CCG19CD" = "identity")) %>%
#                    st_transform(crs = 27700)) %>%
#  group_by(LAD19NM) %>%
#  mutate(area = as.numeric(st_area(geometry))) %>%
#  filter(area >= 0.01 * sum(area)) %>%
#  mutate(pcnt = area / sum(area)) %>%
#  st_transform(crs = 4326)


## County and Unitary authorities to CCG - land derived
UA_CCG_lookup <-
  as_tibble(
    county_uas %>%
      st_transform(crs = 27700) %>%
      select(ctyua19cd, ctyua19nm) %>%
      st_set_agr(c("ctyua19cd" = "identity", "ctyua19nm", "identity")) %>%
      st_intersection(ccg %>%
                        select(ccg20cd, ccg20nm) %>%
                        st_set_agr(c("ccg20cd" = "identity", "ccg20nm" = "identity")) %>%
                        st_transform(crs = 27700)) %>%
      group_by(ctyua19nm) %>%
      mutate(area = as.numeric(st_area(geometry))) %>%
      filter(area >= 0.01 * sum(area)) %>%
      mutate(pcnt = area / sum(area)) %>%
      st_transform(crs = 4326) %>%
      filter(pcnt == max(pcnt))
  ) %>%
  select(-geometry)


