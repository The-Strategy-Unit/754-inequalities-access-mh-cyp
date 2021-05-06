# Local authorities successors

library(janitor)

# Import Local authority lists by year

la_2020 <- "https://opendata.arcgis.com/datasets/fe6bcee87d95476abc84e194fe088abb_0.csv" %>% read_csv() %>% clean_names() %>% select(2,3)
la_2019 <- "https://opendata.arcgis.com/datasets/c3ddcd23a15c4d7985d8b36f1344b1db_0.csv" %>% read_csv() %>% clean_names() %>% select(2,3)
la_2018 <- "https://opendata.arcgis.com/datasets/17eb563791b648f9a7025ca408bb09c6_0.csv" %>% read_csv() %>% clean_names() %>% select(1,2)
la_2017 <- "https://opendata.arcgis.com/datasets/a267b55f601a4319a9955b0197e3cb81_0.csv" %>% read_csv() %>% clean_names() %>% select(1,2)
la_2016 <- "https://opendata.arcgis.com/datasets/464be6191a434a91a5fa2f52c7433333_0.csv" %>% read_csv() %>% clean_names() %>% select(1,3)
la_2015 <- "https://opendata.arcgis.com/datasets/8edafbe3276d4b56aec60991cbddda50_0.csv?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D" %>%
  read_csv() %>% clean_names() %>% select(2,3)

# Identify changes in LA structures
la_2015 %>%
  anti_join(la_2016, by = c("lad15cd" = "lad16cd")) # 0

la_2016 %>%
  anti_join(la_2017 , by = c("lad16cd" = "lad17cd")) # 0

la_2017 %>%
  anti_join(la_2018 , by = c("lad17cd" = "lad18cd")) # 2 changes/mis-matches - Fife + Perth and Kinross

la_2018 %>%
  anti_join(la_2019 , by = c("lad18cd" = "lad19cd")) # 16 changes/mis-matches


la_2019 %>%
  anti_join(la_2020 , by = c("lad19cd" = "lad20cd")) # 4 changes/mis-matches


# Import summary of changes to LA codes 2017-2020
la_successors <-
read_csv("C:/Users/alexander.lawless/OneDrive - Midlands and Lancashire CSU/Data_management/Local_authority_structural_changes_2017_20.csv") %>%
  select(1:8) %>%
  clean_names() %>%
  mutate(old_code = case_when(!is.na(lad17cd) ~ lad17cd,
                              !is.na(lad18cd) ~ lad18cd,
                              TRUE ~ lad19cd))

la_successors %>%
  mutate(lad20nm = str_replace(lad20nm, 'xa0','_')) ##### Fix wierd CSV import issue (\xa0 instead of spaces) ########## ?




