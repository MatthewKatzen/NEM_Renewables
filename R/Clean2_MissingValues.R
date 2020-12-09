


### load packages
library(tidyverse)
library(lubridate)
library(data.table)
library(janitor)
library(ggpubr)
Sys.setenv(TZ='UTC')

#Load data


generator_details_AEMO <- fread("D:/Data/RAW/AEMO/Website/generators_and_loads.csv") %>% clean_names() %>% 
  distinct(duid, .keep_all = TRUE) %>% 
  select(duid, classification, technology_type_descriptor, station_name, region) %>% 
  mutate(region = substr(region, 1, nchar(region)-1)) %>% 
  filter(technology_type_descriptor %in% c("Wind - Onshore", "Photovoltaic Flat Panel", "Photovoltaic Tracking  Flat Panel"))


scada <- map(list.files("D:/Data/RAW/AEMO/NEMWEB/UNIT_SCADA/", full.names = T),
             ~fread(.x) %>% clean_names() %>% 
               select(settlementdate, duid, scadavalue) %>% 
               mutate(renewable = (duid %in% generator_details_AEMO$duid))) %>% 
  rbindlist() %>% 
  mutate(settlementdate = ymd_hms(settlementdate))



scada <- scada %>% 
  complete(settlementdate, duid) %>% #add missing vals
  group_by(duid) %>% 
  filter(settlementdate > min(settlementdate[!is.na(scadavalue)]),#remove if before start or after end
         settlementdate < max(settlementdate[!is.na(scadavalue)])) 




#Explore
############################

#time of day
scada %>% filter(is.na(scadavalue)) %>% 
  mutate(time = ymd_hms(paste0("2020/12/08 ",substr(settlementdate, 12,19))), year = nem_year(settlementdate)) %>% 
  left_join(generator_details_AEMO %>% select(duid, technology_type_descriptor), by = "duid") %>% 
  ggplot(aes(x = time)) + 
  geom_histogram() +
  facet_grid(year ~ technology_type_descriptor) +
  ggsave("Output/Missing/Time.png")

#sum of ren output
scada %>% 
  group_by(settlementdate) %>% summarise(sum_output = sum(scadavalue, na.rm = TRUE), 
                                         count_missing = sum(is.na(scadavalue))) %>% 
  ggplot(aes(x = sum_output, y = count_missing)) + 
  geom_point()+
  ggsave("Output/Missing/RenOutput.png")

#table total output 
scada %>% group_by(duid) %>% 
  summarise(output = sum(scadavalue/12, na.rm = TRUE), count_missing = sum(is.na(scadavalue))) %>% 
  arrange(-count_missing)

scada %>% ungroup() %>% 
  summarise(output = sum(scadavalue/12, na.rm = TRUE), missing_obs = sum(is.na(scadavalue)), total_obs = n())

scada %>% group_by(duid) %>%
  mutate(any_missing = any(is.na(scadavalue))) %>% 
  group_by(any_missing) %>% 
  summarise(output = sum(scadavalue/12, na.rm = TRUE), missing_obs = sum(is.na(scadavalue)), total_obs = n())

scada %>% left_join(generator_details_AEMO %>% select(duid, classification), by = "duid") %>% 
  group_by(duid) %>%
  mutate(any_missing = any(is.na(scadavalue))) %>% 
  group_by(any_missing, classification) %>% 
  summarise(output = sum(scadavalue/12, na.rm = TRUE), missing_obs = sum(is.na(scadavalue)), total_obs = n())

scada %>% left_join(generator_details_AEMO %>% select(duid, classification), by = "duid") %>% 
  group_by(duid) %>%
  mutate(any_missing = any(is.na(scadavalue))) %>% ungroup() %>% 
  count(any_missing, classification=="Non-Scheduled")

#sum of total output

scada_2 <- map(list.files("D:/Data/RAW/AEMO/NEMWEB/UNIT_SCADA/", full.names = T),
             ~fread(.x) %>% clean_names() %>% 
               select(settlementdate, duid, scadavalue) %>% 
               mutate(renewable = (duid %in% generator_details_AEMO$duid)) %>% 
               complete(settlementdate, duid) %>% #add missing vals
               group_by(duid) %>% 
               filter(settlementdate > min(settlementdate[!is.na(scadavalue)]),#remove if before start or after end
                      settlementdate < max(settlementdate[!is.na(scadavalue)])) %>% 
               group_by(settlementdate) %>% 
               summarise(sum_output = sum(scadavalue, na.rm = TRUE), 
                         count_missing = sum(is.na(scadavalue)))) %>% 
  rbindlist() %>% 
  mutate(settlementdate = ymd_hms(settlementdate))

scada_2 %>% 
  ggplot(aes(x = sum_output, y = count_missing))+
  geom_point()+
  ggsave("Output/Missing/TotalOutput.png")




#Regression imputation
###############################
matching <- data.frame(missing = missing$duid,
                       c("EMERASF1",
                         "WOODLWN1",
                         "LKBONNY2",
                         "ARWF1"))


full_data %>% mutate(cf = scadavalue/registeredcapacity) %>% 
  filter(region == "QLD", technology_type_descriptor == "Photovoltaic Tracking  Flat Panel") %>% 
  group_by(duid) %>% 
  filter(!any(is.na(scadavalue))| duid == "HUGSF1") %>% #only using duids with no NAs
  select(settlementdate, duid, region, technology_type_descriptor, cf) %>% ungroup() %>% 
  pivot_wider(names_from = duid, values_from = cf) %>% 
  impute_lm(., HUGSF1~ -settlementdate)