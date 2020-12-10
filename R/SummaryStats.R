
### load packages
library(tidyverse)
library(lubridate)
library(data.table)
library(janitor)
library(ggpubr)
Sys.setenv(TZ='UTC')

#Load data
full_data <- fread("D:/Data/Cleaned/Renewables/Renewables_5min_data.csv") %>% 
  mutate(settlementdate = ymd_hms(settlementdate)) %>% as_tibble() 

# SUMMARY STATS
###########################

#how many of each type?
full_data %>% select(duid, technology_type_descriptor) %>% unique() %>% 
  count(technology_type_descriptor)

#cap of each
full_data %>% select(technology_type_descriptor, registeredcapacity, region) %>% unique() %>% 
  group_by(region, technology_type_descriptor) %>%  summarise(sum(registeredcapacity), n())

# how much generated?
full_data %>% group_by(technology_type_descriptor)  %>% 
  summarise(sum(mwh))

#total generated
generator_details_AEMO <- fread("D:/Data/RAW/AEMO/Website/generators_and_loads.csv") %>% clean_names() %>% 
  distinct(duid, .keep_all = TRUE) %>% 
  select(duid, dispatch_type, technology_type_descriptor, station_name, region) %>% 
  mutate(region = substr(region, 1, nchar(region)-1)) %>% 
  filter(dispatch_type == "Generator")

scada <- scada %>% 
  group_by(duid, year = mk_year(settlementdate)) %>%  
  filter(any(yday(settlementdate) == 1 & 
               mk_year(settlementdate) == year(settlementdate))) %>%  #produce at any point on 1 JAN for each of the years
  ungroup() %>% select(-year)


# E[output] graphs Dispactable only 


