
### load packages
library(tidyverse)
library(lubridate)
library(data.table)
library(janitor)
library(ggpubr)
Sys.setenv(TZ='UTC')

#Merge data
#########################################

generator_details_AEMO <- fread("D:/Data/RAW/AEMO/Website/generators_and_loads.csv") %>% clean_names() %>% 
  distinct(duid, .keep_all = TRUE) %>% 
  select(duid, classification, fuel_source_descriptor, station_name, region) %>% 
  mutate(region = substr(region, 1, nchar(region)-1)) %>% 
  filter(fuel_source_descriptor %in% c("Wind", "Solar"),
         duid != "HPRG1") %>% #for some reaosn Hornsdale battery considered wind powered
  left_join(fread("D:/Data/RAW/AEMO/NEMWEB/DUDETAIL/PUBLIC_DVD_DUDETAIL_201912010000.CSV") %>% #add cap
              clean_names() %>% 
              group_by(duid) %>% 
              filter(effectivedate == max(effectivedate)) %>% 
              select(duid, registeredcapacity), 
            by = "duid") 

#scada data
scada <- map(list.files("D:/Data/RAW/AEMO/NEMWEB/UNIT_SCADA/", full.names = T),
             ~fread(.x) %>% clean_names() %>% 
               select(settlementdate, duid, scadavalue) %>% 
               filter(duid %in% generator_details_AEMO$duid)) %>% 
  rbindlist() %>% 
  mutate(settlementdate = ymd_hms(settlementdate))

#rrp
rrp <-  map(list.files("D:/Data/RAW/AEMO/NEMWEB/DISPATCHPRICE/", full.names = T),
            ~fread(.x) %>% clean_names() %>% 
              filter(intervention == 0) %>% 
              select(settlementdate, rrp, regionid)) %>%  
  rbindlist() %>% 
  mutate(settlementdate = ymd_hms(settlementdate)) %>% 
  group_by(interval = ceiling_date(settlementdate, "30 min"), regionid) %>% 
  mutate(rrp30 = mean(rrp),
         region = substr(regionid, 1, nchar(regionid)-1)) %>% 
  ungroup() %>% 
  select(-regionid, -interval)



# CLEAN and IMPUTATION
# RUN CLEAN2_MissingValues
########################################


#merge
#########################
full_data <- scada_imputed %>% 
  left_join(generator_details_AEMO %>% select(-classification, -station_name), by = "duid") %>% 
  left_join(rrp, by = c("region", "settlementdate"))

fwrite(full_data, "D:/Data/Cleaned/Renewables/Renewables_5min_with_non_data_ungrouped.csv")

#

full_data <- fread("D:/Data/Cleaned/Renewables/Renewables_5min_with_non_data_ungrouped.csv") %>% 
  mutate(settlementdate = ymd_hms(settlementdate)) %>% 
  mutate(registeredcapacity = fifelse(duid == "WATERLWF" & #only WATERLWF changed capacity bw '15-19
                                        ymd_hms(settlementdate) < ymd_hms("2016/10/29 00:00:00 UTC"),
                                      111,
                                      registeredcapacity))



# Aggregate cleaning
###################################
full_data <- full_data %>% ungroup() %>% 
  mutate(duid2 = fcase(duid == "BNGSF2", "BNGSF1", #merge all gens with multiple duids
                          duid == "DAYDSF2", "DAYDSF1",
                          duid == "DUNDWF2", "DUNDWF1",
                          duid == "DUNDWF3", "DUNDWF1",
                          duid == "GULLRWF2", "GULLRWF1",
                          duid == "HALLWF2", "HALLWF1",
                          duid == "HDWF2", "HDWF1",
                          duid == "HDWF3", "HDWF1",
                          duid == "LKBONNY2", "LKBONNY1",
                          duid == "LKBONNY3", "LKBONNY1",
                          duid == "LIMOSF21", "LIMOSF11",
                          duid == "NUMURSF2", "NUMURSF1",
                          duid == "OAKEY2SF", "OAKEY1SF",
                          duid == "SNOWNTH1", "SNOWSTH1",
                          duid == "SNOWTWN1", "SNOWSTH1",
                          default = NA)) %>% 
  mutate(duid = fifelse(is.na(duid2),
                        duid,
                        duid2)) %>% select(-duid2) %>% data.frame()

fwrite(full_data, "D:/Data/Cleaned/Renewables/Renewables_5min_with_non_data.csv")

# SPlit back into years

full_data_temp <- fread("D:/Data/Cleaned/Renewables/Renewables_5min_with_non_data.csv", 
                   select = c("settlementdate", "duid", "registeredcapacity", "scadavalue", "rrp30")) %>% 
  filter(nem_year(settlementdate) == 2019)

full_data_temp <- full_data_temp %>% 
  group_by(settlementdate, duid) %>% 
  summarise(scadavalue = sum(scadavalue),
            registeredcapacity = sum(registeredcapacity),
            rrp30 = rrp30[1]) %>% #all rrp30s are the same
  ungroup() %>% 
  mutate(cf = scadavalue/registeredcapacity,#create cf variable
         rev_mw = scadavalue/12*rrp30*(1/registeredcapacity))  #create rev per MW of cap variable 

fwrite(full_data_temp, "D:/Data/Cleaned/Renewables/Renewables_5min_with_non_data_2019.csv")

#merge years together AGAIN

full_data <- rbind(fread("D:/Data/Cleaned/Renewables/Renewables_5min_with_non_data_2015.csv"),
                   fread("D:/Data/Cleaned/Renewables/Renewables_5min_with_non_data_2017.csv"),
                   fread("D:/Data/Cleaned/Renewables/Renewables_5min_with_non_data_2019.csv"))

fwrite(full_data, "D:/Data/Cleaned/Renewables/Renewables_5min_with_non_data.csv")


# Aggregate cleaning 30min level INCOMPLETE
###################################
full_data <- fread("D:/Data/Cleaned/Renewables/Renewables_5min_with_non_data.csv")
  
full_data <- full_data %>%
  mutate(settlementdate = ceiling_date(settlementdate, "30 min")) %>% #convert to 30 min
  group_by(settlementdate, duid, registeredcapacity) %>%
  summarise(scadavalue = sum(scadavalue),
            rev_mw = sum(rev_mw),
            cf = mean(cf))

fwrite(full_data, "D:/Data/Cleaned/Renewables/Renewables_30min_with_non_data.csv")


  

