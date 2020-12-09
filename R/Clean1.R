
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
  select(duid, classification, technology_type_descriptor, station_name, region) %>% 
  mutate(region = substr(region, 1, nchar(region)-1)) %>% 
  filter(technology_type_descriptor %in% c("Wind - Onshore", "Photovoltaic Flat Panel", "Photovoltaic Tracking  Flat Panel"))



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


#capacities
#only WATERLWF increased capacity during timeframe
capacities <- fread("D:/Data/RAW/AEMO/NEMWEB/DUDETAIL/PUBLIC_DVD_DUDETAIL_201912010000.CSV") %>% 
  clean_names() %>% 
  group_by(duid) %>% 
  filter(effectivedate == max(effectivedate)) %>% 
  select(duid, registeredcapacity)  
  #distinct(duid, .keep_all = TRUE) 

# CLEAN
#########################


#Remove if not generating from start of year 
scada <- scada %>% 
  group_by(duid, year = mk_year(settlementdate)) %>%  
  filter(any(yday(settlementdate) == 1 & 
               mk_year(settlementdate) == year(settlementdate))) %>%  #produce at any point on 1 JAN for each of the years
  ungroup() %>% select(-year)

#insert missing scada observations INCOMPLETE
scada <- scada %>% 
  group_by(year = nem_year(settlementdate)) %>% 
  complete(settlementdate, duid) %>% 
  ungroup %>% select(-year)

#neg values
scada <- scada %>% mutate(scadavalue = ifelse(scadavalue<0, 
                                              0, 
                                              scadavalue))


#merge
#########################
full_data <- generator_details_AEMO %>% left_join(capacities, by = "duid") %>% 
  right_join(scada, by = "duid") %>% 
  left_join(rrp, by = c("region", "settlementdate"))


#WATERLWF changed capacity
##########################
full_data <- full_data %>% mutate(registeredcapacity = ifelse(duid == "WATERLWF" & 
                                                                ymd_hms(settlementdate) < ymd_hms("2016/10/29 00:00:00 UTC"),
                                                         111,
                                                         registeredcapacity))
  





#missing data regression imputation INCOMPLETE
############################



#Create CF and Rev variables
##############################
full_data <- full_data %>% ungroup() %>% 
  mutate(cf = scadavalue/registeredcapacity,#create cf variable
         rev_mw = scadavalue/12*rrp30*(1/registeredcapacity))
  

# Aggregate cleaning ONLY SEMI 
###################################
full_data <- full_data %>% ungroup() %>% filter(classification == "Semi-Scheduled") %>% #only keep semi
  mutate(duid = case_when(duid == "BNGSF2" ~ "BNGSF1", #merge all gens with multiple duids
                          duid == "DAYDSF2" ~ "DAYDSF1",
                          duid == "DUNDWF2" ~ "DUNDWF1",
                          duid == "DUNDWF3" ~ "DUNDWF1",
                          duid == "GULLRWF2" ~ "GULLRWF1",
                          duid == "HALLWF2" ~ "HALLWF1",
                          duid == "HDWF2" ~ "HDWF1",
                          duid == "HDWF3" ~ "HDWF1",
                          duid == "LKBONNY2" ~ "LKBONNY1",
                          duid == "LKBONNY3" ~ "LKBONNY1",
                          duid == "LIMOSF21" ~ "LIMOSF11",
                          duid == "NUMURSF2" ~ "NUMURSF1",
                          duid == "OAKEY2SF" ~ "OAKEY1SF",
                          duid == "SNOWNTH1" ~ "SNOWSTH1",
                          duid == "SNOWTWN1" ~ "SNOWSTH1",
                          TRUE ~ duid)) %>% 
  group_by(settlementdate, duid, technology_type_descriptor, rrp30) %>% 
  summarise(scadavalue = sum(scadavalue),
            mwh = sum(scadavalue)/12,
            registeredcapacity = sum(registeredcapacity)) %>% 
  ungroup() %>% 
  mutate(cf = scadavalue/registeredcapacity,#create cf variable
         rev_mw = mwh*rrp30*(1/registeredcapacity))  #create rev per MW of cap variable 

fwrite(full_data, "D:/Data/Cleaned/Renewables/Renewables_5min_data.csv")


# Aggregate cleaning INCOMPLETE 
###################################
full_data <- full_data %>% ungroup() %>% 
  mutate(duid = case_when(duid == "BNGSF2" ~ "BNGSF1", #merge all gens with multiple duids
                          duid == "DAYDSF2" ~ "DAYDSF1",
                          duid == "DUNDWF2" ~ "DUNDWF1",
                          duid == "DUNDWF3" ~ "DUNDWF1",
                          duid == "GULLRWF2" ~ "GULLRWF1",
                          duid == "HALLWF2" ~ "HALLWF1",
                          duid == "HDWF2" ~ "HDWF1",
                          duid == "HDWF3" ~ "HDWF1",
                          duid == "LKBONNY2" ~ "LKBONNY1",
                          duid == "LKBONNY3" ~ "LKBONNY1",
                          duid == "LIMOSF21" ~ "LIMOSF11",
                          duid == "NUMURSF2" ~ "NUMURSF1",
                          duid == "OAKEY2SF" ~ "OAKEY1SF",
                          duid == "SNOWNTH1" ~ "SNOWSTH1",
                          duid == "SNOWTWN1" ~ "SNOWSTH1",
                          TRUE ~ duid)) %>% 
  group_by(settlementdate, duid, technology_type_descriptor, rrp30) %>% 
  summarise(scadavalue = sum(scadavalue),
            mwh = sum(scadavalue)/12,
            registeredcapacity = sum(registeredcapacity)) %>% 
  ungroup() %>% 
  mutate(cf = scadavalue/registeredcapacity,#create cf variable
         rev_mw = mwh*rrp30*(1/registeredcapacity)) #%>%  #create rev per MW of cap variable 
  #mutate(settlementdate = ceiling_date(settlementdate, "hour")) %>% #convert to hourly
  #group_by(settlementdate, duid, technology_type_descriptor) %>%
  #summarise(mwh = sum(mwh),
  #         rev = sum(rev),
  #         cf = mean(cf),
  #         registeredcapacity = mean(registeredcapacity)) #some scada values are missing, this takes mean of cap over hour


# SAVE
###########################
  

