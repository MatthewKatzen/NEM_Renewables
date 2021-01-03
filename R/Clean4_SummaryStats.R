#Clean4_SummaryStats

full_data <- fread("D:/Data/Cleaned/Renewables/Renewables_5min_with_non_data_and_details.csv") %>% 
  mutate(year = nem_year(settlementdate))

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

scada <- map(grep("2015|2017|2019", list.files("D:/Data/RAW/AEMO/NEMWEB/UNIT_SCADA/", full.names = T), 
                  value = TRUE),
             ~fread(.x) %>% clean_names() %>% 
               select(settlementdate, duid, scadavalue) %>% 
               filter(duid %in% generator_details_AEMO$duid)) %>% 
  rbindlist() %>% 
  mutate(settlementdate = ymd_hms(settlementdate))

#add missing vals
scada <- scada %>%   
  complete(settlementdate, duid) 

#add year 
scada <- scada %>% mutate(year = nem_year(settlementdate))

#filter out if not producing for entire year
scada <- scada %>%   
  group_by(duid, year) %>%  # only keep if have non-na/non-zero at any point on 1 JAN for each of the years (i.e. produce for whole year)
  filter(any(yday(settlementdate) == 1 & 
               year(settlementdate) == year &
               (!is.na(scadavalue) & scadavalue!=0))) %>% 
  ungroup() 

#neg values
scada <- scada %>% mutate(scadavalue = ifelse(scadavalue<0, 
                                                        0, 
                                                        scadavalue)) %>% ungroup()



#Explore
############################

#sum of ren output
scada %>% 
  group_by(settlementdate) %>% summarise(sum_output = sum(scadavalue, na.rm = TRUE), 
                                         count_missing = sum(is.na(scadavalue))) %>% 
  ggplot(aes(x = sum_output, y = count_missing)) + 
  geom_point()+
  ggsave("Output/Missing/RenOutput.png")

#table total output 
scada %>% group_by(year) %>% 
  summarise(MWh = sum(scadavalue/12, na.rm = TRUE), 
            missing_obs = sum(is.na(scadavalue)), 
            total_obs = n())


scada %>% left_join(generator_details_AEMO %>% select(duid, classification, registeredcapacity), by = "duid") %>% 
  group_by(year, classification) %>% 
  summarise(MWh = sum(scadavalue/12, na.rm = TRUE), 
            missing_obs = sum(is.na(scadavalue)), 
            total_obs = n()) #how much produced by each type


scada %>% left_join(generator_details_AEMO %>% select(duid, fuel_source_descriptor, classification, registeredcapacity), by = "duid") %>% 
  group_by(year, fuel_source_descriptor, classification, registeredcapacity) %>% 
  distinct(duid) %>% 
  group_by(year, fuel_source_descriptor, classification) %>% 
  summarise(total_cap = sum(registeredcapacity))

