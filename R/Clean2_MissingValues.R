
### load packages
library(tidyverse)
library(lubridate)
library(data.table)
library(janitor)
library(ggpubr)
library(geosphere)
library(magrittr)
Sys.setenv(TZ='UTC')

#Load data from Clean1.R


#Missing Vals
###############################
year <- 2019 #just rerun for 

scada_year <- scada %>% 
  filter(nem_year(settlementdate) == 2019)

#add missing vals
scada_year <- scada_year %>%   
  complete(settlementdate, duid) 

#filter out if not producing for entire year
scada_year <- scada_year %>%   
  mutate(year = nem_year(settlementdate)) %>% 
  group_by(duid, year) %>%  # only keep if have non-na/non-zero at any point on 1 JAN for each of the years (i.e. produce for whole year)
  filter(any(yday(settlementdate) == 1 & 
               year == year(settlementdate) &
               (!is.na(scadavalue) & scadavalue!=0))) %>% 
  ungroup() %>% select(-year)

#neg values
scada_year <- scada_year %>% mutate(scadavalue = ifelse(scadavalue<0, 
                                              0, 
                                              scadavalue)) %>% ungroup()

#IMPUTATION
#######################

gens <- scada_year %>% select(duid) %>% unique() %>% .[["duid"]]

gen_latlons <- fread("D:/Data/RAW/AEMO/NEMWEB/DUDETAILSUMMARY/PUBLIC_DVD_DUDETAILSUMMARY_201912010000.CSV") %>% clean_names() %>% 
                                                   select(duid, stationid) %>% 
                                                   unique() %>% 
  left_join(fread("D:/Data/RAW/Tom/stations-2.csv") %>% select(stationid = code, latitude, longitude), by = "stationid") %>% 
  select(-stationid) %>% 
  filter(duid %in% gens) %>% 
  left_join(generator_details_AEMO %>% select(duid, classification, fuel_source_descriptor), by = "duid")

#fuel_type matrix
source_match <- (gen_latlons$fuel_source_descriptor == "Wind") %*% t(gen_latlons$fuel_source_descriptor == "Wind") +
  (gen_latlons$fuel_source_descriptor == "Solar") %*% t(gen_latlons$fuel_source_descriptor == "Solar") %>% 
  data.frame() %>% 
  set_colnames(gen_latlons$duid) %>% set_rownames(gen_latlons$duid) 

source_match <- source_match^(-1) #makes non-match Inf


#get closest DUID
distance <- matrix(c(gen_latlons$longitude, gen_latlons$latitude), ncol = 2) %>% distm()

source_distance <- source_match*distance

matching_reference <- data.frame(source_distance) %>% set_colnames(gen_latlons$duid) %>% set_rownames(gen_latlons$duid) %>% 
  .[, which(gen_latlons$classification == "Semi-Scheduled")] %>% #keep semi-scheduled cols
  .[which(gen_latlons$classification == "Non-Scheduled"),] %>% #keep non-scheduled rows
  as.data.frame() %>% 
  mutate(non = rownames(.)) %>% 
  pivot_longer(cols = -non, names_to = "semi") %>% 
  group_by(non) %>% 
  filter(value == min(value)) %>% 
  distinct(non, .keep_all = TRUE)#removes duplicates



#####################
input <- scada_year %>% ungroup() %>% filter(duid %in% matching_reference$non) %>% select(duid, scadavalue) %>% 
  left_join(generator_details_AEMO, by = "duid") %>% 
  mutate(non_cf = scadavalue/registeredcapacity) %>% select(duid, non_cf) %>% 
  group_by(duid) %>% nest() %>% 
  rename(non = duid, non_data = data)

output <- scada_year %>% ungroup() %>% filter(duid %in% matching_reference$semi) %>% select(duid, scadavalue) %>%  
  left_join(generator_details_AEMO, by = "duid") %>% 
  mutate(semi_cf = scadavalue/registeredcapacity) %>% select(duid, semi_cf) %>% 
  group_by(duid) %>% nest() %>% 
  rename(semi = duid, semi_data = data)

matching_data <- matching_reference %>% ungroup() %>% left_join(input, by = "non") %>% left_join(output, by = "semi") %>% 
  mutate(data = map2(non_data, semi_data, 
                     ~ as_tibble(cbind(.x, .y)))) %>% 
  mutate(model =  map(data, 
                      ~lm(non_cf ~ semi_cf, data = .)),
         pred_data = map2(semi_data, model, 
                          ~as_tibble(data.frame(pred = predict(.y, .x)))),#
         imputed_non_cf = map2(non_data, pred_data, 
                        ~ ifelse(is.na(.x$non_cf), 
                                 .y$pred,
                                 .x$non_cf))) %>% 
  left_join(generator_details_AEMO %>% select(non = duid, registeredcapacity), by = "non") %>% 
  mutate(imputed_non_scada = map2(imputed_non_cf, registeredcapacity,
                                  ~ .x*.y))


#view all coeffs
map(matching_data$model, ~summary(.) %>% coefficients())


#Add back to scada dataset
####################################

imputed_data <- matching_data %>% select(duid = non, imputed_non_scada) %>% 
  mutate(imputed_non_scada = map(imputed_non_scada, ~data.frame(scadavalue = ., 
                                            settlementdate = (scada_year %>% .[["settlementdate"]] %>% 
                                                                unique())))) %>% 
  unnest(cols = c(imputed_non_scada))


scada_2019_imputed <- scada_year %>% 
  filter(!(duid %in% (imputed_data %>% .[["duid"]] %>% unique()))) %>% #remove non-sched
  rbind(imputed_data)#add new imputed non-sched



#merge datasets
########################
scada_imputed <- scada_2019_imputed %>% 
  rbind(scada_2017_imputed) %>% 
  rbind(scada_2015_imputed)



