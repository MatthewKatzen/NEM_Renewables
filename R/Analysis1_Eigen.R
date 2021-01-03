
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



full_data <- fread("D:/Data/Cleaned/Renewables/Renewables_30min_with_non_data.csv", 
                   select = c("settlementdate", "duid", "cf", "rev_mw")) %>% 
  mutate(settlementdate = ymd_hms(settlementdate)) %>% as_tibble() %>% 
  left_join(generator_details_AEMO %>% 
              select(duid, region, fuel_source_descriptor, classification, registeredcapacity), by = "duid")

# PLOT CF
########################################
full_data$cf %>% qplot()
full_data %>% filter(cf>1)

#  Output Eigenvalues
######################################

# Just run Eigen Yearly
###

S %>% filter(year == 2019, classification == "All") %>% 
  ggplot(aes(x = index, y = value, colour = tech)) +
  geom_point()+
  geom_line() +
  geom_abline(aes(intercept=0, slope=1/(S %>% filter(year==2019, tech == "Solar", classification == "All") %>% nrow()), colour = "Solar"))+
  geom_abline(aes(intercept=0, slope=1/(S %>% filter(year==2019, tech == "Wind", classification == "All") %>% nrow()), colour = "Wind"))+
  geom_abline(aes(intercept=0, slope=1/(S %>% filter(year==2019, tech == "All", classification == "All") %>% nrow()), colour = "All"))+
  scale_x_continuous(breaks = seq(0, S %>% filter(year==2019) %>% nrow(), 10))+
  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0,1)) +
  theme(legend.title=element_blank()) +
  labs(x = "Number of PCs", y = "% Explained Variation") + 
  theme_bw(base_size=10)+
  theme(legend.title=element_blank())+
  ggsave("Output/Semi and Non/30 min/2019/EigenSum_30min_CF.png", width = 7)

S %>% filter(year == 2019, tech == "All") %>% 
  ggplot(aes(x = index, y = value, colour = classification)) +
  geom_point()+
  geom_line() +
  geom_abline(aes(intercept=0, slope=1/(S %>% filter(year==2019, tech == "All", classification == "Non-Scheduled") %>% nrow()), 
                  colour = "Non-Scheduled"))+
  geom_abline(aes(intercept=0, slope=1/(S %>% filter(year==2019, tech == "All", classification == "Semi-Scheduled") %>% nrow()), 
                  colour = "Semi-Scheduled"))+
  geom_abline(aes(intercept=0, slope=1/(S %>% filter(year==2019, tech == "All", classification == "All") %>% nrow()), 
                  colour = "All"))+
  scale_x_continuous(breaks = seq(0, S %>% filter(year==2019) %>% nrow(), 10))+
  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0,1)) +
  theme(legend.title=element_blank()) +
  labs(x = "Number of PCs", y = "% Explained Variation") + 
  theme_bw(base_size=10)+
  theme(legend.title=element_blank())+
  ggsave("Output/Semi and Non/30 min/2019/EigenSum_30min_class_CF.png", width = 7)

#  Revenue Eigenvalues
######################################

# Just run Eigen Yearly
###

S %>% filter(year == 2019, classification == "All") %>% 
  ggplot(aes(x = index, y = value, colour = tech)) +
  geom_point()+
  geom_line() +
  geom_abline(aes(intercept=0, slope=1/(S %>% filter(year==2019, tech == "Solar", classification == "All") %>% nrow()), colour = "Solar"))+
  geom_abline(aes(intercept=0, slope=1/(S %>% filter(year==2019, tech == "Wind", classification == "All") %>% nrow()), colour = "Wind"))+
  geom_abline(aes(intercept=0, slope=1/(S %>% filter(year==2019, tech == "All", classification == "All") %>% nrow()), colour = "All"))+
  scale_x_continuous(breaks = seq(0, S %>% filter(year==2019) %>% nrow(), 10))+
  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0,1)) +
  theme(legend.title=element_blank()) +
  labs(x = "Number of PCs", y = "% Explained Variation") + 
  theme_bw(base_size=10)+
  theme(legend.title=element_blank())+
  ggsave("Output/Semi and Non/30 min/2019/EigenSum_30min_Rev.png", width = 7)

S %>% filter(year == 2019, tech == "All") %>% 
  ggplot(aes(x = index, y = value, colour = classification)) +
  geom_point()+
  geom_line() +
  geom_abline(aes(intercept=0, slope=1/(S %>% filter(year==2019, tech == "All", classification == "Non-Scheduled") %>% nrow()), 
                  colour = "Non-Scheduled"))+
  geom_abline(aes(intercept=0, slope=1/(S %>% filter(year==2019, tech == "All", classification == "Semi-Scheduled") %>% nrow()), 
                  colour = "Semi-Scheduled"))+
  geom_abline(aes(intercept=0, slope=1/(S %>% filter(year==2019, tech == "All", classification == "All") %>% nrow()), 
                  colour = "All"))+
  scale_x_continuous(breaks = seq(0, S %>% filter(year==2019) %>% nrow(), 10))+
  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0,1)) +
  theme(legend.title=element_blank()) +
  labs(x = "Number of PCs", y = "% Explained Variation") + 
  theme_bw(base_size=10)+
  theme(legend.title=element_blank())+
  ggsave("Output/Semi and Non/30 min/2019/EigenSum_30min_class_Rev.png", width = 7)


