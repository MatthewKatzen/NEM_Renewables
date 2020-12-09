### load packages
library(tidyverse)
library(lubridate)
library(data.table)
library(janitor)
library(ggpubr)
library(scales)
Sys.setenv(TZ='UTC')



#Load data
full_data <- fread("D:/Data/Cleaned/Renewables/Renewables_5min_data.csv") %>% 
  mutate(settlementdate = ymd_hms(settlementdate)) %>% as_tibble() 
         
# OUTPUT frontiers
####################################

# Just run Frontier Yearly

output %>% filter(year == 2019) %>% 
  mutate(tech = as.character(tech)) %>% 
  mutate(tech = case_when(type == "RA Max" ~ "RA Max",
                          type == "Actual" ~ "Actual",
                          TRUE ~ tech)) %>% 
  group_by(tech, type, year) %>% filter(cf>=cf[which(sd==min(sd))]) %>% ungroup() %>% 
  filter(sd < 8) %>% 
  mutate(tech = factor(tech, levels =  c("Solar", "Wind","All", "RA Max", "Actual"))) %>% 
  ggplot(aes(x = sd, y = cf, colour = tech, shape = tech, size = tech))+
  geom_point() +
  labs(x = "Standard Deviation", y = "Expected Capacity Factor") +
  scale_shape_manual(values=c(16, 16, 16, 8, 10))+
  scale_size_manual(values=c(1.5, 1.5, 1.5, 4, 4))+
  scale_colour_manual(values=c(hue_pal()(3)[1], hue_pal()(3)[2], hue_pal()(3)[3], "black", "black"))+
  theme_bw(base_size=10)+
  theme(legend.title=element_blank())+
  ggsave("Output/Frontier_5min_CF.png", width = 7)


# OUTPUT RAMAX v ACTUAL
###############################


portfolio <- full_data %>% filter(nem_year(settlementdate) == 2019) %>% 
  group_by(duid) %>% filter(row_number() == 1) %>%  #bc LKBONNY1 has some data missing, its regcap varies, so for now this is taking its orginal cap
  ungroup() %>% 
  select(-mwh, -rev_mw, -cf) %>% 
  mutate(actual_weight = registeredcapacity/sum(registeredcapacity))

i = output %>% filter(tech == "All", year == 2019, type == "ra_max") %>% .[["cf"]]
sol <- quadprog(C = omega, #coefficients in min problem
                d = rep(0, length(mu)), # d=0
                Aeq = matrix(c(mu, #A matrix of constraint coeffs
                               rep(1,length(mu))), 
                             nrow = (2), 
                             byrow = T), 
                beq = c(i,1), #loops through values of i, constraint equals
                lb = 0)

portfolio <- portfolio %>% cbind(ra_max_weight = sol$xmin) %>% 
  left_join(generator_details_AEMO %>% select(duid, region), by = "duid")

#plot
portfolio %>% 
  mutate(tech_simple = case_when(technology_type_descriptor %in% c("Photovoltaic Tracking  Flat Panel", "Photovoltaic Flat Panel") ~ "Solar", 
                                 technology_type_descriptor == "Wind - Onshore" ~ "Wind")) %>% 
  select(duid, tech_simple, region, actual_weight, ra_max_weight) %>% 
  pivot_longer(cols = c("actual_weight", "ra_max_weight")) %>% 
  mutate(value = ifelse(value<0.00001, 0, value)) %>%
  arrange(region) %>% 
  ggplot(aes(x = factor(duid, levels = unique(duid)), y = value, fill = region))+
  geom_histogram(stat="identity") +
  facet_grid(name~tech_simple, scale="free_x",  
             labeller = labeller(name = c("actual_weight" = "Actual", "ra_max_weight" = "RA Max")))+
  labs(y = "Weight")+
  theme_bw(base_size=10)+
  theme(axis.title.x=element_blank(),#remove duid names
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(legend.title=element_blank())+ #remove legeend title
  ggsave("Output/RaMax_CF_Weights.png", width = 7)



















# REVENUE FRONTIER
######################################

# Just run Frontier Yearly


output %>% filter(year == 2019) %>% 
  mutate(tech = as.character(tech)) %>% 
  mutate(tech = case_when(type == "ra_max" ~ "RA Max",
                          type == "actual" ~ "Actual",
                          TRUE ~ tech)) %>% 
  group_by(tech, type, year) %>% filter(rev_mw>=rev_mw[which(sd==min(sd))]) %>% ungroup() %>% 
  filter(sd < 8) %>% 
  mutate(tech = factor(tech, levels =  c("Solar", "Wind","All", "RA Max", "Actual"))) %>% 
  ggplot(aes(x = sd, y = rev_mw, colour = tech, shape = tech, size = tech))+
  geom_point() +
  labs(x = "Standard Deviation", y = "5min Revenue per MW") +
  scale_shape_manual(values=c(16, 16, 16, 8, 10))+
  scale_size_manual(values=c(1.5, 1.5, 1.5, 4, 4))+
  scale_colour_manual(values=c(hue_pal()(3)[1], hue_pal()(3)[2], hue_pal()(3)[3], "black", "black"))+
  theme_bw(base_size=10)+
  theme(legend.title=element_blank())+
  ggsave("Output/Frontier_5min_Rev.png", width = 7)


# OUTPUT RAMAX v ACTUAL
###############################


portfolio <- full_data %>% filter(nem_year(settlementdate) == 2019) %>% 
  group_by(duid) %>% filter(row_number() == 1) %>%  #bc LKBONNY1 has some data missing, its regcap varies, so for now this is taking its orginal cap
  ungroup() %>% 
  select(-mwh, -rev_mw, -rev_mw) %>% 
  mutate(actual_weight = registeredcapacity/sum(registeredcapacity))

i = output %>% filter(tech == "All", year == 2019, type == "ra_max") %>% .[["rev_mw"]]
sol <- quadprog(C = omega, #coefficients in min problem
                d = rep(0, length(mu)), # d=0
                Aeq = matrix(c(mu, #A matrix of constraint coeffs
                               rep(1,length(mu))), 
                             nrow = (2), 
                             byrow = T), 
                beq = c(i,1), #loops through values of i, constraint equals
                lb = 0)

portfolio <- portfolio %>% cbind(ra_max_weight = sol$xmin) %>% 
  left_join(generator_details_AEMO %>% select(duid, region), by = "duid")

#plot
portfolio %>% 
  mutate(tech_simple = case_when(technology_type_descriptor %in% c("Photovoltaic Tracking  Flat Panel", "Photovoltaic Flat Panel") ~ "Solar", 
                                             technology_type_descriptor == "Wind - Onshore" ~ "Wind")) %>% 
  select(duid, tech_simple, region, actual_weight, ra_max_weight) %>% select(duid, tech_simple, region, actual_weight, ra_max_weight) %>% 
  pivot_longer(cols = c("actual_weight", "ra_max_weight")) %>% 
  mutate(value = ifelse(value<0.00001, 0, value)) %>%
  arrange(region) %>% 
  ggplot(aes(x = factor(duid, levels = unique(duid)), y = value, fill = region))+
  geom_histogram(stat="identity") +
  facet_grid(name~tech_simple, scale="free_x",  
             labeller = labeller(name = c("actual_weight" = "Actual", "ra_max_weight" = "RA Max")))+
  labs(ylab = "Weight")+
  theme_bw(base_size=10)+
  theme(axis.title.x=element_blank(),#remove duid names
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(legend.title=element_blank())+ #remove legeend title
  labs(y = "Weight")+
  ggsave("Output/RaMax_Rev_Weights.png", width = 7)




