
### load packages
library(tidyverse)
library(lubridate)
library(data.table)
library(janitor)
library(ggpubr)
Sys.setenv(TZ='UTC')

#Load data
full_data <- fread("D:/Data/Cleaned/Renewables/Renewables_data.csv") %>% 
  mutate(settlementdate = ymd_hms(settlementdate)) %>% as_tibble()

# PLOT CF
########################################
full_data$cf %>% qplot()
full_data %>% filter(cf>1)

#  Output Eigenvalues
######################################

# Just run Eigen Yearly
###

S %>% filter(year == 2019) %>% 
  ggplot(aes(x = index, y = value, colour = tech)) +
  geom_point()+
  geom_line() +
  geom_abline(aes(intercept=0, slope=1/(S %>% filter(year==2019, tech == "Solar") %>% nrow()), colour = "Solar"))+
  geom_abline(aes(intercept=0, slope=1/(S %>% filter(year==2019, tech == "Wind") %>% nrow()), colour = "Wind"))+
  geom_abline(aes(intercept=0, slope=1/(S %>% filter(year==2019, tech == "All") %>% nrow()), colour = "All"))+
  scale_x_continuous(breaks = seq(0, S %>% filter(year==2019) %>% nrow(), 10))+
  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0,1)) +
  theme(legend.title=element_blank()) +
  labs(x = "Number of PCs", y = "% Explained Variation") + 
  theme_bw(base_size=10)+
  theme(legend.title=element_blank())+
  ggsave("Output/EigenSum_5min_CF.png", width = 7)


#  Revenue Eigenvalues
######################################

# Just run Eigen Yearly
###

S %>% filter(year == 2019) %>% 
  ggplot(aes(x = index, y = value, colour = tech)) +
  geom_point()+
  geom_line() +
  geom_abline(aes(intercept=0, slope=1/(S %>% filter(year==2019, tech == "Solar") %>% nrow()), colour = "Solar"))+
  geom_abline(aes(intercept=0, slope=1/(S %>% filter(year==2019, tech == "Wind") %>% nrow()), colour = "Wind"))+
  geom_abline(aes(intercept=0, slope=1/(S %>% filter(year==2019, tech == "All") %>% nrow()), colour = "All"))+
  scale_x_continuous(breaks = seq(0, S %>% filter(year==2019) %>% nrow(), 10))+
  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0,1)) +
  theme(legend.title=element_blank()) +
  labs(x = "Number of PCs", y = "% Explained Variation") + 
  theme_bw(base_size=10)+
  theme(legend.title=element_blank())+
  ggsave("Output/EigenSum_5min_Rev.png", width = 7)

