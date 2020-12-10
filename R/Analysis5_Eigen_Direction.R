#Analysis6_Eigen_Direction

### load packages
library(tidyverse)
library(lubridate)
library(data.table)
library(janitor)
library(ggpubr)
library(pracma)
Sys.setenv(TZ='UTC')

#Load data
###

full_data <- fread("D:/Data/Cleaned/Renewables/Renewables_5min_data.csv") %>% #only SEMI
  mutate(settlementdate = ymd_hms(settlementdate)) %>% as_tibble() %>% 
  left_join(generator_details_AEMO %>% select(duid, region), by = "duid")


tech <- list(Solar = c("Photovoltaic Tracking  Flat Panel", "Photovoltaic Flat Panel"), 
             Wind = "Wind - Onshore",
             All = c("Photovoltaic Tracking  Flat Panel", "Photovoltaic Flat Panel", "Wind - Onshore"))

years <- c(2015,2017,2019)

direction <- list(NS = c("NSW", "QLD"),
                  EW = c("SA", "VIC"),
                  All = c("SA", "VIC", "TAS", "NSW", "QLD"))

#  Output Eigenvalues
######################################

S <- NULL
for (i in 1:3){#direction
  for (j in 1:3){#tech
    temp <- full_data %>% 
      filter(nem_year(settlementdate) == 2019,#only 2019
             technology_type_descriptor %in% tech[[j]], #all tech
             region %in% direction[[i]]) %>% #loop through direction
      select(duid, cf, settlementdate) %>% 
      pivot_wider(names_from = duid, values_from = cf) %>% 
      select(-settlementdate) %>% 
      as.matrix()
    omega <- cov(temp)
    d <- eigen(omega)$values
    temp_S <- cumsum(d)/sum(d)
    
    S <- S %>% 
      rbind(data.frame(direction = names(direction[i]),
                       tech = names(tech[j]),
                       value = temp_S,
                       index = 1:length(temp_S)))
  }
}

S %>% filter(tech == "All") %>% 
  ggplot(aes(x = index, y = value, colour = direction)) +
  geom_point()+
  geom_line() +
  geom_abline(aes(intercept=0, slope=1/(S %>% filter(direction == "NS", tech == "All") %>% nrow()), colour = "NS"))+
  geom_abline(aes(intercept=0, slope=1/(S %>% filter(direction == "EW", tech == "All") %>% nrow()), colour = "EW"))+
  geom_abline(aes(intercept=0, slope=1/(S %>% filter(direction == "All", tech == "All") %>% nrow()), colour = "All"))+
  scale_x_continuous(breaks = seq(0, S %>% nrow(), 10))+
  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0,1)) +
  labs(x = "Number of PCs", y = "% Explained Variation")+
  theme_bw(base_size=10)+
  theme(legend.title=element_blank())+
  ggsave("Output/Direction/EigenSum_5min_CF_Direction_NS_QLD+NSW+VIC.png", width = 7)


#  Rev Eigenvalues
######################################

S <- NULL
for (i in 1:3){#direction
  for (j in 1:3){#tech
    temp <- full_data %>% 
      filter(nem_year(settlementdate) == 2019,#only 2019
             technology_type_descriptor %in% tech[[j]], #all tech
             region %in% direction[[i]]) %>% #loop through direction
      select(duid, rev_mw, settlementdate) %>% 
      pivot_wider(names_from = duid, values_from = rev_mw) %>% 
      select(-settlementdate) %>% 
      as.matrix()
    omega <- cov(temp)
    d <- eigen(omega)$values
    temp_S <- cumsum(d)/sum(d)
    
    S <- S %>% 
      rbind(data.frame(direction = names(direction[i]),
                       tech = names(tech[j]),
                       value = temp_S,
                       index = 1:length(temp_S)))
  }
}

S %>% filter(tech == "All") %>% 
  ggplot(aes(x = index, y = value, colour = direction)) +
  geom_point()+
  geom_line() +
  geom_abline(aes(intercept=0, slope=1/(S %>% filter(direction == "NS", tech == "All") %>% nrow()), colour = "NS"))+
  geom_abline(aes(intercept=0, slope=1/(S %>% filter(direction == "EW", tech == "All") %>% nrow()), colour = "EW"))+
  geom_abline(aes(intercept=0, slope=1/(S %>% filter(direction == "All", tech == "All") %>% nrow()), colour = "All"))+
  scale_x_continuous(breaks = seq(0, S %>% nrow(), 10))+
  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0,1)) +
  labs(x = "Number of PCs", y = "% Explained Variation")+
  theme_bw(base_size=10)+
  theme(legend.title=element_blank())+
  ggsave("Output/Direction/EigenSum_5min_Rev_Direction_NS_QLD+NSW.png", width = 7)


