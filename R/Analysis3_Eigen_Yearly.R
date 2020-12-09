#Analysis4_Yearly

### load packages
library(tidyverse)
library(lubridate)
library(data.table)
library(janitor)
library(ggpubr)
Sys.setenv(TZ='UTC')


# load data

full_data <- fread("D:/Data/Cleaned/Renewables/Renewables_5min_data.csv") %>% #only SEMI
  mutate(settlementdate = ymd_hms(settlementdate)) %>% as_tibble() 


tech <- list(Solar = c("Photovoltaic Tracking  Flat Panel", "Photovoltaic Flat Panel"), 
             Wind = "Wind - Onshore",
             All = c("Photovoltaic Tracking  Flat Panel", "Photovoltaic Flat Panel", "Wind - Onshore"))

years <- c(2015,2017,2019)

#  Output Eigenvalues
######################################

S <- NULL
for (i in 1:3){#year
  for (j in 1:3){#tech
    temp <- full_data %>% 
      filter(technology_type_descriptor %in% tech[[j]], #all tech
             nem_year(settlementdate) == years[i]) %>% #loop through years
      select(duid, cf, settlementdate) %>% 
      pivot_wider(names_from = duid, values_from = cf) %>% 
      select(-settlementdate) %>% 
      as.matrix()
    omega <- cov(temp)
    d <- eigen(omega)$values
    temp_S <- cumsum(d)/sum(d)
    
    S <- S %>% 
      rbind(data.frame(year = as.character(years[i]),
                       tech = names(tech[j]),
                       value = temp_S,
                       index = 1:length(temp_S)))
  }
}

S %>% filter(tech == "All") %>% 
  ggplot(aes(x = index, y = value, colour = year)) +
  geom_point()+
  geom_line() +
  geom_abline(aes(intercept=0, slope=1/(S %>% filter(year==2015, tech == "All") %>% nrow()), colour = "2015"))+
  geom_abline(aes(intercept=0, slope=1/(S %>% filter(year==2017, tech == "All") %>% nrow()), colour = "2017"))+
  geom_abline(aes(intercept=0, slope=1/(S %>% filter(year==2019, tech == "All") %>% nrow()), colour = "2019"))+
  scale_x_continuous(breaks = seq(0, S %>% filter(year==2019) %>% nrow(), 10))+
  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0,1)) +
  labs(x = "Number of PCs", y = "% Explained Variation")+
  theme_bw(base_size=10)+
  theme(legend.title=element_blank())+
  ggsave("Output/Yearly/EigenSum_5min_CF_yearly.png", width = 7)


S %>%
  ggplot(aes(x = index, y = value, colour = tech)) +
  geom_point()+
  geom_line() +
  scale_x_continuous(breaks = seq(0, S %>% filter(year==2019, tech == "All") %>% nrow(), 10))+
  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0,1)) +
  labs(x = "Number of PCs", y = "% Explained Variation")+
  facet_wrap(~year)+
  theme_bw(base_size=10)+
  theme(legend.title=element_blank())+
  ggsave("Output/Yearly/EigenSum_5min_CF_yearly_byyear.png", width = 7)

#  REV Eigenvalues
######################################

S <- NULL
for (i in 1:3){#year
  for (j in 1:3){#tech
    temp <- full_data %>% 
      filter(technology_type_descriptor %in% tech[[j]], #all tech
             nem_year(settlementdate) == years[i]) %>% #loop through years
      select(duid, rev_mw, settlementdate) %>% 
      pivot_wider(names_from = duid, values_from = rev_mw) %>% 
      select(-settlementdate) %>% 
      as.matrix()
    omega <- cov(temp)
    d <- eigen(omega)$values
    temp_S <- cumsum(d)/sum(d)
    
    S <- S %>% 
      rbind(data.frame(year = as.character(years[i]),
                       tech = names(tech[j]),
                       value = temp_S,
                       index = 1:length(temp_S)))
  }
}


S %>% filter(tech == "All") %>% 
  ggplot(aes(x = index, y = value, colour = year)) +
  geom_point()+
  geom_line() +
  geom_abline(aes(intercept=0, slope=1/(S %>% filter(year==2015, tech == "All") %>% nrow()), colour = "2015"))+
  geom_abline(aes(intercept=0, slope=1/(S %>% filter(year==2017, tech == "All") %>% nrow()), colour = "2017"))+
  geom_abline(aes(intercept=0, slope=1/(S %>% filter(year==2019, tech == "All") %>% nrow()), colour = "2019"))+
  scale_x_continuous(breaks = seq(0, S %>% filter(year==2019) %>% nrow(), 10))+
  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0,1)) +
  labs(x = "Number of PCs", y = "% Explained Variation") + 
  theme_bw(base_size=10)+
  theme(legend.title=element_blank())+
  ggsave("Output/Yearly/EigenSum_5min_Rev_yearly.png", width = 7)



S %>%
  ggplot(aes(x = index, y = value, colour = tech)) +
  geom_point()+
  geom_line() +
  scale_x_continuous(breaks = seq(0, S %>% filter(year==2019, tech == "All") %>% nrow(), 10))+
  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0,1)) +
  labs(x = "Number of PCs", y = "% Explained Variation")+
  facet_wrap(~year)+ 
  theme_bw(base_size=10)+
  theme(legend.title=element_blank())+
  ggsave("Output/Yearly/EigenSum_5min_Rev_yearly_byyear.png", width = 7)


