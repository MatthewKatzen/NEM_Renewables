#Analysis4_Yearly

### load packages
library(tidyverse)
library(lubridate)
library(data.table)
library(janitor)
library(ggpubr)
Sys.setenv(TZ='UTC')


# load data

full_data <- fread("D:/Data/Cleaned/Renewables/Renewables_30min_with_non_data.csv", 
                   select = c("settlementdate", "duid", "cf", "rev_mw")) %>% 
  mutate(settlementdate = ymd_hms(settlementdate)) %>% as_tibble() %>% 
  left_join(generator_details_AEMO %>% 
              select(duid, region, fuel_source_descriptor, classification, registeredcapacity), by = "duid")


tech <- list("Solar" = "Solar", 
             "Wind" = "Wind",
             "All" = c("Solar", "Wind"))

classification_type <- list("Non-Scheduled" = "Non-Scheduled",
                       "Semi-Scheduled" = "Semi-Scheduled",
                       "All" = c("Semi-Scheduled", "Non-Scheduled"))

years <- c(20130, 2017, 2019)

#  Output Eigenvalues
######################################

S <- NULL
for (i in 1:3){#year
  for (j in 1:3){#tech
    for (k in 1:3){#classification
      temp <- full_data %>% 
        filter(fuel_source_descriptor %in% tech[[j]], #tech
               nem_year(settlementdate) == years[i], #loop through years
               classification %in% classification_type[[k]]) %>% #classification
        select(duid, cf, settlementdate) %>% 
        pivot_wider(names_from = duid, values_from = cf) %>% 
        select(-settlementdate) %>% 
        as.matrix()
      if(length(temp)!=0){#no solar in 20130
        omega <- cov(temp)
        d <- eigen(omega)$values
        temp_S <- cumsum(d)/sum(d)
        
        S <- S %>% 
          rbind(data.frame(year = as.character(years[i]),
                           tech = names(tech[j]),
                           classification = names(classification_type[k]),
                           value = temp_S,
                           index = 1:length(temp_S)))}  
    }
  }
}
S <- S %>% mutate(tech = factor(tech, levels = c("Solar", "Wind", "All")))


S %>% filter(tech == "All", classification == "All") %>% 
  ggplot(aes(x = index, y = value, colour = year)) +
  geom_point()+
  geom_line() +
  geom_abline(aes(intercept=0, slope=1/(S %>% filter(year==2015, tech == "All", classification == "All") %>% nrow()), colour = "2015"))+
  geom_abline(aes(intercept=0, slope=1/(S %>% filter(year==2017, tech == "All", classification == "All") %>% nrow()), colour = "2017"))+
  geom_abline(aes(intercept=0, slope=1/(S %>% filter(year==2019, tech == "All", classification == "All") %>% nrow()), colour = "2019"))+
  scale_x_continuous(breaks = seq(0, S %>% filter(year==2019) %>% nrow(), 10))+
  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0,1)) +
  labs(x = "Number of PCs", y = "% Explained Variation")+
  theme_bw(base_size=10)+
  theme(legend.title=element_blank())+
  ggsave("Output/Semi and Non/30 min/Yearly/EigenSum_30min_CF_yearly.png", width = 7)


S %>% filter(classification == "All") %>% 
  ggplot(aes(x = index, y = value, colour = tech)) +
  geom_point()+
  geom_line() +
  scale_x_continuous(breaks = seq(0, S %>% filter(year==2019, tech == "All") %>% nrow(), 10))+
  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0,1)) +
  labs(x = "Number of PCs", y = "% Explained Variation")+
  facet_wrap(~year)+
  theme_bw(base_size=10)+
  theme(legend.title=element_blank())+
  ggsave("Output/Semi and Non/30 min/Yearly/EigenSum_30min_CF_yearly_byyear.png", width = 7)

S %>% filter(tech == "All") %>% 
  ggplot(aes(x = index, y = value, colour = classification)) +
  geom_point()+
  geom_line() +
  scale_x_continuous(breaks = seq(0, S %>% filter(year==2019, tech == "All") %>% nrow(), 10))+
  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0,1)) +
  labs(x = "Number of PCs", y = "% Explained Variation")+
  facet_wrap(~year)+
  theme_bw(base_size=10)+
  theme(legend.title=element_blank())+
  ggsave("Output/Semi and Non/30 min/Yearly/EigenSum_30min_CF_yearly_byclass.png", width = 7)

S %>% 
  ggplot(aes(x = index, y = value, colour = tech)) +
  geom_point()+
  geom_line() +
  scale_x_continuous(breaks = seq(0, S %>% filter(year==2019, tech == "All") %>% nrow(), 10))+
  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0,1)) +
  labs(x = "Number of PCs", y = "% Explained Variation")+
  facet_wrap(classification~year)+
  theme_bw(base_size=10)+
  theme(legend.title=element_blank())+
  ggsave("Output/Semi and Non/30 min/Yearly/EigenSum_30min_CF_yearly_byclass_and_year.png", width = 10, height = 10)

#  REV Eigenvalues
######################################

S <- NULL
for (i in 1:3){#year
  for (j in 1:3){#tech
    for (k in 1:3){#class
    temp <- full_data %>% 
      filter(fuel_source_descriptor %in% tech[[j]], #all tech
             nem_year(settlementdate) == years[i],#loop through years
             classification %in% classification_type[[k]]) %>% #class
      select(duid, rev_mw, settlementdate) %>% 
      pivot_wider(names_from = duid, values_from = rev_mw) %>% 
      select(-settlementdate) %>% 
      as.matrix()
    if(length(temp)!=0){#no solar in 20130
      omega <- cov(temp)
      d <- eigen(omega)$values
      temp_S <- cumsum(d)/sum(d)
      
      S <- S %>% 
        rbind(data.frame(year = as.character(years[i]),
                         tech = names(tech[j]),
                         classification = names(classification_type[k]),
                         value = temp_S,
                         index = 1:length(temp_S)))}
    }
  }
}
S <- S %>% mutate(tech = factor(tech, levels = c("Solar", "Wind", "All")))



S %>% filter(tech == "All", classification == "All") %>% 
  ggplot(aes(x = index, y = value, colour = year)) +
  geom_point()+
  geom_line() +
  geom_abline(aes(intercept=0, slope=1/(S %>% filter(year==2015, tech == "All", classification == "All") %>% nrow()), colour = "2015"))+
  geom_abline(aes(intercept=0, slope=1/(S %>% filter(year==2017, tech == "All", classification == "All") %>% nrow()), colour = "2017"))+
  geom_abline(aes(intercept=0, slope=1/(S %>% filter(year==2019, tech == "All", classification == "All") %>% nrow()), colour = "2019"))+
  scale_x_continuous(breaks = seq(0, S %>% filter(year==2019) %>% nrow(), 10))+
  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0,1)) +
  labs(x = "Number of PCs", y = "% Explained Variation")+
  theme_bw(base_size=10)+
  theme(legend.title=element_blank())+
  ggsave("Output/Semi and Non/30 min/Yearly/EigenSum_30min_Rev_yearly.png", width = 7)


S %>% filter(classification == "All") %>% 
  ggplot(aes(x = index, y = value, colour = tech)) +
  geom_point()+
  geom_line() +
  scale_x_continuous(breaks = seq(0, S %>% filter(year==2019, tech == "All") %>% nrow(), 10))+
  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0,1)) +
  labs(x = "Number of PCs", y = "% Explained Variation")+
  facet_wrap(~year)+
  theme_bw(base_size=10)+
  theme(legend.title=element_blank())+
  ggsave("Output/Semi and Non/30 min/Yearly/EigenSum_30min_Rev_yearly_byyear.png", width = 7)

S %>% filter(tech == "All") %>% 
  ggplot(aes(x = index, y = value, colour = classification)) +
  geom_point()+
  geom_line() +
  scale_x_continuous(breaks = seq(0, S %>% filter(year==2019, tech == "All") %>% nrow(), 10))+
  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0,1)) +
  labs(x = "Number of PCs", y = "% Explained Variation")+
  facet_wrap(~year)+
  theme_bw(base_size=10)+
  theme(legend.title=element_blank())+
  ggsave("Output/Semi and Non/30 min/Yearly/EigenSum_30min_Rev_yearly_byclass.png", width = 7)

S %>% 
  ggplot(aes(x = index, y = value, colour = tech)) +
  geom_point()+
  geom_line() +
  scale_x_continuous(breaks = seq(0, S %>% filter(year==2019, tech == "All") %>% nrow(), 10))+
  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0,1)) +
  labs(x = "Number of PCs", y = "% Explained Variation")+
  facet_wrap(classification~year)+
  theme_bw(base_size=10)+
  theme(legend.title=element_blank())+
  ggsave("Output/Semi and Non/30 min/Yearly/EigenSum_30min_Rev_yearly_byclass_and_year.png", width = 10, height = 10)
