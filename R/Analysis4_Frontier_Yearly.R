#Analysis4_Frontier_Yearly

### load packages
library(tidyverse)
library(lubridate)
library(data.table)
library(janitor)
library(ggpubr)
library(pracma)
Sys.setenv(TZ='UTC')


# load data
#####################
full_data <- fread("D:/Data/Cleaned/Renewables/Renewables_5min_with_non_data.csv", 
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
years <- c(2015,2017,2019)

# CF Frontiers
###########################

output <- NULL
for (i in 1:3){#year
  for (j in 1:3){#tech
    temp <- full_data %>% 
      filter(fuel_source_descriptor %in% tech[[j]], #all tech
             nem_year(settlementdate) == years[i]) %>% #loop through years
      select(duid, cf, settlementdate) %>% 
      pivot_wider(names_from = duid, values_from = cf) %>% 
      select(-settlementdate) %>% 
      as.matrix()
    if(length(temp) !=0){
      omega <- cov(temp)
      mu <- colMeans(temp)
      if(length(omega) == 1){
        output <- output %>% rbind(data.frame(cf = as.numeric(mu), sd = as.numeric(omega), 
                                              year = years[i], tech = as.character(names(tech[j])), type = "Frontier"), 
                                   stringsAsFactors = FALSE)
      }else{
        for (k in seq(from = min(mu)+0.001, to = max(mu)-0.001, by = 0.001)){
          sol <- quadprog(C = omega, #coefficients in min problem
                          d = rep(0, length(mu)), # d=0
                          Aeq = matrix(c(mu, #A matrix of constraint coeffs
                                         rep(1,length(mu))), 
                                       nrow = (2), 
                                       byrow = T), 
                          beq = c(k,1), #loops through values of i, constraint equals
                          lb = 0) #only works if give lb slightly positive
          w <- sol$xmin
          x <- w%*%mu
          y <- sqrt(t(w) %*% omega %*% w)
          
          output <- output %>% rbind.data.frame(data.frame(cf = x, sd = y, 
                                                           year = years[i], 
                                                           tech = as.character(names(tech[j])), 
                                                           type = "Frontier"), 
                                                stringsAsFactors = FALSE)
        }
      }
      
    }
  }
  
  
  #add yearly RAmax
  x <- output %>% filter(year == years[i]) %>% mutate(ra_max=cf/sd) %>% filter(ra_max == max(ra_max)) %>% .[,"cf"]
  y <- output %>% filter(year == years[i]) %>% mutate(ra_max=cf/sd) %>% filter(ra_max == max(ra_max)) %>% .[,"sd"]
  output <- output %>% rbind.data.frame(data.frame(cf = x, sd = y, year = years[i], 
                                                   tech = as.character(names(tech[j])), type = "RA Max"),  
                                 stringsAsFactors = FALSE) 
  
  #add yearly actual allocation
  actual_allocation <- full_data  %>% filter(nem_year(settlementdate) == years[i]) %>% 
    group_by(duid) %>% filter(row_number() == 1) %>%  #bc LKBONNY1 has some data missing, its regcap varies, so for now this is taking its orginal cap
    ungroup() %>% 
    mutate(weight = registeredcapacity/sum(registeredcapacity))
  
  x <- actual_allocation$weight%*%mu #need full mu
  y <- sqrt(t(actual_allocation$weight) %*% omega %*% actual_allocation$weight)
  
  output <- output %>% rbind.data.frame(data.frame(cf = x, sd = y, year = years[i],
                                                   tech = as.character(names(tech[j])), type = "Actual"),  
                                        stringsAsFactors = FALSE) 
  
}
output <- output %>% mutate(tech = factor(tech, levels = c("Solar", "Wind", "All")))





#Plots
output %>% filter(tech == "All") %>% 
  mutate(tech = as.character(tech),
         type = as.character(type)) %>% 
  mutate(type = case_when(type == "ra_max" ~ "RA Max",
                          type == "actual" ~ "Actual",
                          TRUE ~ type)) %>% 
  group_by(tech, type, year) %>% filter(cf>=cf[which(sd==min(sd))]) %>% ungroup() %>% 
  mutate(tech = factor(tech),
         year = factor(year)) %>% 
  ungroup() %>% 
  ggplot(aes(x = sd, y = cf, colour = year, shape = type, size = type))+
  geom_point() +
  labs(x = "Standard Deviation", y = "Expected Capacity Factor") +
  scale_shape_manual(breaks = c("RA Max", "Actual"), values=c(10, 16, 8))+
  scale_size_manual(breaks = c("RA Max", "Actual"), values=c(4, 1.5, 4))+
  theme_bw(base_size=10)+
  theme(legend.title=element_blank(),
        legend.spacing.y = unit(0, 'cm'),
        legend.margin = margin(0,0,0,0, unit="cm"))+
  ggsave("Output/Semi and Non/30 min/Yearly/Frontier_30min_Yearly_CF.png", width = 7)

output %>% filter(type == "Frontier") %>% 
  mutate(year = as.character(year)) %>% group_by(tech, year) %>% filter(cf>=cf[which(sd==min(sd))]) %>%
  ggplot(aes(x = sd, y = cf, colour = tech))+
  geom_point() +
  labs(x = "Standard Deviation", y = "Expected Capacity Factor") +
  facet_wrap(~ year) +
  theme_bw(base_size=10)+
  theme(legend.title=element_blank())+
  ggsave("Output/Semi and Non/30 min/Yearly/Frontier_30min_Yearly_CF_byyear.png", width = 7)

# Rev Frontiers
###########################

output <- NULL
for (i in 1:3){#year
  for (j in 1:3){#tech
    temp <- full_data %>% 
      filter(fuel_source_descriptor %in% tech[[j]], #all tech
             nem_year(settlementdate) == years[i]) %>% #loop through years
      select(duid, rev_mw, settlementdate) %>% 
      pivot_wider(names_from = duid, values_from = rev_mw) %>% 
      select(-settlementdate) %>% 
      as.matrix()
    if(length(temp) !=0){
      omega <- cov(temp)
      mu <- colMeans(temp)
      if(length(omega) == 1){
        output <- output %>% rbind(data.frame(rev_mw = as.numeric(mu), sd = as.numeric(omega), 
                                              year = years[i], tech = as.character(names(tech[j])), type = "Frontier"), 
                                   stringsAsFactors = FALSE)
      }else{
        for (k in seq(from = min(mu)+0.001, to = max(mu)-0.001, by = 0.001)){
          sol <- quadprog(C = omega, #coefficients in min problem
                          d = rep(0, length(mu)), # d=0
                          Aeq = matrix(c(mu, #A matrix of constraint coeffs
                                         rep(1,length(mu))), 
                                       nrow = (2), 
                                       byrow = T), 
                          beq = c(k,1), #loops through values of i, constraint equals
                          lb = 0) #only works if give lb slightly positive
          w <- sol$xmin
          x <- w%*%mu
          y <- sqrt(t(w) %*% omega %*% w)
          
          output <- output %>% rbind.data.frame(data.frame(rev_mw = x, sd = y, 
                                                           year = years[i], 
                                                           tech = as.character(names(tech[j])), 
                                                           type = "Frontier"), 
                                                stringsAsFactors = FALSE)
        }
      }
      
    }
  }
  
  
  #add yearly RAmax
  x <- output %>% filter(year == years[i]) %>% mutate(ra_max=rev_mw/sd) %>% filter(ra_max == max(ra_max)) %>% .[,"rev_mw"]
  y <- output %>% filter(year == years[i]) %>% mutate(ra_max=rev_mw/sd) %>% filter(ra_max == max(ra_max)) %>% .[,"sd"]
  output <- output %>% rbind.data.frame(data.frame(rev_mw = x, sd = y, year = years[i], 
                                                   tech = as.character(names(tech[j])), type = "RA Max"),  
                                        stringsAsFactors = FALSE) 
  
  #add yearly actual allocation
  actual_allocation <- full_data  %>% filter(nem_year(settlementdate) == years[i]) %>% 
    group_by(duid) %>% filter(row_number() == 1) %>%  #bc LKBONNY1 has some data missing, its regcap varies, so for now this is taking its orginal cap
    ungroup() %>% 
    mutate(weight = registeredcapacity/sum(registeredcapacity))
  
  x <- actual_allocation$weight%*%mu #need full mu
  y <- sqrt(t(actual_allocation$weight) %*% omega %*% actual_allocation$weight)
  
  output <- output %>% rbind.data.frame(data.frame(rev_mw = x, sd = y, year = years[i],
                                                   tech = as.character(names(tech[j])), type = "Actual"),  
                                        stringsAsFactors = FALSE) 
  
}
output <- output %>% mutate(tech = factor(tech, levels = c("Solar", "Wind", "All")))






#Plots
output %>% filter(tech == "All") %>%  
  mutate(tech = as.character(tech),
         type = as.character(type)) %>% 
  mutate(type = case_when(type == "ra_max" ~ "RA Max",
                          type == "actual" ~ "Actual",
                          TRUE ~ type)) %>% 
  group_by(tech, type, year) %>% filter(rev_mw>=rev_mw[which(sd==min(sd))]) %>% ungroup() %>% 
  mutate(tech = factor(tech),
         year = factor(year)) %>% 
  ungroup() %>% 
  filter(sd < 5) %>% 
  ggplot(aes(x = sd, y = rev_mw, colour = year, shape = type, size = type))+
  geom_point() +
  labs(x = "Standard Deviation", y = "5min Revenue per MW") +
  scale_shape_manual(breaks = c("RA Max", "Actual"), values=c(10, 16, 8))+
  scale_size_manual(breaks = c("RA Max", "Actual"), values=c(4, 1.5, 4))+
  theme_bw(base_size=10)+
  theme(legend.title=element_blank(),
        legend.spacing.y = unit(0, 'cm'),
        legend.margin = margin(0,0,0,0, unit="cm"))+
  ggsave("Output/Semi and Non/30 min/Yearly/Frontier_30min_Yearly_Rev.png", width = 7)

output %>% filter(type == "Frontier") %>% mutate(year = as.character(year)) %>% 
  group_by(tech, year) %>% filter(rev_mw>=rev_mw[which(sd==min(sd))]) %>%
  ggplot(aes(x = sd, y = rev_mw, colour = tech))+
  geom_point() +
  labs(x = "Standard Deviation", y = "5min Revenue per MW") +
  facet_wrap(~ year) +
  theme_bw(base_size=10)+
  theme(legend.title=element_blank())+
  ggsave("Output/Semi and Non/30 min/Yearly/Frontier_30min_Yearly_Rev_byyear.png", width = 7)
