#Analysis7_Frontier_Direction

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

full_data_2019 <- fread("D:/Data/Cleaned/Renewables/Renewables_30min_with_non_data_and_details.csv") %>% 
  filter(nem_year(settlementdate) == 2019)

tech <- list("Solar" = "Solar", 
             "Wind" = "Wind",
             "All" = c("Solar", "Wind"))

direction <- list(NS = c("NSW", "QLD"),
                  EW = c("TAS", "SA", "VIC"),
                  All = c("SA", "VIC", "TAS", "NSW", "QLD"))

# CF Frontiers
###########################

output <- NULL
for (i in 1:3){#direction
  for (j in 1:3){#tech
    temp <- full_data_2019 %>% 
      filter(fuel_source_descriptor %in% tech[[j]], #all tech
             region %in% direction[[i]]) %>% #loop through direction
      select(duid, cf, settlementdate) %>% 
      pivot_wider(names_from = duid, values_from = cf) %>% 
      select(-settlementdate) %>% 
      as.matrix()
    omega <- cov(temp)
    mu <- colMeans(temp)
    
    if(length(omega) == 1){
      output <- output %>% rbind(data.frame(cf = as.numeric(mu), sd = as.numeric(omega), 
                                            directions = names(direction[i]), 
                                            tech = as.character(names(tech[j])), 
                                            type = "Frontier"), 
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
        
        output <- output %>% rbind.data.frame(data.frame(cf = x,sd = y, 
                                                         directions = names(direction[i]), 
                                                         tech = as.character(names(tech[j])), 
                                                         type = "Frontier"), 
                                              stringsAsFactors = FALSE)
      }
    }
  }
  
  
  #add direction RAmax
  x <- output %>% filter(directions == names(direction[i])) %>% mutate(ra_max=cf/sd) %>% filter(ra_max == max(ra_max)) %>% .[,"cf"]
  y <- output %>% filter(directions == names(direction[i])) %>% mutate(ra_max=cf/sd) %>% filter(ra_max == max(ra_max)) %>% .[,"sd"]
  output <- output %>% rbind.data.frame(data.frame(cf = x, sd = y, 
                                                   directions = names(direction[i]), 
                                                   tech = as.character(names(tech[j])), 
                                                   type = "RA Max"),  
                                        stringsAsFactors = FALSE) 
  
  #add regional actual allocation
  actual_allocation <- full_data_2019  %>% filter(region %in% direction[[i]]) %>% 
    group_by(duid) %>% filter(row_number() == 1) %>%  #bc LKBONNY1 has some data missing, its regcap varies, so for now this is taking its orginal cap
    ungroup() %>% 
    mutate(weight = registeredcapacity/sum(registeredcapacity))
  
  x <- actual_allocation$weight%*%mu #need full mu
  y <- sqrt(t(actual_allocation$weight) %*% omega %*% actual_allocation$weight)
  
  output <- output %>% rbind.data.frame(data.frame(cf = x, sd = y, 
                                                   directions = names(direction[i]),
                                                   tech = as.character(names(tech[j])), 
                                                   type = "Actual"),  
                                        stringsAsFactors = FALSE) 
  
}





#Plots
output %>% filter(tech == "All") %>% 
  mutate(tech = as.character(tech),
         type = as.character(type))  %>% 
  group_by(tech, type, directions) %>% filter(cf>=cf[which(sd==min(sd))]) %>% ungroup() %>% 
  mutate(tech = factor(tech)) %>% 
  ungroup() %>% 
  ggplot(aes(x = sd, y = cf, colour = directions, shape = type, size = type))+
  geom_point() +
  labs(x = "Standard Deviation", y = "Expected Capacity Factor") +
  scale_shape_manual(breaks = c("RA Max", "Actual"), values=c(10, 16, 8))+
  scale_size_manual(breaks = c("RA Max", "Actual"), values=c(4, 1.5, 4))+
  theme_bw(base_size=10)+
  theme(legend.title=element_blank(),
        legend.spacing.y = unit(0, 'cm'),
        legend.margin = margin(0,0,0,0, unit="cm"))+
  ggsave("Output/Semi and Non/30 min/Direction/Frontier_30min_Direction_CF_NSW+QLD_SA+VIC+TAS.png", width = 7)



# Rev Frontiers
###########################

output <- NULL
for (i in 1:3){#direction
  for (j in 1:3){#tech
    temp <- full_data_2019 %>% 
      filter(fuel_source_descriptor %in% tech[[j]], #all tech
             region %in% direction[[i]]) %>% #loop through direction
      select(duid, rev_mw, settlementdate) %>% 
      pivot_wider(names_from = duid, values_from = rev_mw) %>% 
      select(-settlementdate) %>% 
      as.matrix()
    omega <- cov(temp)
    mu <- colMeans(temp)
    
    if(length(omega) == 1){
      output <- output %>% rbind(data.frame(rev_mw = as.numeric(mu), sd = as.numeric(omega), 
                                            directions = names(direction[i]), 
                                            tech = as.character(names(tech[j])), 
                                            type = "Frontier"), 
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
        
        output <- output %>% rbind.data.frame(data.frame(rev_mw = x,sd = y, 
                                                         directions = names(direction[i]), 
                                                         tech = as.character(names(tech[j])), 
                                                         type = "Frontier"), 
                                              stringsAsFactors = FALSE)
      }
    }
  }
  
  
  #add direction RAmax
  x <- output %>% filter(directions == names(direction[i])) %>% mutate(ra_max=rev_mw/sd) %>% filter(ra_max == max(ra_max)) %>% .[,"rev_mw"]
  y <- output %>% filter(directions == names(direction[i])) %>% mutate(ra_max=rev_mw/sd) %>% filter(ra_max == max(ra_max)) %>% .[,"sd"]
  output <- output %>% rbind.data.frame(data.frame(rev_mw = x, sd = y, 
                                                   directions = names(direction[i]), 
                                                   tech = as.character(names(tech[j])), 
                                                   type = "RA Max"),  
                                        stringsAsFactors = FALSE) 
  
  #add regional actual allocation
  actual_allocation <- full_data_2019  %>% filter(region %in% direction[[i]]) %>% 
    group_by(duid) %>% filter(row_number() == 1) %>%  #bc LKBONNY1 has some data missing, its regcap varies, so for now this is taking its orginal cap
    ungroup() %>% 
    mutate(weight = registeredcapacity/sum(registeredcapacity))
  
  x <- actual_allocation$weight%*%mu #need full mu
  y <- sqrt(t(actual_allocation$weight) %*% omega %*% actual_allocation$weight)
  
  output <- output %>% rbind.data.frame(data.frame(rev_mw = x, sd = y, 
                                                   directions = names(direction[i]),
                                                   tech = as.character(names(tech[j])), 
                                                   type = "Actual"),  
                                        stringsAsFactors = FALSE) 
  
}





#Plots
output %>% filter(tech == "All") %>% 
  mutate(tech = as.character(tech),
         type = as.character(type))  %>% 
  group_by(tech, type, directions) %>% filter(rev_mw>=rev_mw[which(sd==min(sd))]) %>% ungroup() %>% 
  mutate(tech = factor(tech)) %>% 
  ungroup() %>% 
  filter(sd<50) %>% 
  ggplot(aes(x = sd, y = rev_mw, colour = directions, shape = type, size = type))+
  geom_point() +
  labs(x = "Standard Deviation", y = "30min Revenue per MW") +
  scale_shape_manual(breaks = c("RA Max", "Actual"), values=c(10, 16, 8))+
  scale_size_manual(breaks = c("RA Max", "Actual"), values=c(4, 1.5, 4))+
  theme_bw(base_size=10)+
  theme(legend.title=element_blank(),
        legend.spacing.y = unit(0, 'cm'),
        legend.margin = margin(0,0,0,0, unit="cm"))+
  ggsave("Output/Semi and Non/30 min/Direction/Frontier_30min_Direction_Rev_NSW+QLD_SA+VIC+TAS.png", width = 7)
