#Analysis6_Eigen_Direction

### load packages
library(tidyverse)
library(lubridate)
library(data.table)
library(janitor)
library(ggpubr)
library(pracma)
Sys.setenv(TZ='UTC')

#
full_data <- fread("D:/Data/Cleaned/Renewables/Renewables_30min_with_non_data.csv", 
                   select = c("settlementdate", "duid", "cf", "rev_mw")) %>% 
  mutate(settlementdate = ymd_hms(settlementdate)) %>% as_tibble() %>% 
  left_join(generator_details_AEMO %>% 
              select(duid, region, fuel_source_descriptor, classification, registeredcapacity), by = "duid")

fwrite(full_data, "D:/Data/Cleaned/Renewables/Renewables_30min_with_non_data_and_details.csv")

#Load data
###
full_data_2019 <- fread("D:/Data/Cleaned/Renewables/Renewables_30min_with_non_data_and_details.csv") %>% 
  filter(nem_year(settlementdate) == 2019)

tech <- list("Solar" = "Solar", 
             "Wind" = "Wind",
             "All" = c("Solar", "Wind"))


direction <- list(NS = c("NSW", "QLD"),
                  EW = c("TAS", "SA", "VIC"),
                  All = c("SA", "VIC", "TAS", "NSW", "QLD"))

#  Output Eigenvalues
######################################

S <- NULL
for (i in 1:3){#direction
  for (j in 1:3){#tech
    temp <- full_data_2019 %>% 
      filter(fuel_source_descriptor %in% tech[[j]], #all tech
             region %in% direction[[i]]) %>% #loop through direction
      select(duid, cf, settlementdate) %>% 
      pivot_wider(names_from = duid, values_from = cf) %>% 
      select(-settlementdate) %>% 
      as.matrix()
    if(length(temp)!=0){#no solar in 2015
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
  ggsave("Output/Semi and Non/30 min/Direction/EigenSum_30min_CF_Direction_NSW+QLD_SA+VIC+TAS.png", width = 7)


#  Rev Eigenvalues
######################################

S <- NULL
for (i in 1:3){#direction
  for (j in 1:3){#tech
    temp <- full_data_2019 %>% 
      filter(fuel_source_descriptor %in% tech[[j]], #all tech
             region %in% direction[[i]]) %>% #loop through direction
      select(duid, rev_mw, settlementdate) %>% 
      pivot_wider(names_from = duid, values_from = rev_mw) %>% 
      select(-settlementdate) %>% 
      as.matrix()
    if(length(temp)!=0){#no solar in 2015
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
  ggsave("Output/Semi and Non/30 min/Direction/EigenSum_30min_Rev_Direction_NSW+QLD_SA+VIC+TAS.png", width = 7)


