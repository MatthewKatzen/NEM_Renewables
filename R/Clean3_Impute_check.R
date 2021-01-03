#impute check
##########################

#list of all models
matching_data$model %>% plyr::ldply(coef) 

#view all coeffs
temp <- matching_data %>% mutate(int = map_dbl(model, ~coef(.) %>% .[1]),
                                 int_p = map_dbl(model, ~summary(.) %>% coefficients() %>% .[1,4]),
                                 beta = map_dbl(model, ~coef(.) %>% .[2]),
                                 beta_p = map_dbl(model, ~summary(.) %>% coefficients() %>% .[2,4]),
                                 missing_obs = map_int(non_data, ~summarise(., sum(is.na(non_cf)))%>% .[[1,1]])) %>% 
  select(non, semi, value, int, int_p, beta, beta_p, missing_obs) %>% 
  left_join(generator_details_AEMO %>% select(non = duid, fuel_source_descriptor, non_station = station_name, non_region = region), by = "non") %>% 
  left_join(generator_details_AEMO %>% select(semi = duid, semi_station = station_name, semi_region = region), by = "semi") %>% 
  left_join(gen_latlons %>% rename(non = duid), by = "non") %>% 
  left_join(gen_latlons %>% rename(semi = duid), by = "semi") 
  


temp %>% select(non, semi, fuel_source_descriptor, distance_km = value, int, beta, missing_obs)

# How much extra MWh?

matching_data %>% 
  mutate(raw_scada = map2(.x = non_data, .y = registeredcapacity, ~.x*.y)) %>% 
  select(non, raw_scada, imputed_non_scada) %>% unnest(cols = c(raw_scada, imputed_non_scada)) %>% 
  summarise(raw_MWh = sum(non_cf, na.rm = TRUE)/12, 
            imputed_MWh = sum(imputed_non_scada)/12, 
            dif = imputed_MWh-raw_MWh)
