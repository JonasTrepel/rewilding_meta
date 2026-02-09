#data cleaning

library(data.table)
library(tidyverse)
library(metafor)


dt_raw <- fread("data/raw_data/rewilding_meta_raw_dataset - dataset.csv") %>% 
  filter(!is.na(citation), citation != "")


# 1. Calculate Effect Sizes ----------------------------

table(dt_raw$mean_type)
table(dt_raw$error_type) # ok cool, most already SD. Need convert the rest for escalc function 


dt_es_1 <- dt_raw %>% 
  mutate(error_high_megafauna = ifelse(error_type == "SD", error_high_megafauna, 
                                       error_high_megafauna*sqrt(n_high_megafauna)), 
         error_low_megafauna = ifelse(error_type == "SD", error_low_megafauna, 
                                       error_low_megafauna*sqrt(n_low_megafauna)))

#log ration of means
es_rom <- escalc(measure = "ROM", 
       n1i = n_high_megafauna, 
       m1i = raw_mean_high_megafauna, 
       sd1i = error_high_megafauna, 
       n2i = n_low_megafauna, 
       m2i = raw_mean_low_megafauna, 
       sd2i = error_low_megafauna, 
       data = dt_es_1) %>% 
  dplyr::select(data_point_id, citation, vi_rom = vi, yi_rom = yi)

#standardized mean difference
es_smd <- escalc(measure = "SMD", 
                 n1i = n_high_megafauna, 
                 m1i = raw_mean_high_megafauna, 
                 sd1i = error_high_megafauna, 
                 n2i = n_low_megafauna, 
                 m2i = raw_mean_low_megafauna, 
                 sd2i = error_low_megafauna, 
                 data = dt_es_1) %>% 
  dplyr::select(vi_smd = vi, yi_smd = yi, data_point_id, citation)

#standardized mean difference with heteroscedastic population variances in the two groups 
es_smdh <- escalc(measure = "SMDH", 
                 n1i = n_high_megafauna, 
                 m1i = raw_mean_high_megafauna, 
                 sd1i = error_high_megafauna, 
                 n2i = n_low_megafauna, 
                 m2i = raw_mean_low_megafauna, 
                 sd2i = error_low_megafauna, 
                 data = dt_es_1) %>% 
  dplyr::select(vi_smdh = vi, yi_smdh = yi, data_point_id, citation)


dt_es <- es_rom %>% 
  left_join(es_smd) %>% 
  left_join(es_smdh)

# 2. Sort Megafauna ---------------------------------


# 3. Group Responses --------------------------------