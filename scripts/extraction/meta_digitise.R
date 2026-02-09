library(metaDigitise)
library(data.table)
library(tidyverse)


# Noble et al 2025 Ecological Applications ----

#species richness
dt_noble_sr <- metaDigitise(dir = "data/extraction/Noble et al 2025 Ecological Applications/")

#ok, turns out you need one plot copy per y variable. We can do that :)

#evenness and shannon
dt_noble_eve <- metaDigitise(dir = "data/extraction/Noble et al 2025 Ecological Applications/")


# Chollet et al. 2013 Écoscience -----------------------------

#Joe did it! 
data <- metaDigitise(dir = "~/Library/CloudStorage/OneDrive-UniversityofAdelaide/Jonas Rewilding MA/figures for data extraction")
# write.csv(data/extraction/Chollet et al. 2013 Écoscience/, "digitised_data_Joe.csv")


# Ribeiro et al 2025 Frontiers in Ecology and Evolution -------------

(dt_rib <- metaDigitise(dir = "data/extraction/Ribeiro et al 2025 Frontiers in Ecology and Evolution/"))

#gah, fix silly mistakes 
(dt_rib2 <- metaDigitise(dir = "data/extraction/Ribeiro et al 2025 Frontiers in Ecology and Evolution/"))

# Ejrnæs et al 2024 Applied Vegetation Science -----------------------------------

(dt_ej <- metaDigitise(dir = "data/extraction/Ejrnæs et al 2024 Applied Vegetation Science/"))

# Ratajczak et al 2022 PNAS  -----------------------------------

(dt_rat <- metaDigitise(dir = "data/extraction/Ratajczak et al 2022 PNAS/"))

# Dvorský et al 2022 Plant Ecology  -----------------------------------

(dt_dvor <- metaDigitise(dir = "data/extraction/Dvorský et al 2022 Plant Ecology/"))
dt_dvor


# Moinardeau et al 2021 Global Ecology and Conservation  -----------------------------------

(dt_moin <- metaDigitise(dir = "data/extraction/Moinardeau et al 2021 Global Ecology and Conservation/"))
dt_moin

# Thulin et al 2025 Animals   -----------------------------------

(dt_thu <- metaDigitise(dir = "data/extraction/Thulin et al 2025 Animals/"))
dt_thu

# Chollet et al 2020 Ecology   -----------------------------------

(dt_chol <- metaDigitise(dir = "data/extraction/Chollet et al 2020 Ecology/"))
dt_chol

# Garrido et al 2020 Ambio   -----------------------------------

(dt_gar <- metaDigitise(dir = "data/extraction/Garrido et al 2020 Ambio/"))
dt_gar

# Barber et al 2019 Natural Areas Journal   -----------------------------------

(dt_bar <- metaDigitise(dir = "data/extraction/Barber et al 2019 Natural Areas Journal/"))
dt_bar

# McMillan et al 2028 Restoration Ecology   -----------------------------------

(dt_mcmill <- metaDigitise(dir = "data/extraction/McMillan et al 2028 Restoration Ecology/"))
dt_mcmill

# Martin et al 2009 Biological Invasions    -----------------------------------

(dt_mart <- metaDigitise(dir = "data/extraction/Martin et al 2009 Biological Invasions/"))
dt_mart

# Plassmann et al 2010 Applied Vegetation Science     -----------------------------------

(dt_pass <- metaDigitise(dir = "data/extraction/Plassmann et al 2010 Applied Vegetation Science/"))
library(tidyverse)
dt_pass %>% 
  mutate(mean = round(mean, 4), 
         sd = round(sd, 4)) %>% 
  View


# Veen et al 2024 Oikos     -----------------------------------

(dt_veen <- metaDigitise(dir = "data/extraction/Veen et al 2024 Oikos/"))
dt_veen %>% 
  mutate(mean = round(mean, 4), 
         sd = round(sd, 4)) %>% 
  View

# Tapia et al 2021 Restoration Ecology     -----------------------------------

(dt_tap <- metaDigitise(dir = "data/extraction/Tapia et al 2021 Restoration Ecology/"))
dt_tap %>% 
  mutate(mean = round(mean, 4), 
         sd = round(sd, 4)) %>% 
  View


# Stroh et al 2021 European Journal of Ecology   -----------------------------------

(dt_stroh <- metaDigitise(dir = "data/extraction/Stroh et al 2021 European Journal of Ecology/"))
dt_stroh %>% 
  mutate(mean = round(mean, 4), 
         sd = round(sd, 4)) %>% 
  View
