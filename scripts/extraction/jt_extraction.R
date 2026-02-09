##### JT extraction ####

library(data.table)
library(tidyverse)


#Kaštovská et al 2025 Science of The Total Environment ---------------------------------------------------

# https://www.sciencedirect.com/science/article/pii/S0048969725007764?via%3Dihub#f0020
# let's start with the blood soil --
dt_k_soil_raw <- fread("data/extraction/Kaštovská et al 2025 Science of The Total Environment/kastovska_soil_data.csv") %>%
  filter(!Site == "JOS")

names(dt_k_soil_raw)

soil_vars <- names(dt_k_soil_raw) %>% .[!grepl("Site|Management", .)]


dt_k_soil_2 <- dt_k_soil_raw %>%
  pivot_longer(
    cols = all_of(soil_vars),
    names_to = "variable",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = Management,
    values_from = value,
    names_prefix = "value_"
  ) %>% 
  arrange(variable)  %>%
  separate(
    value_control,
    into = c("raw_mean_low_megafauna", "error_low_megafauna"),
    sep = "±",
    convert = TRUE
  ) %>%
  separate(
    value_rewilding,
    into = c("raw_mean_high_megafauna", "error_high_megafauna"),
    sep = "±",
    convert = TRUE
  ) %>%
  mutate(
    Site = factor(Site, levels = c("MBV", "MTR", "DOB", "HAV", "MAS", "HRK"))
  ) %>%
  arrange(variable, Site) %>%
  mutate(n_high_megafauna = 4, n_low_megafauna = 4) %>% 
  select(
    Site,
    variable,
    raw_mean_high_megafauna,
    error_high_megafauna,
    n_high_megafauna,
    raw_mean_low_megafauna,
    error_low_megafauna,
    n_low_megafauna
  )

fwrite(dt_k_soil_2, "data/extraction/Kaštovská et al 2025 Science of The Total Environment/long_soil_data.csv")

# now the bloody plants 

dt_k_plant_raw <- fread("data/extraction/Kaštovská et al 2025 Science of The Total Environment/kastovska_plant_data.csv") %>%
  rename(Site = jt) %>% 
  filter(!Site == "JOS")

names(dt_k_plant_raw)

plant_vars <- names(dt_k_plant_raw) %>% .[!grepl("Site|Management", .)]

dt_k_plant_2 <- dt_k_plant_raw %>%
  pivot_longer(
    cols = all_of(plant_vars),
    names_to = "variable",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = Management,
    values_from = value,
    names_prefix = "value_"
  ) %>% 
  arrange(variable)  %>%
  separate(
    value_control,
    into = c("raw_mean_low_megafauna", "error_low_megafauna"),
    sep = "±",
    convert = TRUE
  ) %>%
  separate(
    value_rewilding,
    into = c("raw_mean_high_megafauna", "error_high_megafauna"),
    sep = "±",
    convert = TRUE
  ) %>%
  mutate(
    Site = factor(Site, levels = c("MBV", "MTR", "DOB", "HAV", "MAS", "HRK"))
  ) %>%
  arrange(variable, Site) %>%
  mutate(n_high_megafauna = 4, n_low_megafauna = 4) %>% 
  select(
    Site,
    variable,
    raw_mean_high_megafauna,
    error_high_megafauna,
    n_high_megafauna,
    raw_mean_low_megafauna,
    error_low_megafauna,
    n_low_megafauna
  )

fwrite(dt_k_plant_2, "data/extraction/Kaštovská et al 2025 Science of The Total Environment/long_plant_data.csv")



# Alaniz et al 2024 Journal of Insect Conservation --------------------------------------------------------------

dt_a_ab_raw <- fread("data/extraction/Alaniz et al 2024 Journal of Insect Conservation/alaniz_invertebrate_abundance.csv")

art_vars <- names(dt_a_ab_raw) %>% .[!grepl("Site|Year", .)]

dt_a_meta <- fread("data/extraction/Alaniz et al 2024 Journal of Insect Conservation/alaniz_site_meta.csv")

## let's ignore the fire treatment here as we're only intersted in the bison effect 
table(dt_a_meta$Bison)

#8 sites with bison, 12 without 

setdiff(unique(dt_a_meta$Site), unique(dt_a_ab_raw$Site))
setdiff(unique(dt_a_ab_raw$Site), unique(dt_a_meta$Site))


dt_a_ab <- dt_a_ab_raw %>%
  mutate(across(all_of(art_vars), as.numeric)) %>%
  left_join(dt_a_meta[, c("Site", "Bison")]) %>% 
  pivot_longer(
    cols = all_of(art_vars),
    names_to = "variable",
    values_to = "value"
  ) %>% 
  filter(!is.na(value), !is.na(Bison)) %>% 
  group_by(variable, Year, Bison) %>% 
  summarise(
    raw_mean = mean(value, na.rm = TRUE),
    error    = sd(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = Bison,
    values_from = c(raw_mean, error),
    names_glue = "{.value}_{ifelse(Bison == 'Y', 'high_megafauna', 'low_megafauna')}"
  ) %>% 
  mutate(n_high_megafauna = 8, n_low_megafauna = 12) %>% 
  select(
    Year,
    variable,
    raw_mean_high_megafauna,
    error_high_megafauna,
    n_high_megafauna,
    raw_mean_low_megafauna,
    error_low_megafauna,
    n_low_megafauna
  ) 

dt_a_ab

fwrite(dt_a_ab, "data/extraction/Alaniz et al 2024 Journal of Insect Conservation/alaniz_long.csv")


#### Tanentzap et al 2023 Biological Conservation------------------------------------

#animal densities
dt_a <- fread("data/extraction/Tanentzap et al 2023 Biological Conservation/animals.csv")
mean(dt_a$FallowDeer)/450 #mean per ha
mean(dt_a$RedDeer)/450
mean(dt_a$TamworthPigs)/450
mean(dt_a$LonghornCattle)/450
mean(dt_a$ExmoorPonies)/450

#### ## Arthropods

dt_art_raw <- fread("data/extraction/Tanentzap et al 2023 Biological Conservation/arthropods.csv") 
nrow(dt_art_raw)
n_distinct(dt_art_raw$Site)
dt_art_raw %>% 
  group_by(Exclosure) %>% 
  summarize(mean = mean(`Trap biomass`), 
            sd = sd(`Trap biomass`), 
            n = n())


art_vars <- names(dt_art_raw) %>% .[!grepl("Site|Exclosure|Day|Trap", .)]

dt_art_raw %>%
  select(-`Trap biomass`) %>% 
  pivot_longer(
    cols = all_of(art_vars),
    names_to = "variable",
    values_to = "value"
  ) %>% 
  filter(value > 0) %>% 
  group_by(Site, Exclosure, Day) %>% 
  summarize(family_richness = n_distinct(variable)) %>% 
  group_by(Exclosure) %>% 
  summarize(mean = mean(family_richness), 
            sd = sd(family_richness), 
            n = n())

#  herbacous veg

dt_herbs_raw <- fread("data/extraction/Tanentzap et al 2023 Biological Conservation/groundveg.csv") 

dt_herbs_raw %>% 
  group_by(Treatment) %>% 
  summarize(mean = mean(percent_cover), 
            sd = sd(percent_cover), 
            n = n())

dt_herbs_raw %>% 
  group_by(Treatment) %>% 
  summarize(mean = mean(ht_cm), 
            sd = sd(ht_cm), 
            n = n())

# woodies 
dt_trees_raw <- fread("data/extraction/Tanentzap et al 2023 Biological Conservation/trees.csv") 

dt_trees_raw %>% 
  group_by(Treatment) %>% 
  summarize(mean = mean(Ht_m), 
            sd = sd(Ht_m), 
            n = n())

unique(dt_trees_raw$Site)

dt_trees_raw %>% 
  count(Site, Treatment, name = "n_trees") %>% 
  tidyr::complete(
    Site,
    Treatment,
    fill = list(n_trees = 0)
  ) %>% 
  group_by(Treatment) %>% 
  summarize(mean = mean(n_trees), 
            sd = sd(n_trees), 
            n = n())

dt_trees_raw %>% 
  group_by(Site, Treatment) %>% 
  summarize(
    n_species = n_distinct(Species),
    .groups = "drop"
  ) %>% 
  tidyr::complete(
    Site,
    Treatment,
    fill = list(n_species = 0)
  ) %>% 
  group_by(Treatment) %>% 
  summarize(
    mean = mean(n_species),
    sd   = sd(n_species),
    n    = n()
  )

### soils 

dt_soil_raw <- fread("data/extraction/Tanentzap et al 2023 Biological Conservation/soils.csv") 

dt_soil_raw %>% 
  mutate(bulk_density = drymass_g/vol_cm) %>% 
  group_by(Treatment) %>% 
  summarize(
    mean = mean(bulk_density),
    sd   = sd(bulk_density),
    n    = n()
  )
  

#don't know what LOI is, so omit. 
dt_trees_raw %>% 
  count(Site, Treatment, name = "n_trees") %>% 
  tidyr::complete(
    Site,
    Treatment,
    fill = list(n_trees = 0)
  ) %>% 
  group_by(Treatment) %>% 
  summarize(mean = mean(n_trees), 
            sd = sd(n_trees), 
            n = n())

# in theory they should have also soil nutrient data, but apparently not reporting it

#### Gordon et al 2023 Ecological Applications -----------------------------


dt_shrubs <- fread("data/extraction/Gordon et al 2023 Ecological Applications/gordon_shrub_matrix.csv") %>% 
  filter(Reintroduction.Time %in% c(2003, "none")) %>% 
  mutate(treatment = ifelse(Reintroduction.Time == "none", "control", "elephants")) 

shrub_vars <- names(dt_shrubs) %>% .[!grepl("Site|Reintroduction|treatment", .)]

dt_shrubs %>%
  pivot_longer(
    cols = all_of(shrub_vars),
    names_to = "variable",
    values_to = "value"
  ) %>% group_by(Site.Name, treatment) %>% 
  summarize(n_shrubs = sum(value)) %>% 
  group_by(treatment) %>% 
  summarize(mean = mean(n_shrubs), 
            sd = sd(n_shrubs), 
            n = n())


dt_trees <- fread("data/extraction/Gordon et al 2023 Ecological Applications/gordon_tree_dens.csv") %>% 
  filter(Reintroduction.Time %in% c(2003, "none")) %>% 
  mutate(treatment = ifelse(Reintroduction.Time == "none", "control", "elephants")) 


dt_trees %>% 
  group_by(treatment) %>% 
  summarize(mean = mean(All.Tree.Den_10ha), 
            sd = sd(All.Tree.Den_10ha), 
            n = n())



###### Fischer et al 2022 Biogeosciences ---------------------------------

#transect (1=Grazed; 2=ungrazed), the chamber (1, 2, and 3 for grazed; 0 and 2 for ungrazed. #https://edmond.mpg.de/file.xhtml?fileId=102121&version=1.0
dt_fisch <- fread("data/extraction/Fischer et al 2022 Biogeosciences/chamber_data_EDMONT.csv") %>% 
  filter(chamber.ID %in% c(0:3)) %>% 
  mutate(treatment = ifelse(transect.ID == 1, "grazed", "control"))

#nee
dt_fisch %>% 
  filter(flux_ID == "NEE") %>% 
  filter(!is.na(Flux_NEE_ER)) %>%
  group_by(treatment) %>% 
  summarize(mean = mean(Flux_NEE_ER, na.rm = T), 
            sd = sd(Flux_NEE_ER, na.rm = T), 
            n = n()) 
#er
dt_fisch %>% 
  filter(flux_ID == "ER") %>% 
  filter(!is.na(Flux_NEE_ER)) %>%
  group_by(treatment) %>% 
  summarize(mean = mean(Flux_NEE_ER, na.rm = T), 
            sd = sd(Flux_NEE_ER, na.rm = T), 
            n = n())

dt_fisch %>% 
  filter(!is.na(Flux_GPP)) %>%
  group_by(treatment) %>% 
  summarize(mean = mean(Flux_GPP, na.rm = T), 
            sd = sd(Flux_GPP, na.rm = T), 
            n = n()) 

dt_fisch %>% 
  filter(!is.na(Flux_CH4)) %>%
  group_by(treatment) %>% 
  summarize(mean = mean(Flux_CH4, na.rm = T), 
            sd = sd(Flux_CH4, na.rm = T), 
            n = n()) 
table(dt_fisch$transect.ID)
table(dt_fisch$chamber.ID)


### Garrido et al 2019 Journal of Applied Ecology --------------------------------------------


#bumblebees and butterflies
dt_b_raw <- fread("data/extraction/Garrido et al 2019 Journal of Applied Ecology/pollinator+and+plant+richness.csv")


dt_b_raw %>%
  group_by(treatment) %>% 
  summarize(mean = mean(but.sps, na.rm = T), 
            sd = sd(but.sps, na.rm = T), 
            n = n()) 

dt_b_raw %>%
  group_by(treatment) %>% 
  summarize(mean = mean(bumb.sps , na.rm = T), 
            sd = sd(bumb.sps , na.rm = T), 
            n = n()) 

dt_b_raw %>%
  group_by(treatment) %>% 
  summarize(mean = mean(but.eat , na.rm = T), 
            sd = sd(but.eat , na.rm = T), 
            n = n()) 

dt_b_raw %>%
  group_by(treatment) %>% 
  summarize(mean = mean(bumb.eat , na.rm = T), 
            sd = sd(bumb.eat , na.rm = T), 
            n = n()) 


#plant species richness

dt_p <- fread("data/extraction/Garrido et al 2019 Journal of Applied Ecology/plant+species+richness+data.csv")

dt_p %>% 
  filter(year == 2015 & season == "spring") %>% 
  group_by(treatment) %>% 
  summarize(mean = mean(num.species , na.rm = T), 
            sd = sd(num.species , na.rm = T), 
            n = n()) 

dt_p %>% 
  filter(year == 2015 & season == "summer") %>% 
  group_by(treatment) %>% 
  summarize(mean = mean(num.species , na.rm = T), 
            sd = sd(num.species , na.rm = T), 
            n = n()) 

dt_p %>% 
  filter(year == 2015 & season == "autumn") %>% 
  group_by(treatment) %>% 
  summarize(mean = mean(num.species , na.rm = T), 
            sd = sd(num.species , na.rm = T), 
            n = n()) 

dt_p %>% 
  filter(year == 2016 & season == "spring") %>% 
  group_by(treatment) %>% 
  summarize(mean = mean(num.species , na.rm = T), 
            sd = sd(num.species , na.rm = T), 
            n = n()) 

dt_p %>% 
  filter(year == 2016 & season == "summer") %>% 
  group_by(treatment) %>% 
  summarize(mean = mean(num.species , na.rm = T), 
            sd = sd(num.species , na.rm = T), 
            n = n()) 

dt_p %>% 
  filter(year == 2016 & season == "autumn") %>% 
  group_by(treatment) %>% 
  summarize(mean = mean(num.species , na.rm = T), 
            sd = sd(num.species , na.rm = T), 
            n = n()) 



# Gizicki et al 2018 Biological Invasions -------------

#plants 
dt_veg <- fread("data/extraction/Gizicki et al 2018 Biological Invasions/gizicki_vegetation.csv") %>% 
  filter(!Island == "", Status != "R") %>% 
  mutate(`Average % Vegetation Cover` = as.numeric(gsub("±", "", `Average % Vegetation Cover`)), 
         `Average Plant Biomass (g/m2)` = as.numeric(gsub("±", "", `Average Plant Biomass (g/m2)`)), 
         `Average Plant Height (cm)` = as.numeric(gsub("±", "", `Average Plant Height (cm)`)))

mean(dt_veg$`Area (m2)`)/ 10000
mean(dt_veg[Status == "G", ]$`Area (m2)`)

names(dt_veg)

dt_veg %>% 
  group_by(Status) %>% 
  summarize(mean = mean(`Observed Plant Species (Sobs)` , na.rm = T), 
            sd = sd(`Observed Plant Species (Sobs)` , na.rm = T), 
            n = n()) 

dt_veg %>% 
  group_by(Status) %>% 
  summarize(mean = mean(`Estimated Plant Species (SChao2)` , na.rm = T), 
            sd = sd(`Estimated Plant Species (SChao2)` , na.rm = T), 
            n = n()) 

dt_veg %>% 
  group_by(Status) %>% 
  summarize(mean = mean(`Estimated Plant Species Density (C)` , na.rm = T), 
            sd = sd(`Estimated Plant Species Density (C)` , na.rm = T), 
            n = n()) 

dt_veg %>% 
  group_by(Status) %>% 
  summarize(mean = mean(`Plant SWDI` , na.rm = T), 
            sd = sd(`Plant SWDI` , na.rm = T), 
            n = n()) 

dt_veg %>% 
  group_by(Status) %>% 
  summarize(mean = mean(`Average % Vegetation Cover` , na.rm = T), 
            sd = sd(`Average % Vegetation Cover` , na.rm = T), 
            n = n()) 

dt_veg %>% 
  group_by(Status) %>% 
  summarize(mean = mean(`Average Plant Biomass (g/m2)` , na.rm = T), 
            sd = sd(`Average Plant Biomass (g/m2)` , na.rm = T), 
            n = n()) 

dt_veg %>% 
  group_by(Status) %>% 
  summarize(mean = mean(`Average Plant Height (cm)` , na.rm = T), 
            sd = sd(`Average Plant Height (cm)` , na.rm = T), 
            n = n()) 

## arthropods and biiiiiiiirrrrdddddsssss

dt_brd <- fread("data/extraction/Gizicki et al 2018 Biological Invasions/gizicki_arthropods_and_birds.csv") %>% 
  filter(!Island == "", Status != "R") %>% 
  mutate(`Average # Arthropods/` = as.numeric(gsub("±", "", `Average # Arthropods/`)), 
         `Average Arthropod Biomass` = as.numeric(gsub("±", "", `Average Arthropod Biomass`)), 
         Observed = as.numeric(Observed))

names(dt_brd)

dt_brd %>% 
  group_by(Status) %>% 
  summarize(mean = mean(`Seabird Density (per km2)` , na.rm = T), 
            sd = sd(`Seabird Density (per km2)` , na.rm = T), 
            n = n()) 

dt_brd %>% 
  group_by(Status) %>% 
  summarize(mean = mean(`Observed` , na.rm = T), 
            sd = sd(`Observed` , na.rm = T), 
            n = n()) 

dt_brd %>% 
  group_by(Status) %>% 
  summarize(mean = mean(`Estimated Arthropod Species (SChao1)` , na.rm = T), 
            sd = sd(`Estimated Arthropod Species (SChao1)` , na.rm = T), 
            n = n()) 

dt_brd %>% 
  group_by(Status) %>% 
  summarize(mean = mean(`Estimated Arthropod Species Density (C)` , na.rm = T), 
            sd = sd(`Estimated Arthropod Species Density (C)` , na.rm = T), 
            n = n()) 

dt_brd %>% 
  group_by(Status) %>% 
  summarize(mean = mean(`Arthropod SWDI` , na.rm = T), 
            sd = sd(`Arthropod SWDI` , na.rm = T), 
            n = n()) 

dt_brd %>% 
  group_by(Status) %>% 
  summarize(mean = mean(`Average Arthropod Biomass` , na.rm = T), 
            sd = sd(`Average Arthropod Biomass` , na.rm = T), 
            n = n()) 

dt_brd %>% 
  group_by(Status) %>% 
  summarize(mean = mean(`Average # Arthropods/` , na.rm = T), 
            sd = sd(`Average # Arthropods/` , na.rm = T), 
            n = n()) 

#soil 

dt_soil <- fread("data/extraction/Gizicki et al 2018 Biological Invasions/gizicki_soil.csv") %>% 
  filter(!Island == "", Status != "R") %>% 
  mutate(`Average % Bare Ground` = as.numeric(gsub("±", "", `Average % Bare Ground`)), 
         `Average Soil Depth (cm)` = as.numeric(gsub("±", "", `Average Soil Depth (cm)`)),
         `Average % Organic Matter` = as.numeric(gsub("±", "", `Average % Organic Matter`)), 
         `Average % N` = as.numeric(gsub("±", "", `Average % N`)),
         `Average % P` = as.numeric(gsub("±", "", `Average % P`)), 
         `Average % CaCO3` = as.numeric(gsub("±", "", `Average % CaCO3`)),
         `Average C:N` = as.numeric(gsub("±", "", `Average C:N`)))

names(dt_soil)


dt_soil %>% 
  group_by(Status) %>% 
  summarize(mean = mean(`Average % Bare Ground` , na.rm = T), 
            sd = sd(`Average % Bare Ground` , na.rm = T), 
            n = n()) 


dt_soil %>% 
  group_by(Status) %>% 
  summarize(mean = mean(`Average Soil Depth (cm)` , na.rm = T), 
            sd = sd(`Average Soil Depth (cm)` , na.rm = T), 
            n = n()) 


dt_soil %>% 
  group_by(Status) %>% 
  summarize(mean = mean(`Average % Organic Matter` , na.rm = T), 
            sd = sd(`Average % Organic Matter` , na.rm = T), 
            n = n()) 


dt_soil %>% 
  group_by(Status) %>% 
  summarize(mean = mean(`Average % N` , na.rm = T), 
            sd = sd(`Average % N` , na.rm = T), 
            n = n()) 


dt_soil %>% 
  group_by(Status) %>% 
  summarize(mean = mean(`Average % P` , na.rm = T), 
            sd = sd(`Average % P` , na.rm = T), 
            n = n()) 


dt_soil %>% 
  group_by(Status) %>% 
  summarize(mean = mean(`Average % CaCO3` , na.rm = T), 
            sd = sd(`Average % CaCO3` , na.rm = T), 
            n = n()) 


dt_soil %>% 
  group_by(Status) %>% 
  summarize(mean = mean(`Average C:N` , na.rm = T), 
            sd = sd(`Average C:N` , na.rm = T), 
            n = n()) 
