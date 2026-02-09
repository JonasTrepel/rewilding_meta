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

table(dt_raw$animal_density_units)
table(dt_raw$areal_density_units)


dt_traits <- fread("data/covariates/HerbiTraits_1.2.csv") %>% 
  mutate(mass_kg = Mass.g/1000, 
         species = Binomial, 
         common_name = Common.Name)

glimpse(dt_traits)
unique(dt_traits[grepl("Equus", Binomial), ]$Binomial)
unique(dt_traits[grepl("Bison bison", Binomial), ]$Binomial)
unique(dt_traits[grepl("Bos", Binomial), ]$Binomial)
unique(dt_traits[grepl("Bison bonasus", Binomial), ]$Binomial)
unique(dt_traits[grepl("Castor fiber", Binomial), ]$Binomial)
unique(dt_traits[grepl("Equus ferus caballus", Binomial), ]$Binomial)
unique(dt_traits[grepl("Cervus elaphus", Binomial), ]$Binomial)
unique(dt_traits[grepl("Sus scrofa", Binomial), ]$Binomial)
unique(dt_traits[grepl("Dama dama", Binomial), ]$Binomial)
unique(dt_traits[grepl("Loxodonta africana", Binomial), ]$Binomial)
unique(dt_traits[grepl("Alces alces", Binomial), ]$Binomial)
unique(dt_traits[grepl("Ovibos moschatus", Binomial), ]$Binomial)
unique(dt_traits[grepl("Rangifer tarandus", Binomial), ]$Binomial)
unique(dt_traits[grepl("Yak", Common.Name), ]$Binomial)
unique(dt_traits[grepl("Odocoileus hemionus", Binomial), ]$Binomial)
unique(dt_traits[grepl("Capra", Binomial), ]$Binomial)
unique(dt_traits[grepl("Ovis", Binomial), ]$Binomial)
unique(dt_traits[grepl("Odocoileus virginianus", Binomial), ]$Binomial)
unique(dt_traits[grepl("Ovis ammon musimon", Binomial), ]$Binomial)
unique(dt_traits[grepl("ouflon", Common.Name), ]$Binomial)
unique(dt_traits[grepl("Equus hemionus", Binomial), ]$Binomial)
unique(dt_traits[grepl("Chelonoidis hoodensis", Binomial), ]$Binomial) #ah right, not a mammal or a bird. 




glimpse(dt_raw)
table(dt_raw$density_high_megafauna)


dt_mega_raw <- dt_raw %>% 
  mutate(density_high_megafauna = gsub(",", ";", density_high_megafauna)) %>% 
  dplyr::select(data_point_id, citation, density_high_megafauna) %>% 
  separate_rows(density_high_megafauna, sep = ";\\s*") %>%
  separate(
    density_high_megafauna,
    into = c("individuals_ha", "species"),
    sep = " ", 
    extra = "merge") %>% 
  mutate(species = case_when( #make sure herbitraits got those 
    .default = species, 
    species == "Equus caballus" ~ "Equus ferus caballus", 
    species == "Bos taurus" ~ "Bos primigenius taurus",
    species == "Bos grunniens" ~ "Bos mutus",
    species == "Ovis aries" ~ "Ovis orientalis aries",
    species == "Capra hircus" ~ "Capra aegagrus hircus",
    species == "Ovis ammon musimon" ~ "Ovis orientalis",
  ), 
  individuals_ha = as.numeric(individuals_ha)) %>% 
  left_join(dt_traits[, c("species", "mass_kg", "common_name")]) %>% 
  mutate(mass_kg = ifelse(species == "Chelonoidis hoodensis", 50 ,mass_kg), ##seems to be a subspecies of Chelonoides niger which can get massive. But they only introduced young ones and it seems to be a smaller subspecies, so may be 50 kg is ok?
         biomass_kg_ha = individuals_ha*mass_kg) 

dt_sp <- dt_mega_raw %>% 
  dplyr::select(citation, species) %>% 
  unique()

table(dt_sp$species)

dt_mega <- dt_mega_raw %>%
  group_by(data_point_id) %>%
  summarise(
    total_biomass_kg_ha = sum(biomass_kg_ha, na.rm = TRUE),
    cwm_mass_kg = weighted.mean(mass_kg, individuals_ha, na.rm = TRUE),
    max_species_mass_kg = max(mass_kg, na.rm = TRUE),
    metabolic_biomass_ha = sum((mass_kg^0.75) * individuals_ha, na.rm = TRUE)
  ) %>% 
  as.data.table()
dt_mega  

hist(dt_mega$total_biomass_kg_ha)



# 3. Group Responses ------------------------------------------------------------------------------------
     
dt_raw %>% 
  select(species_or_group, response) %>% 
  arrange(species_or_group) %>% 
  unique() %>% 
  write.table(pipe("pbcopy"), sep = "\t", row.names = FALSE)
     
     
dt_response <- dt_raw %>% 
  mutate(eco_response = case_when(
    
    
    ##### Animals ##############################
    
    ### invertegrate abundance 
    response %in% c("abundance", "biomass",  "n individuals", 
                    "eating individuals per plot", "visits") & 
    species_or_group %in% c("All Arachnids", "All Orthopterans", 
                            "Ants", "Crickets", 
                            "Dung Beetles", "Grasshoppers",
                            "Ground Beetles", "Harvestmen", 
                            "Isopods", "Millipedes", 
                            "Other Beetles", "Rove Beetles", 
                            "Slugs and Snails", "Spiders",
                            "True Bugs", "arthropods", "dung beetles",
                            "bumblebees", "butterflies", 
                            "ground dwelling arthropodes", 
                            "hymenoptera", "litter invertebrates", 
                            "orthoptera", "pollinator", 
                            "understory invertebrates", 
                            "diptera") ~ "invertebrate_abundance", 
    
    ### invertebrate richness 
    response %in% c("species richness", "estimated species richness",
                    "species density (species / area)", "taxonomic richness") & 
      species_or_group %in% c("arthropods", "butterflies",
                              "ground dwelling arthropodes", 
                              "litter invertebrates", "orthoptera", 
                              "understory invertebrates", "bumblebees") ~ "invertebrate_richness", 
    
    ### invertebrate diversity 
    response %in% c("shannon diversity") & 
      species_or_group %in% c("arthropods", "litter invertebrates", 
                              "orthoptera", "understory invertebrates") ~ "invertebrate_diversity", 
    
    
    ### bird richness 
    response %in% c("species richness") & 
      species_or_group %in% c("birds") ~ "bird_richness",
    
    ### bird abundance 
    response %in% c("abundance", "seabird density") & 
      species_or_group %in% c("birds", "seabirds") ~ "bird_abundance",
    
    ### small mammal abundance 
    response %in% c("total individuals") & 
      species_or_group %in% c("rodents") ~ "small_mammal_abundance",
    
    ##### Plants ##############################
    
    ### plant species richness
    response %in% c("species richness", "richness", "species density",
                    "estimated species richness", "species density (species / area)", 
                    "red list species richness") & 
    species_or_group %in% c("C3 grasses", "C4 grasses", 
                    "Forbs", "woody plants", "all plants", 
                    "annuals and biennials", "bryophytes", 
                    "graminoids", "perennials",
                    "shrubs", "trees") ~ "plant_richness", 
    
    ### plant evenness
    response %in% c("evenness") & 
    species_or_group %in% c("all plants") ~ "plant_evenness", 
    
    ### plant diversity 
    response %in% c("exponential shannon index",  "simpson diversity", 
                    "shannon diversity", "Inverse Simpson Diversity Index",
                    "Exponential Shannon Diversity", "simpson dominance", 
                    "shannon diversity") & 
     species_or_group %in% c("all plants", "bryophytes") ~ "plant_diversity", 
    
    #plant cn 
    response %in% c("Aboveground C/N ratio", "Belowground C/N ratio") & 
      species_or_group %in% c("all plants") ~ "plant_cn", 
    
    #plant cp 
    response %in% c("Aboveground C/P ratio", "Belowground C/P ratio") & 
      species_or_group %in% c("all plants") ~ "plant_cp", 
    
    #plant np 
    response %in% c("Aboveground N/P ratio", "Belowground N/P ratio") & 
      species_or_group %in% c("all plants") ~ "plant_np", 
    
    #plant biomass 
    response %in% c("Aboveground dry biomass", 
                    "Aboveground fresh biomass", 
                    "Belowground dry biomass", 
                    "biomass", "shrub biomass") & 
      species_or_group %in% c("all plants", "shrubs") ~ "plant_biomass", 
    
    ### plant C 
    response %in% c("Aboveground-C content",  "Belowground-C content", 
                    "C") & 
      species_or_group %in% c("all plants") ~ "plant_c", 
    
    ### plant N 
    response %in% c("Aboveground-N content", "Belowground-N content", 
                    "N") & 
      species_or_group %in% c("all plants") ~ "plant_n", 
    
    ### plant P 
    response %in% c("Aboveground-P content",  "Belowground-P content", 
                    "P") & 
      species_or_group %in% c("all plants") ~ "plant_p", 
    
    ### plant K
    response %in% c("K") & 
      species_or_group %in% c("all plants") ~ "plant_k", 
    
    ### plant Ca 
    response %in% c("Ca") & 
      species_or_group %in% c("all plants") ~ "plant_ca", 
    
    ### plant Mg 
    response %in% c("Mg") & 
      species_or_group %in% c("all plants") ~ "plant_mg", 
    
    ### plant height 
    response %in% c("vegetation height", "plant height",
                    "grass height", "height", "shrub height") & 
      species_or_group %in% c("all plants", "grass", 
                              "herbaceous plants", "shrubs", 
                              "trees") ~ "plant_height", 
    
    ### plant cover 
    response %in% c("vegetation cover", "plant cover", "cover", "shrub cover") & 
      species_or_group %in% c("all plants", "bryophytes",
                              "forbs", "grass",
                              "herbaceous plants", "herbs", 
                              "lichen", "moss", "shrubs", 
                              "woody plants", "lichens") ~ "plant_cover", 
    
    ### plant abundance 
    response %in% c("abundance", "tree abundance", "density") & 
      species_or_group %in% c("trees") ~ "plant_abundance", 
    
    ### litter
    response %in% c("litter") & 
      species_or_group %in% c("all plants") ~ "plant_litter", 
    
    # above belowground ratio
    response %in% c("Aboveground/Belowground") & 
      species_or_group %in% c("all plants") ~ "plant_above_below_ratio", 
    
    # plant leaf water content 
    response %in% c("relative leaf water content") & 
      species_or_group %in% c("all plants") ~ "plant_relative_water_content", 
    
    # plant beta diversity 
    response %in% c("ß diversity", "community dissimilarity") & 
      species_or_group %in% c("all plants") ~ "plant_beta_diversity", 
    
    ##### Soil ##############################
    
    
    ### soil N 
    response %in% c("Dissolved N", 
                    "Dissolved NH4-N", 
                    "Dissolved NO3-N",
                    "Microbial N", 
                    "soil N content", 
                    "N") & 
      species_or_group %in% c("soil") ~ "soil_n",
    
    ### soil C 
    response %in% c("Dissolved org. C", 
                    "Microbial C", 
                    "soil C content", 
                    "organic matter content") & 
      species_or_group %in% c("soil") ~ "soil_n",
    
    ### soil P 
    response %in% c("Soluble reactive P", 
                    "soil P content", 
                    "P") & 
      species_or_group %in% c("soil") ~ "soil_p",
    
    ### soil C/N
    response %in% c("Soil C/N ratio", "soil C:N", 
                    "C:N", 
                    "Microbial C/N ratio") & 
      species_or_group %in% c("soil") ~ "soil_cn",
    
    ### soil C/P
    response %in% c("Soil C/P ratio") & 
      species_or_group %in% c("soil") ~ "soil_cp",
    
    ### soil N/P
    response %in% c("Soil N/P ratio") & 
      species_or_group %in% c("soil") ~ "soil_np",
    
    ### soil CaCO3
    response %in% c("Soil N/P ratio") & 
      species_or_group %in% c("soil") ~ "soil_np",
    
    ### soil density
    response %in% c("Calcium carbonate") & 
      species_or_group %in% c("soil") ~ "soil_caco3",
    
    ### soil K
    response %in% c("K") & 
      species_or_group %in% c("soil") ~ "soil_k",
    
    ### soil water retention capacity 
    response %in% c("Water retention capacity") & 
      species_or_group %in% c("soil") ~ "soil_wrc",
    
    ### soil fine particles???
    response %in% c("fine particles") & 
      species_or_group %in% c("soil") ~ "soil_fine_particles",
    
    ### soil pH
    response %in% c("pH", "soil pH", "pH (CaCl₂)") & 
      species_or_group %in% c("soil") ~ "soil_ph",
    
    ### Bare ground 
    response %in% c("bare ground") & 
      species_or_group %in% c("soil") ~ "bare_ground",
    
    ### Soil Depth 
    response %in% c("soil depth") & 
      species_or_group %in% c("soil") ~ "soil_depth",
    
    ### Soil Density 
    response %in% c("Soil dry weight", "bulk density") & 
      species_or_group %in% c("soil") ~ "soil_density",
    
    
    
    ##### Ecosystem Processes ##############################
    
    
    ### ecosystem gas fluxes 
    response %in% c("ecosystem respiration",  "gross primary productivity", 
                    "methane flux", "Respiration rate") & 
      species_or_group %in% c("herbaceous plants", "soil") ~ "carbon_fluxes",
    
    ### ecosystem gas exchange 
    response %in% c("net ecosystem exchange") & 
      species_or_group %in% c("herbaceous plants") ~ "carbon_balance",
    
    ### ecosystem nitrification 
    response %in% c("Net nitrification") & 
      species_or_group %in% c("soil") ~ "net_nitrification")) %>% 
  as.data.table()
     
     
dt_response[is.na(eco_response), .(response, species_or_group)]
#sweet

# 4. Combine ---------------------------------------------

glimpse(dt_response)

dt_comb <- dt_response %>% 
  select(-c(resident_herbivores, strata_or_soil_depth, age_class, high_value_equals_high_response)) %>% 
  left_join(dt_mega) %>% 
  left_join(dt_es) %>% 
  as.data.table()


fwrite(dt_comb, "data/processed_data/clean_rewilding_meta_dataset.csv")

# fuck around 
plot(dt_comb$yi_smd, dt_comb$yi_smdh)

plot(dt_comb$yi_smdh, dt_comb$yi_rom)
cor.test(dt_comb$yi_smdh, dt_comb$yi_rom)


dt_comb[abs(yi_smd) > 100]


n_distinct(dt_comb$citation)
dt_comb %>% 
  select(eco_response, citation) %>% 
  unique() %>% 
  group_by(eco_response) %>% 
  summarize(n = n()) %>% 
  arrange(-n)

dt_comb %>% 
  group_by(citation, site_name, eco_response) %>% 
  slice_max(time_series_clean) %>% 
  group_by(eco_response) %>% 
  filter(n() > 3) %>% 
  ungroup() %>% 
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(aes(x = yi_smdh, y = eco_response), alpha = 0.2) +
  theme_minimal()

library(sf)
library(rnaturalearth)

world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(continent != "Antarctica")

dt_comb %>% 
  group_by(eco_response) %>% 
  filter(n() > 3) %>% 
  select(citation, longitude, latitude) %>% 
  unique() %>% 
  st_as_sf(., coords = c("longitude", "latitude"), crs = 4326) %>% 
  ggplot() +
  geom_sf(data = world) +
  geom_sf() +
  theme_minimal()
  


     