#data cleaning

library(data.table)
library(tidyverse)
library(metafor)
library(googlesheets4)


update_data <- T

if(update_data){
  #get most recent version from google docs
  dt_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1qmrCjXNrm7251FCP0Bt82aMtsV3KXLQ-YEZGzlI8-Qg/edit?usp=sharing", 
                   col_types = "c") %>% 
    filter(!is.na(citation), citation != "")
  
  fwrite(dt_raw, "data/raw_data/rewilding_meta_raw_dataset - dataset.csv")
  
}
  
dt_raw <- fread("data/raw_data/rewilding_meta_raw_dataset - dataset.csv") %>% 
    filter(!is.na(citation), citation != "") %>%
  filter(!experimental_mechanism == "islands with and without herbivore") %>% 
  mutate(error_high_megafauna = ifelse(error_high_megafauna == 0, 0.001, error_high_megafauna), #replace 0 error with tiny number, else effect size calculation will fail
         error_low_megafauna = ifelse(error_low_megafauna == 0, 0.001, error_low_megafauna))




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

es_cvr <- escalc(measure = "CVR", 
                  n1i = n_high_megafauna, 
                  m1i = raw_mean_high_megafauna, 
                  sd1i = error_high_megafauna, 
                  n2i = n_low_megafauna, 
                  m2i = raw_mean_low_megafauna, 
                  sd2i = error_low_megafauna, 
                  data = dt_es_1) %>% 
  dplyr::select(vi_cvr = vi, yi_cvr = yi, data_point_id, citation)



dt_es <- es_rom %>% 
  left_join(es_smd) %>% 
  left_join(es_smdh) %>% 
  left_join(es_cvr)

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
unique(dt_traits[grepl("Pecari tajacu", Binomial), ]$Binomial) #ah right, not a mammal or a bird. 


glimpse(dt_raw)
table(dt_raw$density_high_megafauna)


dt_mega_raw <- dt_raw %>% 
  mutate(density_high_megafauna = gsub(",", ";", density_high_megafauna)) %>% 
  dplyr::select(data_point_id, citation, experimental_mechanism, site_name, density_high_megafauna) %>% 
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
    species == "Dicotyles tajacu" ~ "Pecari tajacu"
  ), 
  individuals_ha = as.numeric(individuals_ha)) %>% 
  left_join(dt_traits[, c("species", "mass_kg", "common_name")]) %>% 
  mutate(mass_kg = ifelse(species == "Chelonoidis hoodensis", 50 ,mass_kg), ##seems to be a subspecies of Chelonoides niger which can get massive. But they only introduced young ones and it seems to be a smaller subspecies, so may be 50 kg is ok?
         common_name = ifelse(species == "Chelonoidis hoodensis", "Giant Tortoise", common_name),
         common_name = ifelse(common_name == "Cow", "Cattle", common_name),
         common_name = ifelse(common_name == "Wild Yak", "Yak", common_name),
         common_name = ifelse(common_name == "Common Fallow Deer; European Fallow Deer; Fallow Deer", "Fallow Deer", common_name),
         biomass_kg_ha = individuals_ha*mass_kg) 

fwrite(dt_mega_raw, "data/processed_data/rewilding_meta_dataset_with_species_traits.csv")

dt_sp <- dt_mega_raw %>% 
  dplyr::select(citation, site_name, experimental_mechanism, species) %>% 
  unique()

table(dt_sp$species)

dt_mega <- dt_mega_raw %>%
  group_by(citation, experimental_mechanism, site_name, data_point_id) %>%
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
                              "Grasshoppers", 
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
      species_or_group %in% c("all plants", "shrubs", "herbaceous plants") ~ "plant_biomass", 
    
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
      species_or_group %in% c("soil") ~ "soil_c",
    
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
  group_by(eco_response) %>% 
  mutate(n_citations = n_distinct(citation)) %>% 
  ungroup() %>% 
  as.data.table()

table(dt_comb$experimental_mechanism)

  
fwrite(dt_comb, "data/processed_data/clean_rewilding_meta_dataset.csv")


     