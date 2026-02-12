######### make figures #############

library(tidyverse)
library(data.table)
library(scico)
library(ggridges)



## 1. Dataset overview --------------------------------------------------
dt_mega_raw <- fread("data/processed_data/rewilding_meta_dataset_with_species_traits.csv") %>% 
  mutate(species_label = paste0(common_name, "\n(", species, ")"))

dt_sp <- dt_mega_raw %>% 
  dplyr::select(citation, site_name, experimental_mechanism, species_label, species) %>% 
  unique()

table(dt_sp$species)


p_sp_biomass = dt_mega_raw %>% 
  dplyr::select(citation, site_name, species, species_label, mass_kg, biomass_kg_ha) %>% 
  unique() %>% 
  mutate(species_label = reorder(species_label, biomass_kg_ha)) %>% 
  filter(!is.na(biomass_kg_ha)) %>% 
  ggplot() +
  geom_boxplot(aes(y = species_label, x = biomass_kg_ha*100)) +
  labs(y = "", x = "Species Biomass (kg km⁻²)", title = "B") +
  theme_minimal() +
  theme(panel.grid = element_blank())
p_sp_biomass

dt_mega <- dt_mega_raw %>%
  group_by(citation, experimental_mechanism, site_name, data_point_id) %>%
  summarise(
    total_biomass_kg_ha = sum(biomass_kg_ha, na.rm = TRUE),
    cwm_mass_kg = weighted.mean(mass_kg, individuals_ha, na.rm = TRUE),
    max_species_mass_kg = max(mass_kg, na.rm = TRUE),
    metabolic_biomass_ha = sum((mass_kg^0.75) * individuals_ha, na.rm = TRUE)
  ) %>% 
  as.data.table() %>% 
  filter(!is.na(total_biomass_kg_ha))


p_total_biomass <- dt_mega %>%
  ggplot() +
  geom_boxplot(aes(y = 1, x = total_biomass_kg_ha*100)) +
  labs(y = "", x = "Total Biomass (kg km⁻²)", title = "C") +
 # geom_point(aes(x = mean(total_biomass_kg_ha), y = 1), color = "orange", shape = 18, size = 5) +
  geom_vline(linetype = "dashed", xintercept = mean(dt_mega$total_biomass_kg_ha*100), color = "orange", linewidth = 1.1) +
  theme_minimal() +
  theme(panel.grid = element_blank(), 
        axis.text.y = element_blank() )
p_total_biomass  


dt_sp_plot = dt_sp %>% 
  group_by(species_label) %>% 
  summarize(Sites = n_distinct(site_name), 
            Papers = n_distinct(citation))


p_studies = dt_sp_plot %>%
  mutate(species_label = reorder(species_label, Papers)) %>% 
  pivot_longer(
    cols = c(Sites, Papers),
    names_to = "metric",
    values_to = "count") %>% 
  ggplot(aes(y = species_label, x = count, fill = metric)) +
  geom_col(position = "dodge", alpha = 0.75) +
  labs(y = "", x = "Count", fill = "", title = "A") +
  theme_minimal() +
  scale_fill_scico_d(palette = "batlow", begin = 0.2, end = 0.8) +
  theme(legend.position = c(.5, .2),
        panel.grid = element_blank())

p_studies


library(patchwork)

p_bc <- (p_sp_biomass/p_total_biomass) + plot_layout(heights = c(5, 1))


p_abc <- (p_studies | p_bc)
ggsave(plot = p_abc, "builds/plots/study_overview.png", dpi = 900, 
       width = 10, height = 8)

#### 2. Map ---------

library(sf)
library(rnaturalearth)
library(scico)

world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(continent != "Antarctica")

p_map = dt %>% 
  group_by(eco_response) %>% 
  mutate(included = case_when(
    n_citations >= 3 ~ "Included",
    n_citations < 3 ~ "Sample size too small")) %>% 
  arrange(desc(included)) %>%
  filter(!included == "island") %>% 
  select(included, citation, longitude, latitude) %>% 
  unique() %>% 
  st_as_sf(., coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_transform(., crs = "ESRI:54009") %>% 
  ggplot() +
  scale_color_scico_d(palette = "batlow", begin = 0.2, end = 0.8) +
  geom_sf(data = world %>% st_transform(., crs = "ESRI:54009"), color = "wheat3", fill = "wheat3", alpha = 0.4 ) +
  geom_sf(aes(color = included), alpha = 0.75, size = 1.25) +
  theme_void() +
  labs(color = "") +
  theme(legend.position = "bottom")
p_map

ggsave(plot = p_map, "builds/plots/map.png", dpi = 900)

## 3. Responses with Small Sample Size -------------------

dt <- fread("data/processed_data/clean_rewilding_meta_dataset.csv")

p_res <- dt %>% 
  filter(n_citations < 3) %>% 
  group_by(citation) %>% 
  slice_max(time_series_clean) %>% 
  ungroup() %>% 
  as.data.table() %>% 
  mutate(
    clean_response = case_when(
      .default = eco_response, 
      eco_response == "carbon_balance" ~ "Carbon balance",
      eco_response == "carbon_fluxes" ~ "Carbon fluxes",
      eco_response == "invertebrate_diversity" ~ "Invertebrate diversity",
      eco_response == "soil_n" ~ "Soil N",
      eco_response == "soil_cn" ~ "Soil C:N",
      eco_response == "net_nitrification" ~ "Net nitrification",
      eco_response == "soil_cp" ~ "Soil C:P",
      eco_response == "soil_np" ~ "Soil N:P",
      eco_response == "soil_density" ~ "Soil bulk density",
      eco_response == "soil_p" ~ "Soil P",
      eco_response == "soil_wrc" ~ "Soil water retention capacity",
      eco_response == "soil_fine_particles" ~ "Soil fine particles",
      eco_response == "soil_ph" ~ "Soil pH",
      eco_response == "plant_cn" ~ "Plant C:N",
      eco_response == "plant_cp" ~ "Plant C:P",
      eco_response == "plant_np" ~ "Plant N:P",
      eco_response == "plant_c" ~ "Plant C",
      eco_response == "plant_n" ~ "Plant N",
      eco_response == "plant_p" ~ "Plant P",
      eco_response == "plant_above_below_ratio" ~ "Plant above:belowground ratio",
      eco_response == "plant_relative_water_content" ~ "Plant relative water content",
      eco_response == "plant_beta_diversity" ~ "Plant beta diversity",
      eco_response == "bird_richness" ~ "Bird species richness",
      eco_response == "bird_abundance" ~ "Bird abundance",
      eco_response == "bare_ground" ~ "Bare ground cover",
      eco_response == "soil_k" ~ "Soil K",
      eco_response == "plant_k" ~ "Plant K",
      eco_response == "plant_ca" ~ "Plant Ca",
      eco_response == "plant_mg" ~ "Plant Mg",
      eco_response == "small_mammal_abundance" ~ "Small mammal abundance",
      eco_response == "plant_litter" ~ "Plant litter",
    )) %>%
  group_by(eco_response) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  mutate(label_n = paste0(clean_response, " (n = ", n_citations, " [", n, "])"),
         label_n = reorder(label_n, n_citations)) %>% 
  pivot_longer(cols = c(yi_cvr, yi_smdh), 
               names_to = "effect_size", values_to = "yi") %>% 
  mutate(effect_size = ifelse(effect_size == "yi_smdh", "SMD", "lnCVR"),
         effect_size = reorder(effect_size, desc(effect_size)),
         vi = ifelse(effect_size == "SMD", vi_smdh, vi_cvr), 
         vi_inv = 1/vi) %>% 
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_jitter(data =, aes(x = yi, y = label_n, size = 1/vi_smdh),
              alpha = 0.2, color = "grey25",
              height = 0.1, width = 0.01) +
  labs(x = "Effect Size (Standardized Mean Difference)", y = NULL, color = "") +
  facet_wrap(~effect_size, scales = "free_x") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    legend.position = "none", 
    strip.text = element_text(size = 12, face = "italic"))

p_res
ggsave(plot = p_res, "builds/plots/supplement/small_sample_size_responses.png", dpi = 900, 
       height = 8, width = 7)
