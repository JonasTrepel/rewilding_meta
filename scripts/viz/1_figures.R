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

scico(palette = "batlow", n = 5)
c("#001959", "#215F61", "#818231", "#F19D6B", "#F9CCF9")

p_sp_biomass = dt_mega_raw %>% 
  dplyr::select(citation, site_name, species, species_label, mass_kg, biomass_kg_ha) %>% 
  unique() %>% 
  mutate(species_label = reorder(species_label, biomass_kg_ha)) %>% 
  filter(!is.na(biomass_kg_ha)) %>% 
  ggplot() +
  geom_vline(xintercept = 10000, linetype = "dashed") +
  geom_boxplot(aes(y = species_label, x = biomass_kg_ha*100), fill = "#818231", alpha = 0.5) +
  scale_x_continuous(sec.axis = sec_axis(~ ., name = "")) + 
  labs(y = "", x = "Species Biomass Density (kg km⁻²)", title = "B") +
  theme_minimal() +
  theme(panel.grid = element_blank(), 
        plot.background  = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA))
p_sp_biomass

dt_mega <- dt_mega_raw %>%
  # group_by(citation, site_name) %>% 
  # slice_max(time_series_clean) %>% 
  filter(!is.na(biomass_kg_ha)) %>% 
  group_by(citation, experimental_mechanism, site_name, data_point_id) %>%
  summarise(
    total_biomass_kg_ha = sum(biomass_kg_ha, na.rm = TRUE),
    cwm_mass_kg = weighted.mean(mass_kg, individuals_ha, na.rm = TRUE),
    max_species_mass_kg = max(mass_kg, na.rm = TRUE),
    metabolic_biomass_ha = sum((mass_kg^0.75) * individuals_ha, na.rm = TRUE)
  ) %>% 
  as.data.table() %>% 
  filter(!is.na(total_biomass_kg_ha)) %>% 
  dplyr::select(-data_point_id) %>%
  unique()


p_total_biomass <- dt_mega %>%
  ggplot() +
  geom_vline(xintercept = 10000, linetype = "dashed") +
  geom_boxplot(aes(y = 1, x = total_biomass_kg_ha*100), fill = "#818231", alpha = 0.5) +
  labs(y = "", x = "Total Biomass Density (kg km⁻²)", title = "C") +
 # geom_point(aes(x = mean(total_biomass_kg_ha), y = 1), color = "orange", shape = 18, size = 5) +
  geom_vline(linetype = "dashed", xintercept = mean(dt_mega$total_biomass_kg_ha*100), color = "orange", linewidth = 1.1) +
  scale_x_continuous(sec.axis = sec_axis(~ ., name = "")) + 
  theme_minimal() +
  theme(panel.grid = element_blank(), 
        axis.text.y = element_blank(), 
        plot.background  = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA) )
p_total_biomass  

median(dt_mega$total_biomass_kg_ha*100)
range(dt_mega$total_biomass_kg_ha*100)
mean(dt_mega$total_biomass_kg_ha*100)


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
  scale_x_continuous(sec.axis = sec_axis(~ ., name = "")) + 
  theme(legend.position = c(.5, .2),
        panel.grid = element_blank(), 
        plot.background  = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA))

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
  theme(legend.position = "bottom", 
        plot.background  = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA))
p_map

ggsave(plot = p_map, "builds/plots/map.png", dpi = 900)

p_inset_map = dt %>% 
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
  coord_sf(
    xlim = c(-800000, 1500000), 
    ylim = c(4300000, 7000000),
    expand = FALSE
  ) +
  theme_void() +
  labs(color = "") +
  theme(legend.position = "none", 
        plot.background  = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA))
p_inset_map

ggsave(plot = p_inset_map, "builds/plots/inset_map.png", dpi = 900, height = 3.5, width = 4.5)

## 3. Responses with Small Sample Size -------------------

dt <- fread("data/processed_data/clean_rewilding_meta_dataset.csv")
nrow(dt)

dt %>% 
  group_by(eco_response, citation, site_name) %>% 
  slice_max(time_series_clean) %>% 
  nrow()


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
      eco_response == "soil_c" ~ "Soil C",
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
  mutate(label_n = paste0(clean_response, " (n = ", n, " [", n_citations, "])"),
         label_n = reorder(label_n, n)) %>% 
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
  labs(x = "Effect Size", y = NULL, color = "") +
  facet_wrap(~effect_size, scales = "free_x") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    legend.position = "none", 
    strip.text = element_text(size = 12, face = "italic"), 
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

p_res
ggsave(plot = p_res, "builds/plots/supplement/small_sample_size_responses.png", dpi = 900, 
       height = 8, width = 7)


## 4. Play around ---------------------------------------
n_distinct(dt$citation)

dt %>% 
  filter(eco_response == "plant_richness") %>% 
  group_by(citation, site_name) %>% 
  slice_max(time_series_clean) %>%
  unique() %>% 
  pull(species_or_group) %>% 
  table()

# dt %>% 
#   select(citation, doi) %>% 
#   unique() %>% 
#   View()

dt %>% 
  select(site_name, site_size_ha) %>% 
  unique() %>% 
  pull(site_size_ha) %>% 
  quantile(na.rm = T) %>% 
  round(.,)

dt %>% 
  select(site_name, years_since_introduction) %>% 
  unique() %>% 
  pull(years_since_introduction) %>% 
  quantile(na.rm = T) 


dt %>% 
  select(site_name, total_biomass_kg_ha) %>% 
  unique() %>% 
  filter(total_biomass_kg_ha >0) %>% 
  pull(total_biomass_kg_ha) %>% 
  quantile(na.rm = T)

dt %>% 
  select(site_name, total_biomass_kg_ha) %>% 
  unique() %>% 
  filter(total_biomass_kg_ha >0) %>% 
  pull(total_biomass_kg_ha) %>% 
  mean(na.rm = T)

dt %>% 
  select(citation, site_name, total_biomass_kg_ha) %>% 
  unique() %>% 
  filter(total_biomass_kg_ha < 10)


dt %>% 
  group_by(eco_response, citation, site_name) %>% 
  slice_max(time_series_clean) %>% 
  filter(eco_response == "invertebrate_abundance") %>% 
  select(yi_smdh)

binom.test(5, 6)

binom.test(10, 18)

