#Get plots for the poster / presentations 

library(tidyverse)
library(data.table)
library(scico)
library(ggridges)
library(sf)
library(rnaturalearth)

## 1. Map --------------------------------------
dt <- fread("data/processed_data/clean_rewilding_meta_dataset.csv")


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

ggsave(plot = p_map, "builds/plots/presentations/map.png", dpi = 900, 
       width = 6.65, height = 5)

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

ggsave(plot = p_inset_map, "builds/plots/presentations/europe_map.png", dpi = 900, height = 3.5, width = 4.5)


## 2. Biomass overview --------------------------------------------------
dt_mega_raw <- fread("data/processed_data/rewilding_meta_dataset_with_species_traits.csv") %>% 
  mutate(species_label = paste0(common_name, "\n(", species, ")"))

dt_sp <- dt_mega_raw %>% 
  dplyr::select(citation, site_name, experimental_mechanism, species_label, species) %>% 
  unique()

table(dt_sp$species)

scico(palette = "batlow", n = 5)
c("#001959", "#215F61", "#818231", "#F19D6B", "#F9CCF9")

p_sp_biomass = dt_mega_raw %>% 
  filter(species %in% c("Bison bison", "Equus ferus caballus", "Bos primigenius taurus")) %>%
  dplyr::select(citation, site_name, species, species_label, mass_kg, biomass_kg_ha) %>% 
  unique() %>% 
  mutate(species_label = reorder(species_label, biomass_kg_ha)) %>% 
  filter(!is.na(biomass_kg_ha)) %>% 
  ggplot() +
  geom_vline(xintercept = 10000, linetype = "dashed") +
  geom_boxplot(aes(y = species_label, x = biomass_kg_ha*100), fill = "#818231", alpha = 0.5) +
  #scale_x_continuous(sec.axis = sec_axis(~ ., name = "")) + 
  labs(y = "", x = "Species Biomass Density (kg km⁻²)") +
  theme_minimal() +
  theme(panel.grid = element_blank(), 
        axis.text.x = element_text(size = 8),
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
  geom_boxplot(aes(y = "Full\nHerbivore\nCommunity", x = total_biomass_kg_ha*100), fill = "#818231", alpha = 0.5) +
  labs(y = "", x = "Total Biomass Density (kg km⁻²)") +
  # geom_point(aes(x = mean(total_biomass_kg_ha), y = 1), color = "orange", shape = 18, size = 5) +
  geom_vline(linetype = "dashed", xintercept = mean(dt_mega$total_biomass_kg_ha*100), color = "orange", linewidth = 1.1) +
  #scale_x_continuous(sec.axis = sec_axis(~ ., name = "")) + 
  theme_minimal() +
  theme(panel.grid = element_blank(), 
       # axis.text.y = element_blank(), 
        axis.text.x = element_text(size = 8),
        plot.background  = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA) )
p_total_biomass  

median(dt_mega$total_biomass_kg_ha*100)
range(dt_mega$total_biomass_kg_ha*100)
mean(dt_mega$total_biomass_kg_ha*100)

dt_sp %>% 
  group_by(species) %>% 
  summarize(Sites = n_distinct(site_name), 
            Papers = n_distinct(citation)) %>% 
  arrange(-Sites)



library(patchwork)

(p_bc <- (p_sp_biomass | p_total_biomass)) #+ plot_annotation(tag_levels = "A"))


ggsave(plot = p_bc, "builds/plots/presentations/animal_biomass.png", dpi = 900, 
       width = 6.65, height = 5)


# 3. Results -----------------------------

dt_res_plot <- fread("builds/model_results/main_model_results_plot_data.csv")


dt_plot_points <-  dt %>%
  pivot_longer(cols = c(yi_cvr, yi_smdh), 
               names_to = "effect_size", values_to = "yi") %>% 
  mutate(effect_size = ifelse(effect_size == "yi_smdh", "Effect on Mean\n(SMD)", "Effect on Heterogeneity\n(lnCVR)"),
         effect_size = reorder(effect_size, desc(effect_size)),
         vi = ifelse(effect_size == "Effect on Mean\n(SMD)", vi_smdh, vi_cvr), 
         vi_inv = 1/vi) %>% 
  left_join(dt_res_plot[, c("eco_response", "clean_response", "label_n")] %>% 
              unique()) %>% 
  filter(!is.na(label_n))

dt_annot <- dt_res_plot %>%
  dplyr::filter(effect_size == "Effect on Mean\n(SMD)") %>% 
  dplyr::select(clean_response, label_n, effect_size) %>% 
  unique()

library(scico)
scico(palette = "lajolla", n = 10)
scico(palette = "lajolla", n = 10)

c("#191900", "#33220F", "#5A2F22", "#8E3F3D", "#C7504B", "#DF714F", "#E69352", "#EEB554", "#F8DE7A", "#FFFECB")

scico(palette = "bamako", n = 10)
#"#003A46" "#0E433F" "#1F4E34" "#355E26" "#527014" "#728202" "#988C02" "#BEA82E" "#E1C76D" "#FFE5AC"

response_levels <- dt_res_plot %>% 
  filter(grepl("SMD", effect_size)) %>% 
  arrange(estimate) %>% 
  select(clean_response) %>% 
  unique() %>% 
  pull()


p_res <- dt_res_plot %>% 
  filter(grepl("SMD", effect_size)) %>% 
  mutate(clean_response = reorder(clean_response, estimate)) %>%
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_jitter(data = dt_plot_points %>% 
                filter(grepl("SMD", effect_size)) %>% 
                mutate(effect_size = reorder(effect_size, desc(effect_size))), aes(x = yi, y = clean_response, size = vi_inv),
              alpha = 0.1, color = "grey25",
              height = 0.1, width = 0.01) +
  geom_pointrange(aes(x = estimate,
                      y = clean_response,
                      xmin = ci_lb,
                      xmax = ci_ub, fill = p_levels, color = p_levels),
                  shape = 23, size = 0.9, linewidth = 1.1) +
  scale_fill_manual(values = c("p < 0.05" = "#355E26",
                               "p < 0.1" = "#728202",
                               "p ≥ 0.1" = "#E1C76D")) +
  scale_color_manual(values = c("p < 0.05" = "#355E26",
                                "p < 0.1" = "#728202",
                                "p ≥ 0.1" = "#E1C76D")) +
  geom_text(data = dt_annot %>% 
              mutate(clean_response = factor(clean_response, levels = response_levels)),
            aes(x = -5,
                y = clean_response,
                label = label_n),
            hjust = 0, 
            size = 2.5,
            inherit.aes = FALSE, fontface = "italic", color = "#33220F", alpha = .9) +
  labs(x = "Effect size estimate (±95 % CI)", y = NULL, color = "", fill = "") +
  guides(size = "none") + 
  facet_wrap(~effect_size, scales = "free_x") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    legend.position = "bottom", 
    axis.text.y = element_text(size = 12), 
    strip.text = element_text(size = 12, face = "italic"), 
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

p_res
ggsave(plot = p_res, "builds/plots/presentations/main_results.png", dpi = 900, 
       width = 6, height = 4.5)

