
rm(list=ls())

library(data.table)
library(tidyverse)
library(metafor)

# load data ---------

dt <- fread("data/processed_data/clean_rewilding_meta_dataset.csv")
n_distinct(dt$site_name)
n_distinct(dt$citation)

# 0 get overview -------

n <- dt[, .(n_studies = uniqueN(citation), n_data_points = uniqueN(data_point_id)), by = eco_response]
n

n_distinct(dt[is.na(eco_response), citation])
n_distinct(dt[!is.na(eco_response), citation])

# 1. Intercept only models ---------


## 1.1 create model guide ----------------

distinct_responses = unique(dt %>% 
                              group_by(eco_response) %>% 
                              mutate(n_citations = n_distinct(citation)) %>%
                              filter(n_citations >= 3) %>% 
                              pull(eco_response))

model_guide <- CJ(eco_response = c(distinct_responses))
table(model_guide)


# Now create 'selection' formula for dataset 
model_guide[, select := paste0("eco_response == ", "'" , eco_response, "'")]
model_guide

# add a unique ID
model_guide[, model_id := paste(eco_response, "intercept_only",sep = "_")]
model_guide[, model_id := gsub(":", "_", model_id)]
model_guide

set.seed(161)

#1.2. model for loop ------------

dt_res <- data.frame()

for(i in 1:nrow(model_guide)){
  
#  result <- tryCatch({ 
    #build data
    dt_sub = dt[eval(parse(text = model_guide[i, ]$select)),] %>% 
      group_by(citation, site_name) %>% 
      slice_max(time_series_clean)

    
    m_smd <- rma.mv(yi_smdh ~ 1, # intercept only model
                 V = vi_smdh, 
                 random = list(~ 1 | site_name,
                               ~ 1 | citation), 
                 data = dt_sub, 
                 method = "REML",  test = "t", dfs = "contain")
    
    m_cvr <- rma.mv(yi_cvr ~ 1, # intercept only model
                    V = vi_cvr, 
                    random = list(~ 1 | site_name,
                                  ~ 1 | citation), 
                    data = dt_sub, 
                    method = "REML",  test = "t", dfs = "contain")
    
    ## get I^2
    #https://www.metafor-project.org/doku.php/tips:i2_multilevel_multivariate
    
    #SMD
    
    W <- diag(1/m_smd$vi)
    X <- model.matrix(m_smd)
    P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
    (i2_smd = 100 * sum(m_smd$sigma2) / (sum(m_smd$sigma2) + (m_smd$k-m_smd$p)/sum(diag(P))))
    
    #LnCVR
    
    
    W <- diag(1/m_cvr$vi)
    X <- model.matrix(m_cvr)
    P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
    (i2_cvr = 100 * sum(m_cvr$sigma2) / (sum(m_cvr$sigma2) + (m_cvr$k-m_cvr$p)/sum(diag(P))))
    
    dt_smd <- data.frame(
      eco_response = model_guide[i, ]$eco_response, 
      estimate = m_smd$b[1], 
      ci_lb = m_smd$ci.lb[1],
      ci_ub = m_smd$ci.ub[1],
      p_val = m_smd$pval[1],
      n = nrow(dt_sub),
      n_citations = n_distinct(dt_sub$citation),
      effect_size = "SMD", 
      i2 = i2_smd
    )
    
    dt_cvr <- data.frame(
      eco_response = model_guide[i, ]$eco_response, 
      estimate = m_cvr$b[1], 
      ci_lb = m_cvr$ci.lb[1],
      ci_ub = m_cvr$ci.ub[1],
      p_val = m_cvr$pval[1],
      n = nrow(dt_sub),
      n_citations = n_distinct(dt_sub$citation),
      effect_size = "lnCVR",
      i2 = i2_cvr
    )
    
    dt_tmp = dt_smd %>% rbind(dt_cvr)

  # }, error = function(e) {
  #   return(NULL)
  # })
  # if (is.null(result)) {
  #   next
  # }

  dt_res <- rbind(dt_tmp, dt_res)  
    
  cat(i,"/",nrow(model_guide),"\r")
}



dt_res_plot = dt_res %>%
  mutate(
    clean_response = case_when(
      .default = eco_response,
      eco_response == "soil_ph" ~ "Soil pH",
      eco_response == "soil_p" ~ "Soil P",
      eco_response == "soil_density" ~ "Soil bulk density",
      eco_response == "soil_cn" ~ "Soil C:N ratio",
      
      eco_response == "plant_richness" ~ "Plant richness",
      eco_response == "plant_height" ~ "Plant height",
      eco_response == "plant_evenness" ~ "Plant evenness",
      eco_response == "plant_diversity" ~ "Plant diversity",
      eco_response == "plant_cover" ~ "Plant cover",
      eco_response == "plant_biomass" ~ "Plant biomass",
      eco_response == "plant_abundance" ~ "Woody plant abundance",
      
      eco_response == "invertebrate_richness" ~ "Invertebrate richness",
      eco_response == "invertebrate_diversity" ~ "Invertebrate diversity",
      eco_response == "invertebrate_abundance" ~ "Invertebrate abundance",
      
      eco_response == "bird_abundance" ~ "Bird abundance",
      eco_response == "bare_ground" ~ "Bare ground"), 
    significance = ifelse(ci_lb > 0 | ci_ub < 0, "Significant", "Not significant"), 
    muff_significance = case_when(
      p_val <= 0.001 ~ "Very strong evidence", 
      p_val > 0.001 & p_val <= 0.01 ~ "Strong evidence", 
      p_val > 0.01 & p_val < 0.05 ~ "Moderate evidence", 
      p_val >= 0.05 & p_val <= 0.1 ~ "Weak evidence", 
      p_val > 0.1 ~ "No evidence"),
    p_levels = case_when(
      p_val <= 0.001 ~ "p < 0.05", 
      p_val > 0.001 & p_val <= 0.01 ~ "p < 0.05", 
      p_val > 0.01 & p_val < 0.05 ~ "p < 0.05", 
      p_val >= 0.05 & p_val <= 0.1 ~ "p < 0.1", 
      p_val > 0.1 ~ "p ≥ 0.1"),
    effect_size = reorder(effect_size, desc(effect_size)),
    clean_response = reorder(clean_response, estimate), 
    label_n = paste0(n, " (", n_citations,")"),
    label_n = reorder(label_n, estimate)) 


dt_plot_points = dt %>%
  pivot_longer(cols = c(yi_cvr, yi_smdh), 
               names_to = "effect_size", values_to = "yi") %>% 
  mutate(effect_size = ifelse(effect_size == "yi_smdh", "SMD", "lnCVR"),
         effect_size = reorder(effect_size, desc(effect_size)),
         vi = ifelse(effect_size == "SMD", vi_smdh, vi_cvr), 
         vi_inv = 1/vi) %>% 
  left_join(dt_res_plot[, c("eco_response", "clean_response", "label_n")] %>% 
              unique()) %>% 
  filter(!is.na(label_n))

dt_annot <- dt_res_plot %>%
  dplyr::filter(effect_size == "SMD") %>% 
  dplyr::select(clean_response, label_n, effect_size) %>% 
  unique()
  
library(scico)
scico(palette = "lajolla", n = 10)
c("#191900", "#33220F", "#5A2F22", "#8E3F3D", "#C7504B", "#DF714F", "#E69352", "#EEB554", "#F8DE7A", "#FFFECB")

scico(palette = "bamako", n = 10)
#"#003A46" "#0E433F" "#1F4E34" "#355E26" "#527014" "#728202" "#988C02" "#BEA82E" "#E1C76D" "#FFE5AC"

p_res <- dt_res_plot %>% 
 # filter(effect_size == "SMD") %>% 
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_jitter(data = dt_plot_points, aes(x = yi, y = clean_response, size = vi_inv),
              alpha = 0.2, color = "grey25",
              height = 0.1, width = 0.01) +
  geom_pointrange(aes(x = estimate,
                      y = clean_response,
                      xmin = ci_lb,
                      xmax = ci_ub, fill = p_levels, color = p_levels),
                  shape = 23, size = 0.9, linewidth = 1.1) +
 # scale_fill_manual(values = c("Significant" = "#D55E00", "Not significant" = "wheat3")) +
 # scale_color_manual(values = c("Significant" = "#D55E00", "Not significant" = "wheat3")) +
  # scale_fill_manual(values = c("Very strong evidence" = "#5A2F22", 
  #                              "Strong evidence" = "#8E3F3D", 
  #                              "Moderate evidence" = "#C7504B", 
  #                              "Weak evidence" = "#DF714F",
  #                              "No evidence" = "wheat3")) +
  # scale_color_manual(values = c("Very strong evidence" = "#5A2F22", 
  #                              "Strong evidence" = "#8E3F3D", 
  #                              "Moderate evidence" = "#C7504B", 
  #                              "Weak evidence" = "#DF714F",
  #                              "No evidence" = "wheat3")) +
  scale_fill_manual(values = c("p < 0.05" = "#DF714F",
                               "p < 0.1" = "#5A2F22",
                               "p ≥ 0.1" = "wheat3")) +
  scale_color_manual(values = c("p < 0.05" = "#DF714F",
                                "p < 0.1" = "#5A2F22",
                                "p ≥ 0.1" = "wheat3")) +
  geom_text(data = dt_annot,
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

dt %>% 
  filter(eco_response == "plant_richness", 
         yi_smdh < 0) %>% 
  select(citation, response, species_or_group, data_point_id)
# ah, woody species can respond negatively. that makes sense. 


dt %>% 
  filter(eco_response == "plant_abundance", 
         yi_smdh < 0) %>% 
  select(citation, response, species_or_group, data_point_id)


ggsave(plot = p_res, "builds/plots/intercept_only_results.png", dpi = 900, height = 4, width = 8)


stats_table = dt_res_plot %>% 
  mutate(i2 = round(i2, 2), 
         p_val = round(p_val, 3), 
         estimate_ci = paste0(round(estimate, 2), " [", round(ci_lb, 2), "; ", round(ci_ub, 2), "]")) %>% 
 # filter(effect_size == "SMD") %>%
  select(`Effect Size` = effect_size, Response = clean_response, `Estimate [95 % CI]` = estimate_ci, p = p_val, `I²` = i2)

library(gt)

stats_table %>%
  gt()

