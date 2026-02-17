#### Test for publication bias ####


library(data.table)
library(tidyverse)
library(metafor)


# load data ---------

dt_raw <- fread("data/processed_data/clean_rewilding_meta_dataset.csv")



dt <-  dt_raw %>%
  mutate(eff_n = (n_high_megafauna*n_low_megafauna) / (n_high_megafauna + n_low_megafauna),
         inv_sqrt_eff_n = 1/sqrt(eff_n), ## 1/sqrt(eff_N) for testing for publication bias. If slope is significant == BIAS
         inv_eff_n := 1/(eff_n)) ## If there is bias, use 1/eff_N. The intercept of this is an unbiased estimate of true effect (use alpha = 0.01)


## 1.1 create model guide ----------------

distinct_responses = unique(dt %>% 
                              filter(experimental_mechanism != "islands with and without herbivore") %>% 
                              group_by(eco_response) %>% 
                              mutate(n_citations = n_distinct(citation)) %>%
                              filter(n_citations >= 3) %>% 
                              pull(eco_response))

model_guide <- CJ(eco_response = c(distinct_responses), 
                  mods = c("inv_sqrt_eff_n", "inv_eff_n")) %>% 
  mutate(formula = paste("~", mods))
table(model_guide)


# Now create 'selection' formula for dataset 
model_guide[, select := paste0("eco_response == ", "'" , eco_response, "'")]
model_guide

# add a unique ID
model_guide[, model_id := paste(eco_response, "bias_model",sep = "_")]
model_guide[, model_id := gsub(":", "_", model_id)]
model_guide

set.seed(161)

#1.2. model for loop ------------

dt_res <- data.frame()

for(i in 1:nrow(model_guide)){
  
  #  result <- tryCatch({ 
  #build data
  dt_sub = dt[eval(parse(text = model_guide[i, ]$select)),] %>% 
    filter(experimental_mechanism != "islands with and without herbivore") %>% 
    group_by(citation) %>% 
    slice_max(time_series_clean)
  
  
  m_smd <- rma.mv(yi = yi_smdh, # intercept only model
                  V = vi_smdh, 
                  mods = as.formula(model_guide[i, ]$formula), #invesrse square root of effective sample size 
                  random = list(~ 1 | site_name,
                                ~ 1 | citation), 
                  data = dt_sub, 
                  method = "REML",  test = "t", dfs = "contain")
  
  dt_smd <- data.frame(
    eco_response = model_guide[i, ]$eco_response, 
    estimate = m_smd$b[2], 
    ci_lb = m_smd$ci.lb[2],
    ci_ub = m_smd$ci.ub[2],
    p_val = m_smd$pval[2],
    n = nrow(dt_sub),
    n_citations = n_distinct(dt_sub$citation),
    effect_size = "SMD", 
    mod = model_guide[i, ]$mods, 
    term = model_guide[i, ]$mods)
  
  dt_smd_int <- data.frame(
    eco_response = model_guide[i, ]$eco_response, 
    estimate = m_smd$b[1], 
    ci_lb = m_smd$ci.lb[1],
    ci_ub = m_smd$ci.ub[1],
    p_val = m_smd$pval[1],
    n = nrow(dt_sub),
    n_citations = n_distinct(dt_sub$citation),
    effect_size = "SMD", 
    mod = model_guide[i, ]$mods, 
    term = "intercept")
  
  dt_tmp = dt_smd %>% rbind(dt_smd_int)
  
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
      eco_response == "plant_abundance" ~ "Plant abundance",
      
      eco_response == "invertebrate_richness" ~ "Invertebrate richness",
      eco_response == "invertebrate_diversity" ~ "Invertebrate diversity",
      eco_response == "invertebrate_abundance" ~ "Invertebrate abundance",
      
      eco_response == "bird_abundance" ~ "Bird abundance",
      eco_response == "bare_ground" ~ "Bare ground"), 
    significance = ifelse(ci_lb > 0 | ci_ub < 0, "Significant", "Not significant"), 
    clean_response = reorder(clean_response, estimate), 
    label_n = paste0(clean_response, " (n = ", n_citations, " [", n, "])"),
    label_n = reorder(label_n, estimate)) 


#plot coefficient estimates of inverse square-root effective sample size

p_res <- dt_res_plot %>% 
  filter(mod == "inv_sqrt_eff_n" & term == "inv_sqrt_eff_n") %>%
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_pointrange(aes(x = estimate,
                      y = clean_response,
                      xmin = ci_lb,
                      xmax = ci_ub, fill = significance, color = significance),
                  shape = 23, size = 0.9, linewidth = 1.1) +
  scale_fill_manual(values = c("Significant" = "#D55E00", "Not significant" = "wheat3")) +
  scale_color_manual(values = c("Significant" = "#D55E00", "Not significant" = "wheat3")) +
  labs(x = "Meta-regression slope for inverse √(effective sample size)", y = NULL, color = "") +
  # facet_wrap(~effect_size, scales = "free_x") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    legend.position = "none")

p_res #cool, doesn't look like evidence for publication bias 
ggsave(plot = p_res, "builds/plots/supplement/publication_bias_results.png", dpi = 900, height = 3, width = 9)

  
  
  
