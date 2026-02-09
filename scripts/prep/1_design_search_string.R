#
#
# Design Web of Science Search String 
#
#

rm(list = ls())

library(data.table)
library(tidyverse)

set.seed(161)

# Benchmark Papers ----------------------------

#(identified by haphazardly searching google scholar)
dois <- c(
  "https://doi.org/10.1111/rec.12856", #McMillan et al 2018 Restoration Ecology
  "https://doi.org/10.1002/eap.2810", #Gordon et al 2023 Ecological Applications
  "https://doi.org/10.1111/conl.12968", # Aguilera & Gibbs Conservation Letters
  "https://doi.org/10.1111/1365-2664.14001", #Michaels et al 2021 Journal of Applied Ecology
  "https://doi.org/10.1111/1365-2664.13338", # Garrido et al 2019 Journal of Applied Ecology
  "https://doi.org/10.1007/s10841-022-00420-4", # Garrido et al 2022 Journal of Insect Conservation 
  "https://doi.org/10.1111/avsc.12805", # Ejrnæs et al 2024 Applied Vegetation Science
  "https://doi.org/10.1007/s11258-022-01225-w", # Dvorský et al 2022 Plant Ecology
  "https://doi.org/10.1002/ece3.6743", # Dodge et al 2020 Ecology and Evolution
  "https://doi.org/10.1073/pnas.2210433119" , # Ratajczak et al 2022 PNAS
  "https://doi.org/10.1016/j.jenvman.2023.117719", # Andersen et al 2023 Journal of Environmental Management
  "https://doi.org/10.1016/j.biocon.2023.110005", # Tanentzap et al 2023 Biological Conservation
  "https://doi.org/10.1016/j.jenvman.2024.120430", # Kaštovská et al 2024 Journal of Environmental Management
  "https://doi.org/10.1016/j.agee.2016.01.050", # van Klink et al. 2016 Agriculture, Ecosystems & Environment
  "https://doi.org/10.1111/avsc.12718"  # Bonavent et al. 2023 Applied Vegetation Science
)


dois_clean <- gsub("https://doi.org/", "", dois)


doi_string <- paste0(paste("(DO=(", paste(unique(dois_clean), collapse = " OR "),
          "))"))

cat(doi_string)


# TS string -----------------

# population 

c_sub_pop = c("megafauna",
               "megaherbivor*",
               "'keystone species'", 
               "ecosystem engineer*",
               "herbivor*",
             #  "beaver*",
               "brows*", 
               "graz*")
pop_string <-  paste0("(", paste0(unique(c_sub_pop), collapse = " OR "), ")")
cat(pop_string)

# treatment 
c_sub_treat = c("introduc*",
              "reintroduc*",
              "restor*",
              "rewild*")
treat_string <-  paste0("(", paste0(unique(c_sub_treat), collapse = " OR "), ")")
cat(treat_string)

# outcome 
c_sub_out = c("experiment*",
                "fenceline*",
                "exclosure*",
                "exclusion*",
                "control*",
            #    "compare*",
            #    "contrast*", 
            #    "treatment*", 
            #    "finding*",
                "result*")
out_string <-  paste0("(", paste0(unique(c_sub_out), collapse = " OR "), ")")
cat(out_string)

ts_string <- paste0(paste("(TS=(", paste(unique(c(pop_string, treat_string, out_string)), collapse = " AND "),
                           "))"))

cat(ts_string)

# Subject exclusion string ---------------------------------------------------

# Subject exclusion:

wos_special = data_frame(Database = "WoS", 
                         Area = NA, QS_Subject_Area = NA, 
                         ASJC_Code = NA,
                         Subject = "Science Technology Other Topics", 
                         Restrictive_exclusion = NA)

subjects <- fread("data/literature_search/Subject_Categories.csv",
                  skip = 0) %>% 
  rbind(wos_special) %>% 
  mutate(include = case_when(
    .default = "exclude",
    Subject %in% c(      
      # Core ecology
      "Ecology",
      "Ecology, Evolution, Behavior and Systematics",
     # "Evolutionary Biology",
      "Conservation",
      "Biodiversity Conservation",
      "Biology",
      "Zoology",
      "Animal Science and Zoology",
      "Ornithology",
      "Entomology",
      "Insect Science",
      "Parasitology",
      "Mycology",
      "Nature and Landscape Conservation",
      "Forestry",
      "Plant Science",
      "Plant Sciences",
      "Soil Science",
    #  "Water Resources",
    #  "Water Science and Technology",
    #  "Ecological Modeling",
      "Remote Sensing",
      
      #"Aquatic Science",
      #"Fisheries",
      #"Marine & Freshwater Biology",
      #"Limnology",
      #"Oceanography",
      
      # Environmental science 
      "Environmental Science (all)",
      "Environmental Science (miscellaneous)",
      "Environmental Sciences",
      "Earth and Planetary Sciences (all)",
      "Earth and Planetary Sciences (miscellaneous)",
     # "Geosciences, Multidisciplinary",
    #  "Geography, Physical",
      "Earth-Surface Processes",
      "Global and Planetary Change",
 
      # Others
      "Agricultural and Biological Sciences (all)",
      "Agricultural and Biological Sciences (miscellaneous)",
      "Agriculture, Multidisciplinary",
     # "Agronomy",
    #  "Agronomy and Crop Science", 
      "Multidisciplinary Sciences", 
    "Science Technology Other Topics") ~ "include"
  ))

subjects[grepl("Science", Subject), ]$Subject


unique(subjects$Subject)
subjects <- subjects[Database == "WoS"]

# Let's do exclusion instead...
subject_exclusion <- paste0("WC=(", paste(subjects[Restrictive_exclusion == "Exclude", ]$Subject, collapse = " OR "),
                            ") NOT SU=(", paste(subjects[Restrictive_exclusion == "Exclude", ]$Subject, collapse = " OR "), ")")

subject_exclusion <- paste0("WC=(", paste(subjects[Restrictive_exclusion == "Exclude", ]$Subject, collapse = " OR "), ")")

subject_include <- paste0("WC=(", paste(subjects[Database == "WoS" & include == "include", ]$Subject, collapse = " OR "), ") AND ", 
                          "SU=(", paste(subjects[Database == "WoS" & include == "include", ]$Subject, collapse = " OR "), ")")


#

### topic exclusion --------------------------------------

c_te <- c(#"literature review", 
           # "systematic review",
            "meta-analysis", 
          "review")

te_string <-  paste0("TS = (", paste0(unique(c_te), collapse = " OR "), ")")
cat(te_string)



#### combine ----------------------------------------------------

doi_string
writeLines(doi_string, pipe("pbcopy"))

ts_string
writeLines(ts_string, pipe("pbcopy"))

ts_and_doi = paste(ts_string, "AND ", doi_string)
cat(ts_and_doi)
writeLines(ts_and_doi, pipe("pbcopy"))

ts_and_se = paste(ts_string, "AND ", subject_include)
cat(ts_and_se)
writeLines(ts_and_se, pipe("pbcopy"))

ts_and_se_and_doi = paste(ts_string, "AND ", doi_string, "AND ", subject_include)
cat(ts_and_se_and_doi)
writeLines(ts_and_se_and_doi, pipe("pbcopy"))

#I'm too paranoid that we loose important stuff with the exclusion (even though we most definitely not)
# 
# ts_and_se_and_te = paste(ts_string, "AND ", subject_include, "NOT", te_string)
# cat(ts_and_se_and_te)
# writeLines(ts_and_se_and_te, pipe("pbcopy"))
# 
# ts_and_se_and_doi_and_te = paste(ts_string, "AND ", doi_string, "AND ", subject_include, "NOT", te_string)
# cat(ts_and_se_and_doi_and_te)
# writeLines(ts_and_se_and_doi_and_te, pipe("pbcopy"))

### Adjust to scopus -----

#( TITLE-ABS-KEY ( ( megafauna OR megaherbivor* OR "keystone species" OR "ecosystem engineer*" OR herbivor* OR brows* OR graz*) AND ( introduc* OR reintroduc* OR restor* OR rewild* ) AND ( experiment* OR fenceline* OR exclosure* OR exclusion* OR control* OR result* ) ) ) AND ( LIMIT-TO ( SUBJAREA , "AGRI" ) OR LIMIT-TO ( SUBJAREA , "ENVI" ) OR LIMIT-TO ( SUBJAREA , "EART" ) OR LIMIT-TO ( SUBJAREA , "MULT" ) ) AND ( LIMIT-TO ( DOCTYPE , "ar" ) ) AND ( LIMIT-TO ( LANGUAGE , "English" ) )

