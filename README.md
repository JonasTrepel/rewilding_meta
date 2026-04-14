This repository contains all data and code associated with the manuscript:

**“Trophic rewilding promotes plant diversity and open habitats”** (under review).

--

**Repository Structure**

#### `data/`
Contains all datasets used in the manuscript.

- **`data/raw_data/`**  
  The raw dataset compiled from the literature (see below for column descriptions).

- **`data/literature_search/`**  
  Exported search results from **Web of Science** and **Scopus**.

- **`data/extraction/`**  
  Figure screenshots and extracted raw data from included papers for which numerical values could not be obtained directly from text or tables.

- **`data/covariates/`**  
  The **HerbiTraits** database from Lundgren et al., used as a covariate dataset.

- **`data/processed_data/`**  
  Cleaned dataset used for the analyses, including calculated effect sizes:  
  `clean_rewilding_meta_dataset.csv`

--

#### `scripts/`
Contains all scripts used for data preparation, processing, and analysis.

##### `scripts/prep/`
Scripts used for data preparation:

- `1_design_search_string.R` — Development of the literature search string  
- `2_data_extraction.R` — Extraction of data from figures and raw data  
- `3_calc_effect_sizes_and_clean.R` — Calculation of effect sizes and dataset cleaning

##### `scripts/analysis/`
Scripts used for statistical analyses:

- `1_run_models.R` — Runs the main meta-analytic models  
- `2_publication_bias.R` — Tests for potential publication bias

##### `scripts/viz/`
Scripts used to generate figures:

- `1_figures.R` — Produces the remaining figures used in the manuscript

--

#### `builds/`
Contains generated model outputs and plots produced during the analysis.

--

#### Raw Dataset: Column Descriptions

| Column name | Meaning |
|--------------|---------|
| `data_point_id` | Unique ID for each data point (do not fill manually) |
| `citation` | Unique citation for each study |
| `title` | Study title |
| `doi` | DOI of the paper |
| `herbivore_type` | Type of herbivore introduced (wild native, wild non-native, semi-domestic, livestock, other) |
| `study_notes` | Notes about the study |
| `data_notes` | Notes about the data |
| `design_notes` | Notes about the experimental design |
| `land_use_history_notes` | Description of land use prior to rewilding |
| `management_notes` | Notes on herbivore or site management |
| `experimental_mechanism` | Type of experimental approach |
| `experiment_timing` | Whether experiment was established before, during, or after herbivore introduction |
| `site_size_m2` | Size of the study site (m²) |
| `site_number` | Number of sites |
| `treatment_size_m2` | Size of treatment unit (e.g., exclosure) |
| `treatment_number` | Number of treatment replicates |
| `plot_size_m2` | Plot size |
| `plot_number` | Number of plots |
| `subplot_size_m2` | Subplot size (if plots were subdivided) |
| `subplot_number` | Number of subplots |
| `final_measure_scale` | Scale at which values were reported (subplot, plot, treatment, or site) |
| `continent` | Continent where the study was conducted |
| `latitude` | Latitude of the study area (e.g., centroid) |
| `longitude` | Longitude of the study area (e.g., centroid) |
| `coord_quality` | Accuracy of the geographic coordinates |
| `site_name` | Name of the study site |
| `introduced_herbivores` | Herbivore species introduced (Latin names where possible) |
| `resident_herbivores` | Herbivore species already present (Latin names where possible) |
| `herbivore_manipulation_notes` | Notes about herbivore introductions |
| `density_notes` | Notes about density estimates |
| `density_high_megafauna` | Herbivore density in high-megafauna treatment |
| `density_low_megafauna` | Herbivore density in low-megafauna treatment |
| `animal_density_units` | Units for animal density |
| `areal_density_units` | Area units associated with density values |
| `years_since_introduction` | Years since herbivore introduction |
| `years_since_treatment` | Years since treatment began |
| `treatment_duration_days` | Duration of treatment (days) |
| `time_series_clean` | If part of a time series, indicates measurement order |
| `data_year` | Year when the data were collected |
| `impact_mechanism` | Whether the impact measured was cumulative or mechanism-specific |
| `response_highest_level` | Highest hierarchical level of the response |
| `response_sphere` | Ecological sphere of the response (vegetation, animals, soil, etc.) |
| `species_class` | Type/class of species affected |
| `strata_or_soil_depth` | Soil depth or vegetation layer affected |
| `age_class` | Age class of affected species |
| `species_level` | Taxonomic level (species, genus, family, or higher) |
| `species_or_group` | Species or group responding to megafauna |
| `response` | Type of response (cover, density, diversity, concentration, etc.) |
| `unit` | Units of the response variable |
| `high_value_equals_high_response` | Indicates whether higher values represent stronger responses |
| `mean_type` | Type of mean reported (mean, median) |
| `error_type` | Type of error reported (SD, SE, variance, etc.) |
| `fig_num` | Figure number in the original paper |
| `fig_notes` | Notes regarding the figure screenshot |
| `raw_mean_high_megafauna` | Mean value in the high-megafauna treatment |
| `error_high_megafauna` | Error associated with high-megafauna treatment |
| `n_high_megafauna` | Sample size for high-megafauna treatment |
| `raw_mean_low_megafauna` | Mean value in the low-megafauna treatment |
| `error_low_megafauna` | Error associated with low-megafauna treatment |
| `n_low_megafauna` | Sample size for low-megafauna treatment |
| `plot_covariate` | Covariate for independent plots (e.g., fertilized, burned) |
| `digitization_notes` | Notes from figure digitization |
| `initially_entered_by` | Initials of the person who entered the data |
