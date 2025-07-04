# AusStoich Data Import
library(here)
library(tidyverse)

# Data import ----
all_data <- read_csv(
  file = here('Inputs', 'ausdata_merged_v3_SQ.csv'),
  na = c('', 'NA', '#N/A','uncertain'),
  col_types = cols(
    woodiness = readr::col_factor(c('0', '1')),
    reclass_life_history = readr::col_factor(c('short', 'long')),
    putative_BNF = readr::col_factor(c('0', '1')),
    myc_type = readr::col_factor(c('AM', 'EcM', 'EcM-AM', 'ErM', 'NM', 'NM-AM'))
  )
)


all_data <- all_data %>%
  mutate(
    ln_NP_ratio = log(NP_ratio),
    ln_CN_ratio = log(CN_ratio),
    ln_CP_ratio = log(CP_ratio)
  ) %>%
  relocate(
    NP_ratio, CN_ratio, CP_ratio,
    ln_NP_ratio, ln_CN_ratio, ln_CP_ratio,
    .after = leaf_C_per_dry_mass
  )

# Add proportional variation metric for ratios
all_data <- aus_data %>%
  group_by(species_binom) %>%
  mutate(
    sd_N = sd(leaf_N_per_dry_mass, na.rm = TRUE),
    sd_P = sd(leaf_P_per_dry_mass, na.rm = TRUE),
    sd_C = sd(leaf_C_per_dry_mass, na.rm = TRUE),
    sd_ln_NP = sd(ln_NP_ratio, na.rm = TRUE),
    sd_ln_CP = sd(ln_CP_ratio, na.rm = TRUE),
    sd_ln_CN = sd(ln_CN_ratio, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  relocate(sd_N, sd_P, sd_C,
    sd_ln_NP, sd_ln_CN, sd_ln_CP,
    .after = ln_CP_ratio
  )

# LCVP name standardization - derivation in phylogeny script
naming_corrections <- read_csv(here('Inputs', 'all_naming_corrections.csv'))

all_corrected_data <- all_data %>%
  left_join(
    naming_corrections,
    by = c(
      "species_binom" = "species_before_correction",
      "genus" = "genus_before_correction",
      "family" = "family_before_correction")
  ) %>%
  mutate(
    species_binom = ifelse(!is.na(species_after_correction), species_after_correction, species_binom),
    genus = ifelse(!is.na(genus_after_correction), genus_after_correction, genus),
    family = ifelse(!is.na(family_after_correction), family_after_correction, family)
  ) %>%
  select(-species_after_correction, -genus_after_correction, -family_after_correction)

# Add myc_type assignment post naming correction
aus_data <- aus_data %>%
  mutate(
    myc_type = as.character(myc_type), 
    myc_type = case_when(
      species_binom == "Chenopodium_nutans" ~ "NM-AM",
      species_binom == "Epaltes_australis" ~ "AM",
      species_binom == "Notogrammitis_billardierei" ~ "AM",
      TRUE ~ myc_type
    ),
    myc_type = factor(myc_type)
  )

# Outliers (only Fiona-confirmed, see 03-EDA for all candidates)
outliers <- all_corrected_data |> filter(leaf_N_per_dry_mass > 60)
outliers <- all_corrected_data |>
  filter(leaf_P_per_dry_mass > 9) |>
  bind_rows(outliers)
outliers <- all_corrected_data |>
  filter(leaf_C_per_dry_mass > 650 | leaf_C_per_dry_mass < 250) |>
  bind_rows(outliers)
outliers <- all_corrected_data |>
  filter(is.na(SN_total_0_30)) |>
  bind_rows(outliers)

outliers_removed_data <- all_corrected_data |> setdiff(outliers)

# Set aus_data to use in subsequent scripts and remove intermediates
aus_data <- outliers_removed_data |> relocate(species_binom, .before = woodiness)
rm(all_data, outliers, naming_corrections, all_corrected_data, outliers_removed_data)