# AusStoich Data Import
library(here)
library(tidyverse)

#import data
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

#add logged nutrients
all_data <- all_data %>%
  mutate(
    ln_leaf_N = log(leaf_N_per_dry_mass),
    ln_leaf_P = log(leaf_P_per_dry_mass),
    ln_leaf_C = log(leaf_C_per_dry_mass),
    ln_NP_ratio = log(NP_ratio),
    ln_CN_ratio = log(CN_ratio),
    ln_CP_ratio = log(CP_ratio)
  ) %>%
  relocate(
    ln_leaf_N, ln_leaf_P, ln_leaf_C, 
    NP_ratio, CN_ratio, CP_ratio,
    ln_NP_ratio, ln_CN_ratio, ln_CP_ratio,
    .after = leaf_C_per_dry_mass
  )

#add proportional variation metrics
#for raw leaf nutrients: use SE
#for ratios: sd is proportional variation metric, see Isles 2020
#CV by species, genera, and family
all_data <- all_data %>%
  group_by(species_binom) %>%
  mutate(
    SE_N = std_error(leaf_N_per_dry_mass),
    SE_P = std_error(leaf_P_per_dry_mass),
    SE_C = std_error(leaf_C_per_dry_mass),
    CV_N_sp = sd(leaf_N_per_dry_mass, na.rm = TRUE) / mean(leaf_N_per_dry_mass,
                                                           na.rm = TRUE),
    CV_P_sp = sd(leaf_P_per_dry_mass, na.rm = TRUE) / mean(leaf_P_per_dry_mass,
                                                           na.rm = TRUE),
    CV_C_sp = sd(leaf_C_per_dry_mass, na.rm = TRUE) / mean(leaf_C_per_dry_mass,
                                                           na.rm = TRUE),
    sd_ln_NP_sp = sd(ln_NP_ratio, na.rm = TRUE),
    sd_ln_CP_sp = sd(ln_CP_ratio, na.rm = TRUE),
    sd_ln_CN_sp = sd(ln_CN_ratio, na.rm = TRUE),
    SE_ln_NP_sp = std_error(ln_NP_ratio),
    SE_ln_CP_sp = std_error(ln_CP_ratio),
    SE_ln_CN_sp = std_error(ln_CN_ratio),
  ) %>%
  ungroup() %>%
  relocate(SE_N, SE_P, SE_C,
    CV_N_sp, CV_P_sp, CV_C_sp,
    sd_ln_NP_sp, sd_ln_CN_sp, sd_ln_CP_sp,
    SE_ln_NP_sp, SE_ln_CN_sp, SE_ln_CP_sp,
    .after = ln_CP_ratio
  )

all_data <- all_data %>%
  group_by(genus) %>%
  mutate(CV_N_gen = sd(leaf_N_per_dry_mass, na.rm = TRUE) / mean(leaf_N_per_dry_mass,
                                                             na.rm = TRUE),
         CV_P_gen = sd(leaf_P_per_dry_mass, na.rm = TRUE) / mean(leaf_P_per_dry_mass,
                                                             na.rm = TRUE),
         CV_C_gen = sd(leaf_C_per_dry_mass, na.rm = TRUE) / mean(leaf_C_per_dry_mass,
                                                             na.rm = TRUE),
         sd_lnNP_gen = sd(ln_NP_ratio, na.rm = TRUE),
         sd_lnCN_gen = sd(ln_CN_ratio, na.rm = TRUE), 
         sd_lnCP_gen = sd(ln_CP_ratio, na.rm = TRUE)
         ) %>%
  ungroup() %>%
  relocate(CV_N_gen, CV_P_gen, CV_C_gen,
           sd_lnNP_gen, sd_lnCN_gen, sd_lnCP_gen,
           .after = sd_ln_CP_sp)

all_data <- all_data %>%
  group_by(family) %>%
  mutate(CV_N_fam = sd(leaf_N_per_dry_mass, na.rm = TRUE) / mean(leaf_N_per_dry_mass,
                                                             na.rm = TRUE),
         CV_P_fam = sd(leaf_P_per_dry_mass, na.rm = TRUE) / mean(leaf_P_per_dry_mass,
                                                             na.rm = TRUE),
         CV_C_fam = sd(leaf_C_per_dry_mass, na.rm = TRUE) / mean(leaf_C_per_dry_mass,
                                                             na.rm = TRUE),
         sd_lnNP_fam = sd(ln_NP_ratio, na.rm = TRUE),
         sd_lnCN_fam = sd(ln_CN_ratio, na.rm = TRUE), 
         sd_lnCP_fam = sd(ln_CP_ratio, na.rm = TRUE)
         ) %>%
  ungroup()  %>%
  relocate(CV_N_fam, CV_P_fam, CV_C_fam,
           sd_lnNP_fam, sd_lnCN_fam, sd_lnCP_fam,
           .after = sd_lnCP_gen)


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

#add myc_type assignment post naming correction
all_data <- all_data %>%
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

#set aus_data to use in subsequent scripts and remove intermediates
aus_data <- outliers_removed_data |> relocate(species_binom, .before = woodiness)
rm(all_data, outliers, naming_corrections, all_corrected_data, outliers_removed_data)

#write csv
#write.csv(aus_data, file = "aus_data.csv")