# Clear workspace ---------------------------------------------------------
rm(list = ls())

# Load libraries ----------------------------------------------------------
library("tidyverse")

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data and wrangle----------------------------------------------------
prostate_clean_aug <- read_tsv(file = "data/03_prostate_clean_aug.tsv.gz")

# Factorizing variables 
prostate_clean_aug <- prostate_clean_aug %>%
  mutate(bone_mets = factor(bone_mets),
         CVD = factor(CVD),
         treatment = factor(treatment),
         stage = factor(stage),
         performance_lvl = factor(performance_lvl),
         EKG_lvl = factor(EKG_lvl))

# Summary statistic 
prostate_clean_aug %>% 
  select(where(is.numeric), -patient_ID) %>% 
  tidy()
