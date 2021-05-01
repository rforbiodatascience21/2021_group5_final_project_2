# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
prostate_data <- read_tsv(file = "data/01_prostate_data.tsv.gz")


# Wrangle data ------------------------------------------------------------
## Remove columns that we do not need in the analysis
prostate_clean <- prostate_data  %>%
  select(-sdate, -dtime, -pf, -ekg, -sg)

## Rename variables
prostate_clean <- prostate_clean %>% 
  rename(treatment = rx,
         weight_index = wt,
         tumor_size = sz,
         acid_phosphatase = ap,
         bone_mets = bm,
         CVD = hx)
 
## Change the type of four variables to factor
prostate_clean <- prostate_clean %>%
  mutate(stage = factor(stage),
         patno = factor(patno),
         bone_mets = factor(bone_mets),
         CVD = factor(CVD))
# drop_na()


# Write data --------------------------------------------------------------
## We use write_rds instead of write_tsv to keep information of type of variables
write_rds(x = prostate_clean,
          file = "data/02_prostate_clean.rds.gz")