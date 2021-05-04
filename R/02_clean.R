# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
prostate_data <- read_tsv(file = "data/01_prostate_data.tsv.gz")


# Wrangle data ------------------------------------------------------------
## Remove columns that we wont use in the analysis
prostate_clean <- prostate_data  %>%
  select(-sdate, -dtime)

## Rename variables
prostate_clean <- prostate_clean %>% 
  rename(patient_ID = patno, 
         treatment = rx,
         weight_index = wt,
         performance = pf,
         CVD = hx,
         EKG = ekg,
         hemoglobin = hg,
         tumor_size = sz,
         sg_index = sg,
         acid_phosphatase = ap,
         bone_mets = bm)

# NA values
sum(is.na(prostate_clean))
 
## Change the type of four variables to factor
prostate_clean <- prostate_clean %>%
  mutate(patient_ID = factor(patient_ID),
         stage = factor(stage),
         bone_mets = factor(bone_mets),
         CVD = factor(CVD))


# Write data --------------------------------------------------------------
## We use write_rds instead of write_tsv to keep information of type of variables
write_rds(x = prostate_clean,
          file = "data/02_prostate_clean.rds.gz")