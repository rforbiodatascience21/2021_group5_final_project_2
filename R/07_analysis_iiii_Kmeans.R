# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
prostate_clean_aug <- read_rds(file = "data/03_prostate_clean_aug.rds.gz")


# Wrangle data ------------------------------------------------------------
prostate_clean_aug %>% ...


# Model data --------------------------------------------------------------
prostate_clean_aug %>% ...


# Visualise data ----------------------------------------------------------
prostate_clean_aug %>% ...


# Write data --------------------------------------------------------------
write_tsv(...)
ggsave(...)