# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("readxl")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
prostate_raw_1 <- read_xlsx(path = "data/_raw/prostate.xlsx")
prostate_raw_2 <- read_xlsx(path = "data/_raw/prostate_additional_data.xlsx")

# Wrangle data ------------------------------------------------------------
# Joining the two parts of the data set by patient number 
prostate_data <- full_join(prostate_raw_1, 
                           prostate_raw_2, 
                           by = "patno")


# Write data --------------------------------------------------------------
write_tsv(x = prostate_data,
          file = "data/01_prostate_data.tsv.gz")