# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("readxl")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
prostate_raw <- read_xlsx(path = "data/_raw/prostate.xlsx")
prostate_raw_1 <- read_xlsx(path = "data/_raw/prostate_additional_data.xlsx")

# Wrangle data ------------------------------------------------------------
# Joining the two parts of the data set by patient number 
my_data <- full_join(prostate_raw, prostate_raw_1, by = "patno")


# delete the columns that we do not need in the analysis
my_data <- my_data  %>%
  select(-sdate, -dtime, -pf, -ekg, -sg)


# Write data --------------------------------------------------------------
write_tsv(x = my_data,
          file = "data/01_my_data.tsv")
