# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("readxl")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
prostate_raw <- read_xlsx(path = "data/_raw/prostate.xlsx")


# Wrangle data ------------------------------------------------------------

# delete the columns that we do not need in the analysis
my_data <- prostate_raw  %>%
  select(-sdate, -dtime, -pf, -ekg)


# Write data --------------------------------------------------------------
write_tsv(x = my_data,
          file = "data/01_my_data.tsv")