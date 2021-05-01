# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("gridExtra")
library("fs")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
prostate_clean <- read_tsv(file = "data/02_prostate_clean.tsv.gz")


# Wrangle data ------------------------------------------------------------
## Add a new binary <fct>-variable "outcome" based on the status is alive or dead
prostate_clean_aug <- prostate_clean %>%
  mutate(outcome = case_when(status == "alive" ~ 0,
                             status != "alive" ~ 1),
         outcome = factor(outcome))

## Add numeric variable of estrogen dose
prostate_clean_aug <- prostate_clean_aug %>%
  mutate(treatment_mg = str_sub(treatment, 1, 3),
         treatment_mg = str_replace(treatment_mg, "pla", "0"),
         treatment_mg = as.numeric(treatment_mg)) 

## Add new variable age_group (mean = 71.57)
prostate_clean_aug <- prostate_clean_aug %>% 
  mutate(age_group = case_when(age <= 71 ~ "Young",
                               age > 71 ~ "Old"))


# Write data --------------------------------------------------------------
write_tsv(x = prostate_clean_aug,
          file = "data/03_prostate_clean_aug.tsv.gz")
