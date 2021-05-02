# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
prostate_clean_aug <- read_rds(file = "data/03_prostate_clean_aug.rds.gz")


# Wrangle data ------------------------------------------------------------
prostate_log <- prostate_clean_aug %>% 
  select(-treatment, -status, -age_group)

## Create long nested data of <fct> variables
long_nested_fct <- prostate_log %>%
  select(patno, outcome, stage, CVD, bone_mets, treatment_mg) %>%
  pivot_longer(cols = c(-patno, -outcome), 
               names_to = "variable", 
               values_to = "value") %>%
  group_by(variable) %>%
  nest() %>%
  ungroup()

## Create long nested data of <dbl> variables
long_nested_dbl <- prostate_log %>%
  select(-stage, -CVD, -bone_mets, -treatment_mg) %>%
  pivot_longer(cols = c(-patno, -outcome), 
               names_to = "variable", 
               values_to = "value") %>%
  group_by(variable) %>%
  nest() %>%
  ungroup()

## Combine to one long nested
long_nested <- bind_rows(long_nested_dbl, long_nested_fct)

# Model data --------------------------------------------------------------
## Creating a logistic model

prostate_logistic <- prostate_clean_aug %>% 
  glm(outcome ~ treatment_mg,
      data = .,
      family = binomial(link = "logit"))


prostate_logistic <- prostate_clean_aug %>% 
  drop_na %>% 
  glm(outcome ~ stage + CVD + hg + tumor_size + age,
      data = .,
      family = binomial(link = "logit"))
tidy(prostate_logistic)



# Visualise data ----------------------------------------------------------
prostate_clean_aug %>% ...


# Write data --------------------------------------------------------------
write_tsv(...)

