# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
prostate_clean_aug <- read_rds(file = "data/03_prostate_clean_aug.rds.gz")


# Wrangle data ------------------------------------------------------------

# 
prostate_long_nested <- prostate_clean_aug %>%
  select(-status, -treatment, -age_group) %>%
  pivot_longer(cols = c(-patno, -outcome), 
               names_to = "variable", 
               values_to = "value") %>%
  group_by(variable) %>%
  nest() %>%
  ungroup()

prostate_clean_aug %>%
  select(-status, -treatment, -age_group) %>%
  group_by() %>% 
  nest()


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



## Maybe try another variable to predict outcome - fx treatment?


# What can we say about this? 


# Visualise data ----------------------------------------------------------
prostate_clean_aug %>% ...


# Write data --------------------------------------------------------------
write_tsv(...)

