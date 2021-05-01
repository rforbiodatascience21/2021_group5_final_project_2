# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
prostate_clean_aug <- read_tsv(file = "data/03_my_data_clean_aug.tsv")


# Wrangle data ------------------------------------------------------------
prostate_clean_aug %>% ...


# Model data --------------------------------------------------------------
#Creating a logistic model
## We will try to use the age of the patients to predict the risk for the outcome 
model_age <- my_data_clean_aug %>% 
  glm(outcome ~ age,
      data = .,
      family = binomial(link = "logit"))

# What can we say about this? 
summary(model_age)


# Visualise data ----------------------------------------------------------
prostate_clean_aug %>% ...


# Write data --------------------------------------------------------------
write_tsv(...)

