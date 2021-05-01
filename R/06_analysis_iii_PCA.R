# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
prostate_clean_aug <- read_rds(file = "data/")


# Wrangle data ------------------------------------------------------------
prostate_clean_aug %>% ...


# Model data --------------------------------------------------------------
# PCA analysis
pca_fit <- prostate_clean_aug %>%
  select(where(is.numeric)) %>%
  prcomp(scale = TRUE)

pca_fit %>%
  augment(prostate_clean_aug) %>%
  ggplot(aes(.fittedPC1, .fittedPC2, color = outcome))+
  geom_point(size = 1.5)+
  scale_color_manual(
    values = c("1" = "#D55E00", "0" = "#0072B2")
  )+
  theme_half_open(12) + background_grid()

pca_fit %>%
  tidy(matrix = "rotation")


# Visualise data ----------------------------------------------------------
prostate_clean_aug %>% ...


# Write data --------------------------------------------------------------
write_tsv(...)
