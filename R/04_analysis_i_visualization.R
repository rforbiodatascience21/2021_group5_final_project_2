# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("broom")
library("ggplot2")
library("patchwork")

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
prostate_clean_aug <- read_rds(file = "data/03_prostate_clean_aug.rds.gz")


# Wrangle data ------------------------------------------------------------
prostate_clean_aug <- prostate_clean_aug %>% 
  mutate(outcome = case_when(outcome == 0 ~ "alive",
                             outcome == 1 ~ "dead"))

# Visualise data ----------------------------------------------------------

########################################
### Plots of pre-treatment variables ###
########################################

# could we create a heatmap here with correlation between the variables?

######################################
### Plots of treatment and outcome ###
######################################

## Distribution of alive/dead
prostate_clean_aug %>% 
  ggplot(mapping = aes(outcome,
                       fill = outcome)) +
  geom_bar(alpha = 0.7, show.legend = FALSE) +
  theme_minimal()

## Distribution of alive/dead for each treatment, scaled for better comparison
## across groups
prostate_clean_aug %>% 
  ggplot(mapping = aes(treatment,
                       fill = outcome)) +
  geom_bar(alpha = 0.8, position = "fill") +
  facet_wrap(~stage) +
  theme_minimal()


## Boxplot of tumor size for each outcome stratified on treatment
ggplot(data = prostate_clean_aug,
           mapping = aes(x = outcome,
                         y = tumor_size, 
                         fill = treatment_mg)) +
  geom_boxplot(alpha = 0.5, show.legend = TRUE) +
  theme_minimal()

# maybe another plot which really show that treatment 1mg is the best?

#############################################################################
### Plots of significant variables found by logistic regression (dose 1mg) ##
#############################################################################
p1 <- prostate_clean_aug %>% 
  filter(treatment_mg == "1.0") %>% 
  ggplot(mapping = aes(age,
                       color = outcome)) +
  geom_density() +
  theme_minimal()

p2 <- prostate_clean_aug %>% 
  filter(treatment_mg == "1.0") %>% 
  ggplot(mapping = aes(weight_index,
                       color = outcome)) +
  geom_density() +
  theme_minimal()

p3 <- prostate_clean_aug %>% 
  filter(treatment_mg == "1.0") %>% 
  ggplot(mapping = aes(tumor_size,
                       color = outcome)) +
  geom_density() +
  theme_minimal()

p4 <- prostate_clean_aug %>% 
  filter(treatment_mg == "1.0") %>% 
  ggplot(mapping = aes(CVD,
                       fill = outcome)) +
  geom_bar(position = "fill") +
  theme_minimal()

p4 + p1 / p2 / p3

# Write data --------------------------------------------------------------
write_tsv(...)
ggsave(...)