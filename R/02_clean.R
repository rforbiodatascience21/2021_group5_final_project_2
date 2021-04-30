# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
my_data <- read_tsv(file = "data/01_my_data.tsv")


# Wrangle data ------------------------------------------------------------
## add a new column, outcome, to divide status into dead or alive, which is easy to analyze further
my_data_clean <- my_data %>%
  drop_na() %>%
  mutate(stage = factor(stage)) %>%
  mutate(patno = factor(patno)) %>%
  mutate(bm = factor(bm)) %>%
  mutate(hx = factor(hx)) %>%
  mutate(outcome = case_when(status == "alive" ~ 0,
                             status != "alive" ~ 1)) %>%
  mutate(outcome = factor(outcome))


# Making the rx variable to a continuous variable "Treatment" 
my_data_clean <- my_data_clean %>%
 mutate(Treatment = str_sub(rx, 1, 3)) %>% 
 mutate(Treatment = case_when(Treatment == "pla" ~ 0,
                               TRUE  ~ as.numeric(Treatment)))

# If there is a better way to do it, pleace change :) 


# Write data --------------------------------------------------------------
saveRDS(object = my_data_clean,
          file = "data/02_my_data_clean.rds")