# Clear workspace ---------------------------------------------------------
rm(list = ls())

# Load libraries ----------------------------------------------------------
library("tidyverse")

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data and wrangle----------------------------------------------------
my_data <- read_tsv(file = "data/03_prostate_clean_aug.tsv.gz")

num_data<- select(my_data,age,weight_index,sbp,dbp,hg,tumor_size,outcome,acid_phosphatase,treatment_mg)

longer_data_2 <- pivot_longer(num_data,cols = -"outcome",
                              names_to = "parameter", 
                              values_to = "value") %>% 
                 mutate(outcome = factor(outcome)) 

alive_long_data <- longer_data_2 %>% filter(outcome == 0)
death_long_data <- longer_data_2 %>% filter(outcome == 1)

# Summary------------------------------------------------------------------
table_alive <- alive_long_data %>% 
  group_by(parameter) %>% 
  summarise(n = n(),
            mean_alive = mean(value),
            sigma_alive = sd(value))

table_death <- death_long_data %>% 
  group_by(parameter) %>% 
  summarise(n = n(),
            mean_death = mean(value),
            sigma_death = sd(value))

summary_table<- full_join(table_alive,table_death,by="parameter")

# Write data --------------------------------------------------------------
write_tsv(x = summary_table,
          file = "data/summary_table")
