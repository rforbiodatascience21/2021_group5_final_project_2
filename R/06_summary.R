# Clear workspace ---------------------------------------------------------
rm(list = ls())

# Load libraries ----------------------------------------------------------
library("tidyverse")

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data and wrangle----------------------------------------------------
my_data <- read_tsv(file = "data/03_prostate_clean_aug.tsv.gz")

num_data<- my_data %>% select(patient_ID, stage, outcome, where(is.numeric), -treatment_mg)

longer_data <- num_data %>% pivot_longer(cols = c(-patient_ID, -stage, -outcome), 
                            names_to = "variables", 
                            values_to = "values")

# Summary (total)----------------------------------------------------------
total_table <- longer_data %>% group_by(variables) %>% 
                summarise(n = n(),
                          max_value = max(values),
                          min_value = min(values),
                          mean_value = mean(values),
                          sigma_value = sd(values))

# Summary by outcome ------------------------------------------------------

alive_long_data <- longer_data %>% filter(outcome == 0)
death_long_data <- longer_data %>% filter(outcome == 1)


table_alive <- alive_long_data %>% group_by(variables) %>% 
               summarise(n = n(),
                         max_value = max(values),
                         min_value = min(values),            
                         mean_alive = mean(values),
                         sigma_alive = sd(values))

table_death <- death_long_data %>% group_by(variables) %>% 
               summarise(n = n(),
                         max_value = max(values),
                         min_value = min(values),            
                         mean_alive = mean(values),
                         sigma_alive = sd(values))


SummaryByOutcome_table<- full_join(table_alive,table_death,by="variables")

# Summary by stage ------------------------------------------------------

stage3_long_data <- longer_data %>% filter(stage == 3)
stage4_long_data <- longer_data %>% filter(stage == 4)


table_stage3 <- stage3_long_data %>% group_by(variables) %>% 
               summarise(n = n(),
                         max_value = max(values),
                         min_value = min(values),            
                         mean_alive = mean(values),
                         sigma_alive = sd(values))

table_stage4 <- stage4_long_data %>% group_by(variables) %>% 
               summarise(n = n(),
                         max_value = max(values),
                         min_value = min(values),            
                         mean_alive = mean(values),
                         sigma_alive = sd(values))


SummaryByStage_table<- full_join(table_stage3,table_stage4,by="variables")


# Write data --------------------------------------------------------------
write_tsv(x = summary_table,
          file = "data/summary_table")
