# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
prostate_clean <- read_tsv(file = "data/02_prostate_clean.tsv.gz")


# Wrangle data ------------------------------------------------------------

##########################
## Clean augmented data ##
##########################

## Add new binary <fct>-variable "outcome" based on the status is alive or dead
prostate_clean_aug <- prostate_clean %>%
  mutate(outcome = case_when(status == "alive" ~ 0,
                             status != "alive" ~ 1)) %>% 
  relocate(outcome, 
           .after = status)

## Add numeric variable of estrogen dose
prostate_clean_aug <- prostate_clean_aug %>%
  mutate(treatment_mg = str_sub(treatment, 1, 3),
         treatment_mg = str_replace(treatment_mg, "pla", "0"),
         treatment_mg = as.double(treatment_mg)) %>% 
  relocate(treatment_mg, 
           .after = treatment)

## Add variable of performance level
prostate_clean_aug <- prostate_clean_aug %>% 
  mutate(performance_lvl = case_when(performance == "normal activity" ~ 4,
                                     performance == "in bed < 50% daytime" ~ 3,
                                     performance == "in bed > 50% daytime" ~ 2,
                                     performance == "confined to bed" ~ 1)) %>% 
  relocate(performance_lvl, 
           .after = performance)

# Add variable of EKG level  
prostate_clean_aug <- prostate_clean_aug %>%   
  mutate(EKG_lvl = case_when(EKG == "normal" ~ 0,
                             EKG == "benign" ~ 1,
                             EKG == "rhythmic disturb & electrolyte ch" ~ 2,
                             EKG == "heart block or conduction def" ~ 3,
                             EKG == "heart strain" ~ 4,
                             EKG == "old MI" ~ 5,
                             EKG == "recent MI" ~ 6))%>% 
  relocate(EKG_lvl, 
           .after = EKG)

# Find mean of age
prostate_clean_aug %>% 
  drop_na() %>% 
  summarize(mean(age))

## Add new variable age_group (mean = 71.5)
prostate_clean_aug <- prostate_clean_aug %>% 
  mutate(age_group = case_when(age <= 71 ~ "Young",
                               age > 71 ~ "Old")) %>% 
  relocate(age_group, 
           .after = age)

## Add log-transformed variable of the acid_phosphatase 
prostate_clean_aug <- prostate_clean_aug %>% 
  mutate(acid_phosphatase_log = log(acid_phosphatase)) %>% 
  relocate(acid_phosphatase_log, 
           .after = acid_phosphatase)

##################################
## Data for logistic regression ##
##################################

prostate_data_logistic <- prostate_clean_aug %>% 
  select(-where(is.character), 
         -patient_ID, 
         -performance_lvl,
         -acid_phosphatase) %>% 
  drop_na

##############################
## Data for PCA and K-means ##
##############################

prostate_data_pca <- prostate_clean_aug %>%
  select(-where(is.character), 
         -patient_ID,
         -treatment_mg, 
         -acid_phosphatase_log) %>% 
  drop_na()




# Write data --------------------------------------------------------------
write_tsv(x = prostate_clean_aug,
          file = "data/03_prostate_clean_aug.tsv.gz")
write_tsv(x = prostate_data_logistic,
          file = "data/03_prostate_data_logistic.tsv.gz")
write_tsv(x = prostate_data_pca,
          file = "data/03_prostate_data_pca.tsv.gz")
