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
  mutate(outcome = case_when(status == "alive" ~ 0,
                             status != "alive" ~ 1))
#my_data_clean <- my_data %>%
 # drop_na() %>%
  #mutate(stage = factor(stage)) %>%
  #mutate(outcome = case_when(status == "alive" ~ 0,
   #                          status == "dead - pulmonary embolus" ~ 1,
    #                         status == "dead - prostatic ca" ~ 1,
     #                        status == "dead - heart or vascular" ~ 1,
      #                       status == "dead - cerebrovascular" ~ 1,
       #                      status == "dead - other ca" ~ 1,
        #                     status == "dead - respiratory disease" ~ 1,
         #                    status == "dead - other specific non-ca" ~ 1,
          #                   status == "dead - unspecified non-ca" ~ 1,
           #                  status == "dead - unknown cause" ~ 1))

# Making the rx variable to a continuous variable "treatment" 
my_data_clean <- my_data_clean %>%
 mutate(Treatment = str_sub(rx, 1, 3)) %>% 
 mutate(Treatment = case_when(Treatment == "pla" ~ 0,
                               TRUE  ~ as.numeric(Treatment)))

# placebo must be 0 - dunno how to 

# Write data --------------------------------------------------------------
write_tsv(x = my_data_clean,
          file = "data/02_my_data_clean.tsv")