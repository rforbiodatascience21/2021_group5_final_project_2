# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("ggplot2")
library("GGally")

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
my_data_clean_aug <- read_tsv(file = "data/02_my_data_clean.tsv")
#change back to "03_my_data_clean_aug"

# Wrangle data ------------------------------------------------------------
my_data_clean_aug %>% ...


# Model data
#Creating a logistic model
## We will try to use the age of the patients to predict the risk for the outcome 
model_age <- my_data_clean_aug %>% glm(outcome ~ age,
                                       data = .,
                                       family = binomial(link = "logit"))
# What can we say about this? 
summary(model_age)

# Visualise data ----------------------------------------------------------
my_data_clean_aug <- my_data_clean_aug %>% 
  mutate(outcome = factor(outcome)) 

ggplot(data = my_data_clean_aug,
       mapping = aes(x = Treatment,
                     y = outcome,
                     fill = age_group)) +
  geom_boxplot()

# Investigating correlation 
my_data_clean_aug %>% select(.data = ., Treatment, age, hg)%>% 
  cor( use = "pairwise.complete.obs", method = "pearson")

ggpairs(my_data_clean_aug,
        mapping = aes(color = outcome),
        columns = c("Treatment", "hg", "age", "stage", "outcome"))
#Not really useful ... 


# Write data --------------------------------------------------------------
write_tsv(...)
ggsave(...)