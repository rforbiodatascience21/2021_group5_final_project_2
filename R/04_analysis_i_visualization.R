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

### Plots of pre-treatment variables ###


### Plots of treatment and outcome ###

## Distribution of alive/dead
prostate_clean_aug %>% 
  ggplot(mapping = aes(outcome,
                       fill = outcome)) +
  geom_bar(alpha = 0.8, show.legend = FALSE) +
  theme_minimal()

## Distribution of alive/dead for each treatment, scaled for better comparison
## across groups
prostate_clean_aug %>% 
  ggplot(mapping = aes(treatment,
                       fill = outcome)) +
  geom_bar(alpha = 0.8, position = "fill") +
  facet_wrap(~stage)

## Boxplots of tumor_size for each status after treatment with 1mg
prostate_clean_aug %>% 
  ggplot(mapping = aes(age,
                       tumor_size)) +
  geom_point() +
  geom_smooth()
  #facet_wrap(~treatment_mg) +
  #coord_flip()


## Barplot/histogram of status colored by treatment
prostate_clean_aug %>%
  distinct(patno,stage,treatment,status) %>%
  count(status,treatment) %>%
  ggplot(aes(y = status,x = n))+
  geom_col(aes(fill = treatment),position = "dodge",
           alpha = 0.5)+
  theme_classic(base_family = "Avenir",
                base_size = 12)+
  theme(legend.position = "none")


## Boxplot of tumor size for each outcome stratified on treatment
ggplot(data = prostate_clean_aug,
           mapping = aes(x = outcome,
                         y = tumor_size, 
                         fill = treatment_mg)) +
  geom_boxplot(alpha = 0.5, show.legend = TRUE) +
  theme_minimal()


## Boxplots colored according to treatment
ggplot(data = prostate_clean_aug,
           mapping = aes(x = outcome,
                         y= age, fill=treatment_mg)) +
  geom_boxplot(alpha = 0.5, show.legend = FALSE) +
  theme_classic()

### Plots of those with treatment 1mg
## Look at one treatment to see which variables play a role in the outcome
prostate_clean_aug %>% 
  filter(treatment_mg == "1.0") %>% 
  ggplot() +
  geom_density(mapping = aes(age,
                             color = outcome))

prostate_clean_aug %>% 
  filter(treatment_mg == "1.0") %>% 
  ggplot() +
  geom_density(mapping = aes(age,
                             color = outcome))
 

# Write data --------------------------------------------------------------
write_tsv(...)
ggsave(...)