# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("broom")
library("cowplot")
library("ggplot2")
library("GGally")

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
prostate_clean_aug <- read_rds(file = "data/03_prostate_clean_aug.rds.gz")


# Wrangle data ------------------------------------------------------------
prostate_clean_aug %>% ...


# Visualise data ----------------------------------------------------------

## Barplot/histogram of status colored by treatment
p1 <- prostate_clean_aug %>%
  distinct(patno,stage,treatment,status) %>%
  count(status,treatment) %>%
  ggplot(aes(y = status,x = n))+
  geom_col(aes(fill = treatment),position = "dodge",
           alpha = 0.5)+
  theme_classic(base_family = "Avenir",
                base_size = 12)+
  theme(legend.position = "none")

## This plot doesn't make sense?
p2 <- prostate_clean_aug %>%
  distinct(patno,stage,hg,status) %>%
  ggplot(aes(y = status, x = hg))+
  geom_point(aes(colour = stage))+
  theme_classic(base_family = "Avenir",
                base_size = 12)+
  theme(legend.position = "bottom")

## This plot doesn't make sense?
prostate_clean_aug %>% 
  drop_na %>% 
  ggplot(mapping = aes(x = treatment_mg,
                       y = outcome,
                       fill = age_group)) +
  geom_boxplot()

## This doesn't give anything?
# Investigating correlation 
prostate_clean_aug %>% select(.data = ., treatment_mg, age, hg) %>% 
  cor(use = "pairwise.complete.obs", method = "pearson")

## Not supposed to use this package
ggpairs(prostate_clean_aug,
        mapping = aes(color = outcome),
        columns = c("Treatment", "hg", "age", "stage", "outcome"))

## Not really useful ... 
ggplot(data = prostate_clean_aug,
       mapping = aes(x = age,
                     y = treatment_mg,
                     color = outcome)) +
  geom_point()


# Write data --------------------------------------------------------------
write_tsv(...)
ggsave(...)