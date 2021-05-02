# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
load(file = "results/06_mdl_pca_fit.RData")
prostate_clean_aug <- read_rds(file = "data/03_prostate_clean_aug.rds.gz")


# Wrangle data ------------------------------------------------------------
## Remove rows containing NA
prostate_clean_sub <- prostate_clean_aug %>% 
  select(-patno, -treatment, -status, -age_group) %>% 
  drop_na()

# Model data --------------------------------------------------------------
# K-means clustering

## Extract the first two principal components
points <- pca_fit %>% 
  augment(prostate_clean_sub) %>% 
  select(.fittedPC1, .fittedPC2)

## K-means clustering with k = 1..6 
## and create tidied, glanced and augmented data
kclusts <- 
  tibble(k = 1:6) %>%
  mutate(
    kclust = map(k, ~kmeans(points, .x)),
    tidied = map(kclust, tidy),
    glanced = map(kclust, glance),
    augmented = map(kclust, augment, points)
  )

## Turn into three separate data sets using tidy(), augment() and glance()
clusters <- 
  kclusts %>%
  unnest(cols = c(tidied))

assignments <- 
  kclusts %>% 
  unnest(cols = c(augmented))

clusterings <- 
  kclusts %>%
  unnest(cols = c(glanced))


# Visualise data ----------------------------------------------------------
## Create 6 plots with varying number of clusters k=1..9
p1 <- ggplot(assignments, aes(x = .fittedPC1, y = .fittedPC2)) +
  geom_point(aes(color = .cluster), alpha = 0.8) + 
  facet_wrap(~ k)
p1
## With centers of clusters - don't work for me yet
p2 <- p1 + geom_point(data = clusters, size = 8, shape = "x")
p2

## Variance within clusters, scree-plot 
ggplot(clusterings, aes(k, tot.withinss)) +
  geom_line() +
  geom_point()+ 
  labs( x = "Number of clusters, k", y = "Total within cluster variation")


# Write data --------------------------------------------------------------
write_tsv(...)
ggsave(...)