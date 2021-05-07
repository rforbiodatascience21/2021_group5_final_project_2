# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
load(file = "results/06_mdl_pca_fit.RData")
prostate_clean_aug <- read_tsv(file = "data/03_prostate_clean_aug.tsv.gz")


# Wrangle data ------------------------------------------------------------
prostate_data_pca <- prostate_clean_aug %>%
  select(-where(is.character), 
         -c(patient_ID, treatment_mg, acid_phosphatase)) %>% 
  mutate(outcome = factor(outcome),
         performance_lvl = factor(performance_lvl),
         EKG_lvl = factor(EKG_lvl)) %>%
  drop_na()

# Model data --------------------------------------------------------------
# K-means clustering

## Extract the first two principal components
points <- pca_fit %>% 
  augment(prostate_data_pca) %>% 
  select(.fittedPC1, .fittedPC2)

## K-means clustering with k = 1..6 
## and create tidied, glanced and augmented data
kclusts <- tibble(k = 1:6) %>%
  mutate(kclust = map(k, ~kmeans(points, .x)),
         tidied = map(kclust, tidy),
         glanced = map(kclust, glance),
         augmented = map(kclust, augment, points))

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
## With centers of clusters 
p2 <- p1 + geom_point(data = clusters, size = 8, shape = "x")
p2

## Variance within clusters, scree-plot 
p3 <- ggplot(clusterings, aes(k, tot.withinss)) +
  geom_line() +
  geom_point()+ 
  labs( x = "Number of clusters, k", y = "Total within cluster variation")
p3

## use cubic cluster criteria,CCC to check if the number of cluster is ture or not
library("NbClust")
nc <- NbClust(points, min.nc = 2, max.nc = 6, method = "kmeans")
p3 <- nc %>%
  pluck("All.index") %>%
  as.tibble() %>%
  select(CCC) %>%
  ggplot(aes(y = CCC, x = c(2,3,4,5,6)))+geom_line()+
  labs( x = "Number of clusters", y = "CCC")

##Since the CCC value is negative and decreasing,  the true number of clusters is only one
# Write data --------------------------------------------------------------
ggsave(filename = "results/07_plot_kmeans_6cluster.png",
       plot = p1)
ggsave(filename = "results/07_plot_kmeans_center.png",
       plot = p2)
ggsave(filename = "results/07_plot_kmeans_CCC.png",
       plot = p3)