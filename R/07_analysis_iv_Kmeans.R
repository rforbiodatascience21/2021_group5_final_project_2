# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("broom")
library("ggthemes")
library("NbClust")

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
prostate_data_pca <- read_tsv(file = "data/03_prostate_data_pca.tsv.gz")
load(file = "results/06_mdl_pca_fit.RData")


# Wrangle data ------------------------------------------------------------
prostate_data_pca <- prostate_data_pca %>%
  mutate(outcome = factor(outcome),
         performance_lvl = factor(performance_lvl),
         EKG_lvl = factor(EKG_lvl),
         stage = factor(stage),
         CVD = factor(CVD),
         bone_mets = factor(bone_mets))
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
  mutate(kclust = map(k, ~kmeans(x = points, 
                                 centers = .x)),
         tidied = map(kclust, 
                      tidy),
         glanced = map(kclust, 
                       glance),
         augmented = map(kclust, 
                         augment, 
                         points))

## Turn into three separate data sets using tidy(), augment() and glance()
clusters <- kclusts %>%
  unnest(cols = c(tidied))

assignments <- kclusts %>% 
  unnest(cols = c(augmented))

clusterings <- kclusts %>%
  unnest(cols = c(glanced))


# Visualise data ----------------------------------------------------------
## Create 6 plots with varying number of clusters k=1..6
p1 <- ggplot(data = assignments, 
             mapping = aes(x = .fittedPC1, 
                           y = .fittedPC2)) +
  geom_point(aes(color = .cluster), 
             alpha = 0.8) + 
  facet_wrap(~ k) +
  theme_minimal() +
  labs(x = "Fitted PC1",
       y = "Fitted PC2",
       title ="K-means Clustering",
       color = "Cluster") +
  theme(plot.title = element_text(face = "bold",
                                  size = 16)) +
  scale_color_economist()

## With centers of clusters 
p2 <- p1 + geom_point(data = clusters, 
                      size = 6, 
                      shape = "x",
                      color = "red")

## Variance within clusters, scree-plot 
p3 <- ggplot(data = clusterings, 
             mapping = aes(x = k, 
                           y = tot.withinss)) +
  geom_line() +
  geom_point() +   
  theme_minimal() +
  labs(x = "Number of clusters, k",
       y = "Total variation within cluster",
       title = "Scree-plot") +
  theme(plot.title = element_text(face = "bold", 
                                  size = 16))

## use cubic cluster criteria,CCC to check if the number of cluster is true or not

nc <- NbClust(points, 
              min.nc = 2, 
              max.nc = 6, 
              method = "kmeans")
p4 <- nc %>%
  pluck("All.index") %>%
  as_tibble() %>%
  select(CCC) %>%
  ggplot(aes(y = CCC, 
             x = c(2,3,4,5,6))) +
  geom_line()+
  theme_minimal() +
  labs( x = "Number of clusters", 
        y = "CCC",
        title = "Cubic Clustering Criterion Plot") +
  theme(plot.title = element_text(face = "bold", 
                                  size = 16))
  
p4  

##Since the CCC value is negative and decreasing,  the true number of clusters is only one
# Write data --------------------------------------------------------------
ggsave(filename = "results/07_plot_kmeans_center.png",
       plot = p2,
       width = 8.51,
       height = 4.3,
       units = "in")

ggsave(filename = "results/07_plot_kmeans_scree.png",
       plot = p3,
       width = 8.51,
       height = 4.3,
       units = "in")

ggsave(filename = "results/07_plot_kmeans_CCC.png",
       plot = p4,
       width = 8.51,
       height = 4.3,
       units = "in"
       )
