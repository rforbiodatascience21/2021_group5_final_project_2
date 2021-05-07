# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("broom")
library("cowplot")

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
prostate_clean_aug <- read_tsv(file = "data/03_prostate_clean_aug.tsv.gz")


# Wrangle data ------------------------------------------------------------
prostate_data_pca <- prostate_clean_aug %>%
  select(-where(is.character), 
         -patient_ID
         -treatment_mg, 
         -acid_phosphatase) %>% 
  mutate(outcome = factor(outcome),
         performance_lvl = factor(performance_lvl),
         EKG_lvl = factor(EKG_lvl)) %>%
  drop_na()

# Model data --------------------------------------------------------------

## PCA analysis
pca_fit <- prostate_data_pca %>%
  select(where(is.numeric)) %>% # only numeric columns
  prcomp(scale = TRUE) # PCA on scaled data


# Visualise data ----------------------------------------------------------

## Plot data in PC coordinates
p1 <- pca_fit %>%
  augment(prostate_data_pca) %>% # add original dataset back in
  ggplot(aes(.fittedPC1, 
             .fittedPC2, 
             color = outcome)) + 
  geom_point(size = 1.5, 
             alpha = 0.4) + 
  theme_minimal()

p1

# Arrow style for plot of rotation matrix
arrow_style <- arrow(angle = 20, 
                     ends = "first", 
                     type = "closed", 
                     length = grid::unit(8, "pt"))

## Plot of rotation matrix  
p2 <- pca_fit %>%
  tidy(matrix = "rotation") %>%
  pivot_wider(names_from = "PC", 
              names_prefix = "PC", 
              values_from = "value") %>%
  ggplot(aes(PC1, PC2)) +
  geom_segment(xend = 0, yend = 0, arrow = arrow_style) +
  geom_text(
    aes(label = column),
    hjust = 1, nudge_x = -0.02, 
    color = "#904C2F"
  ) +
  xlim(-0.6,0.5) + ylim(-0.7,0.2)+
  coord_fixed() + # fix aspect ratio to 1:1
  theme_minimal_grid(10)
p2

## Plot of variance explained by first 10 PCs
p3 <- pca_fit %>%
  tidy(matrix = "eigenvalues") %>%
  ## why?
  filter(percent > 0.05) %>% 
  ggplot(mapping = aes(PC, percent)) +
  geom_col(fill = "#56B4E9", 
           alpha = 0.8) +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(labels = scales::percent_format(),
                     expand = expansion(mult = c(0, 0.01))
  )

p3
# Write data --------------------------------------------------------------
save(pca_fit, file = "results/06_mdl_pca_fit.RData")
ggsave(filename = "results/06_plot_PCA_PCcoords.png",
       plot = p1)
ggsave(filename = "results/06_plot_PCA_rotation.png",
       plot = p2)
ggsave(filename = "results/06_plot_PCA_varExpl.png",
       plot = p3)