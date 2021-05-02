# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
prostate_clean_aug <- read_rds(file = "data/03_prostate_clean_aug.rds.gz")


# Wrangle data ------------------------------------------------------------
## Remove rows containing NA
prostate_data_pca <- prostate_clean_aug %>% 
  select(-patno, -treatment, -status, -age_group) %>% 
  drop_na()

# Model data --------------------------------------------------------------
# PCA analysis
pca_fit <- prostate_data_pca %>%
  select(where(is.numeric)) %>% # only numeric columns
  prcomp(scale = TRUE) # PCA on scaled data

## Plot data in PC coordinates
p1 <- pca_fit %>%
  augment(prostate_data_pca) %>% # add original dataset back in
  ggplot(aes(.fittedPC1, .fittedPC2, color = outcome)) + 
  geom_point(size = 1.5, alpha = 0.4) + 
  theme_minimal()

# Define arrow style for plotting
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
  xlim(-1,1) +
  coord_fixed() + # fix aspect ratio to 1:1
  theme_minimal()

## Plot of variance explained by first 10 PCs
p3 <- pca_fit %>%
  tidy(matrix = "eigenvalues") %>%
  ggplot(aes(PC, percent)) +
  geom_col(fill = "#56B4E9", alpha = 0.8) +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(
    labels = scales::percent_format(),
    expand = expansion(mult = c(0, 0.01))
  )



# Visualise data ----------------------------------------------------------
prostate_clean_aug %>% ...


# Write data --------------------------------------------------------------
write_tsv(...)
