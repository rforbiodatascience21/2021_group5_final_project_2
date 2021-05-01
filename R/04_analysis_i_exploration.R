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
my_data_clean_aug <- readRDS(file = "data/02_my_data_clean.rds")
#change back to "03_my_data_clean_aug"

# Wrangle data ------------------------------------------------------------
my_data_clean_aug %>% ...



## make a plot to show the correlation of status, stage type and hg(Serum Hemoglobin (g/100ml))
p1 <- prostate_clean_aug %>%
  distinct(patno,stage,rx,status) %>%
  count(status,rx) %>%
  ggplot(aes(y = status,x = n))+
  geom_col(aes(fill = rx),position = "dodge",
           alpha = 0.5)+
  theme_classic(base_family = "Avenir",
                base_size = 12)+
  theme(legend.position = "none")

p2 <- my_data_clean %>%
  distinct(patno,stage,hg,status) %>%
  ggplot(aes(y = status, x = hg))+
  geom_point(aes(colour = stage))+
  theme_classic(base_family = "Avenir",
                base_size = 12)+
  theme(legend.position = "bottom")

p3 <-grid.arrange(p1,p2,nrow=2,ncol=1)

my_data_clean_aug <- my_data_clean





________________________________________________________________
# Model data
#Creating a logistic model
## We will try to use the age of the patients to predict the risk for the outcome 
model_age <- my_data_clean_aug %>% glm(outcome ~ age,
                                       data = .,
                                       family = binomial(link = "logit"))
# What can we say about this? 
summary(model_age)

# Visualise data ----------------------------------------------------------

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


ggplot(data = my_data_clean_aug,
       mapping = aes(x = age,
                     y = Treatment,
                     color = outcome)) +
  geom_point()

# PCA analysis
pca_fit <- my_data_clean_aug %>%
  select(where(is.numeric)) %>%
  prcomp(scale = TRUE)

pca_fit %>%
  augment(my_data_clean_aug) %>%
  ggplot(aes(.fittedPC1, .fittedPC2, color = outcome))+
  geom_point(size = 1.5)+
  scale_color_manual(
    values = c("1" = "#D55E00", "0" = "#0072B2")
  )+
  theme_half_open(12) + background_grid()

pca_fit %>%
  tidy(matrix = "rotation")


# Write data --------------------------------------------------------------
write_tsv(...)
ggsave(...)