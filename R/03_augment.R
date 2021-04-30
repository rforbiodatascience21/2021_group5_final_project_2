# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("gridExtra")
library("fs")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
my_data_clean <- readRDS(file = "data/02_my_data_clean.rds")


# Wrangle data ------------------------------------------------------------

# Create new variable age_group, mean = 71,57
my_data_clean <- my_data_clean %>% 
  mutate(age_group = case_when( age <= 71 ~ "Young",
                    age > 71 ~ "Old"))


## make a plot to show the correlation of status, stage type and hg(Serum Hemoglobin (g/100ml))
p1 <- my_data_clean %>%
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
  


# Write data --------------------------------------------------------------
write_tsv(x = my_data_clean_aug,
          file = "data/03_my_data_clean_aug.tsv")
