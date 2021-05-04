# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("broom")
library("ggplot2")
library("patchwork")
library("GGally")
library("ggthemes")

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
prostate_clean_aug <- read_tsv(file = "data/03_prostate_clean_aug.tsv.gz")


# Wrangle data ------------------------------------------------------------
## Change the type of four variables to factor
prostate_clean_aug <- prostate_clean_aug %>%
  mutate(patient_ID = factor(patient_ID),
         stage = factor(stage),
         bone_mets = factor(bone_mets),
         CVD = factor(CVD),
         treatment = factor(treatment))

prostate_clean_aug <- prostate_clean_aug %>% 
  mutate(outcome = case_when(outcome == 0 ~ "alive",
                             outcome == 1 ~ "dead"))

# Visualise data ----------------------------------------------------------

########################################
### Plots of pre-treatment variables ###
########################################

# Looking at the numeric variables in the data set to find possible patterns
ggcorr( data = prostate_clean_aug %>% 
          select(where(is.numeric)), 
          method = c("pairwise", "pearson"))

#Looking at outcome or stage?
prostate_clean_aug %>%
  select(where(is.numeric), stage) %>% 
  ggpairs(., mapping = aes(color = stage), 
          columns = c(1:11),
          upper = list(continuous = "blank"),
          diag = list(continuous = wrap("densityDiag", alpha=0.3 )),
          lower = list(continuous = wrap("points", alpha=0.5 ), combo = "box_no_facet"),
          axisLabels = "show") +
  theme(legend.position = "bottom") +
  theme_minimal() + 
  scale_color_economist() + 
  scale_fill_economist() 

# As we don't get any distribution from Acid Phosphatase, we look into that variable 
prostate_clean_aug %>% 
  ggplot(mapping = aes(acid_phosphatase,
                       color=stage)) +
  geom_boxplot() + 
  theme_minimal() + 
  scale_color_economist() + 
  scale_fill_economist()

# Investigating the condition of the patients 
p01<-ggplot(data = prostate_clean_aug,
       mapping = aes(x = tumor_size,
                     y = stage, 
                     fill = stage)) +
  geom_boxplot(alpha = 0.5, show.legend = TRUE) +
  theme_minimal() + 
  scale_color_economist() + 
  scale_fill_economist()

p02<-ggplot(data = prostate_clean_aug,
            mapping = aes(x = age,
                          y = stage, 
                          fill = stage)) +
  geom_boxplot(alpha = 0.5, show.legend = TRUE) +
  theme_minimal() + 
  scale_color_economist() + 
  scale_fill_economist()

p01 + p02
#####################
prostate_clean_aug %>% 
  ggplot(mapping = aes(tumor_size,
                       fill = status)) +
  geom_histogram(binwidth = 15) +
  facet_wrap(~stage) +
  theme_minimal() + 
  scale_color_economist() + 
  scale_fill_economist()

# Looking at the number of observations within each status-group 
# additionally we see that the majority of the dead people are 'old'
prostate_clean_aug %>% 
  ggplot(mapping = aes(status,
                       fill = age_group)) +
  geom_bar()+
  theme_minimal()+
  theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
  scale_fill_economist()

######################################
### Plots of treatment and outcome ###
######################################

## Distribution of alive/dead
prostate_clean_aug %>% 
  ggplot(mapping = aes(outcome,
                       fill = outcome)) +
  geom_bar(alpha = 0.7, show.legend = FALSE) +
  theme_minimal() +
  scale_fill_economist()

## Distribution of alive/dead for each treatment, scaled for better comparison
## across groups
prostate_clean_aug %>% 
  ggplot(mapping = aes(treatment,
                       fill = outcome)) +
  geom_bar(alpha = 0.8, position = "fill") +
  facet_wrap(~stage) +
  theme_minimal()+
  scale_fill_economist()


## Boxplot of tumor size for each outcome stratified on treatment
ggplot(data = prostate_clean_aug,
           mapping = aes(x = tumor_size,
                         y = outcome, 
                         fill = treatment_mg)) +
  geom_boxplot(alpha = 0.5, show.legend = TRUE) +
  theme_minimal() +
  scale_color_economist()

# maybe another plot which really show that treatment 1mg is the best?
ggplot(data = prostate_clean_aug,
       mapping = aes(treatment,
                     fill = age_group)) +
  geom_histogram(binwidth = 10, 
                 alpha = 0.5, 
                 stat="count") +
  facet_wrap(~ outcome) +
  theme_minimal()+
  theme(axis.text.x=element_text(angle = 30, hjust = 1)) +
  scale_fill_economist()

################# show that younger people respond better
ggplot(prostate_clean_aug, 
       mapping = aes(treatment,
                     age, fill = outcome)) +
  geom_raster() + 
  geom_hline(yintercept = 70, linetype = "dashed")+
  theme_minimal() +
  scale_fill_economist()


  #############################################################################
### Plots of significant variables found by logistic regression (dose 1mg) ##
#############################################################################
p1 <- prostate_clean_aug %>% 
  filter(treatment_mg == 1.0) %>% 
  ggplot(mapping = aes(age,
                       color = outcome)) +
  geom_density() +
  theme_minimal() +
  scale_color_economist()

p2 <- prostate_clean_aug %>% 
  filter(treatment_mg == 1.0) %>% 
  ggplot(mapping = aes(weight_index,
                       color = outcome)) +
  geom_density() +
  theme_minimal() +
  scale_color_economist()

p3 <- prostate_clean_aug %>% 
  filter(treatment_mg == 1.0) %>% 
  ggplot(mapping = aes(tumor_size,
                       color = outcome)) +
  geom_density() +
  theme_minimal() +
  scale_color_economist()

p4 <- prostate_clean_aug %>% 
  filter(treatment_mg == 1.0) %>% 
  ggplot(mapping = aes(CVD,
                       fill = outcome)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  scale_fill_economist()

p4 + p1 / p2 / p3 

# Write data --------------------------------------------------------------
write_tsv(...)
ggsave(...)