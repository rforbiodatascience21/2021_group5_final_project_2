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
prostate_clean_aug <- prostate_clean_aug %>%
  mutate(bone_mets = factor(bone_mets),
         CVD = factor(CVD),
         treatment = factor(treatment),
         stage = factor(stage),
         performance_lvl = factor(performance_lvl),
         outcome = case_when(outcome == 0 ~ "alive",
                             outcome == 1 ~ "dead"),
         acid_phosphatase_log = log(acid_phosphatase)) %>% 
  relocate(acid_phosphatase_log, .after = acid_phosphatase)


# Visualise data ----------------------------------------------------------

########################################
### Plots of pre-treatment variables ###
########################################
# I think if we want to investigate the data before treatment, we should select the data when treament_mg = 0 

# Distribution plot for the numeric variables stratified on "stage" 
prostate_clean_aug_longer <- prostate_clean_aug %>% 
  select(patient_ID, stage, where(is.numeric), -treatment_mg) %>% 
  pivot_longer(cols = c(-patient_ID, -stage), names_to = "variables", values_to = "values")

ggplot(prostate_clean_aug_longer, 
       mapping = aes(values, 
                     fill = stage)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~ variables, scales = "free", ncol = 3) +
  labs(x = " ", 
       y = "Density",
       title = "Distribution of the Pre-treatment Variables",
       subtitle = "The distribution of the numeric variables for the pre-treatment measures",
       fill = "Stage") +
  theme_minimal() + 
  theme(legend.position = "bottom") +
  scale_color_economist() +
  scale_fill_economist()
# We can use fct_inorder() to set the order of the plots


# Distribution of the categorical variables stratified on "stage" 
# CVD, bone_mets, performance_lvl

prostate_clean_aug %>%
  select(patient_ID, stage, CVD, bone_mets, performance_lvl) %>%
  pivot_longer(cols = c(-patient_ID, -stage), names_to = "variables", values_to = "values") %>% 
  ggplot(mapping = aes(values,
                       fill = stage)) +
  geom_bar() +
  facet_wrap(~ variables, scales = "free") +
  labs(x = " ", 
       y = "Number of cases",
       title = "Distribution of the Pre-treatment Variables",
       subtitle = "The distribution of the categorical variables for the pre-treatment measures",
       fill = "Stage") +
  theme_minimal() + 
  scale_fill_economist()
  
#The below plot is same with the above, why do it?
CVD_plot <- prostate_clean_aug %>%
  ggplot(mapping = aes(CVD,
                       fill = stage)) +
  geom_bar(show.legend = FALSE) +
  theme_minimal() + 
  scale_color_economist() + 
  scale_fill_economist()

BM_plot <- prostate_clean_aug %>%
  ggplot(mapping = aes(bone_mets,
                       fill = stage)) +
  geom_bar(show.legend = FALSE) +
  theme_minimal() + 
  scale_color_economist() + 
  scale_fill_economist()

performance_plot <- prostate_clean_aug %>%
  ggplot(mapping = aes(performance_lvl,
                       fill = stage)) +
  geom_bar(show.legend = TRUE) +
  theme_minimal() + 
  scale_color_economist() + 
  scale_fill_economist()


CVD_plot + BM_plot + performance_plot +
  plot_annotation( title = "Distribution of the Pre-treatment Variables",
                   subtitle = "The distribution of the categorical variables for the pre-treatment measures") +
  plot_layout(guides = "collect") 

# Must fix axis-label 

# Heatmap of correlations between the numeric variables
prostate_clean_aug %>% 
  select(where(is.numeric), -patient_ID, -treatment_mg) %>% 
  ggcorr(method = c("pairwise", "pearson"),
         label = TRUE, 
         legend.position = "bottom", 
         hjust = 0.8)

#Looking at outcome or stage? # Signe has changed stage to numeric, so we can
#only stratify on outcome. Should we remove categorical variables? Doesn't make
#sense to plot them in a scatterplot
prostate_clean_aug %>%
  select(where(is.numeric), outcome, -patient_ID, -treatment_mg) %>% 
  ggpairs(., mapping = aes(color = outcome), 
          columns = c(1:10),
          upper = list(continuous = "blank"),
          diag = list(continuous = wrap("densityDiag", alpha=0.3 )),
          lower = list(continuous = wrap("points", alpha=0.5 ), combo = "box_no_facet"),
          axisLabels = "show") +
  theme(legend.position = "bottom") +
  theme_minimal() + 
  scale_color_economist() + 
  scale_fill_economist() 

## Same as above, but no color according to a variable.
prostate_clean_aug %>%
  select(where(is.numeric), outcome, -patient_ID, -treatment_mg,-stage,-performance_lvl,-EKG_lvl) %>% 
  ggpairs(., 
          columns = c(1:7),
          upper = list(continuous = "blank"),
          diag = list(continuous = wrap("densityDiag", alpha=0.3 )),
          lower = list(continuous = wrap("points", alpha=0.5 ), combo = "box_no_facet"),
          axisLabels = "show") +
  theme(legend.position = "bottom") +
  theme_minimal() + 
  scale_color_economist() + 
  scale_fill_economist() 

# As we don't get any distribution from Acid Phosphatase, we log-transform
# and plot it for itself
prostate_clean_aug %>% 
  mutate(stage = factor(stage),
         log_ap = log10(acid_phosphatase)) %>% 
  ggplot(mapping = aes(log_ap,
                       fill = stage)) +
  geom_boxplot() + 
  theme_minimal() + 
  scale_color_economist() + 
  scale_fill_economist()

# Investigating the condition of the patients 
p01<-ggplot(data = prostate_clean_aug,
       mapping = aes(x = tumor_size,
                     y = outcome)) +
  geom_boxplot(alpha = 0.5, show.legend = TRUE) +
  theme_minimal() + 
  scale_color_economist() + 
  scale_fill_economist()

p02<-ggplot(data = prostate_clean_aug,
            mapping = aes(x = age,
                          y = outcome)) +
  geom_boxplot(alpha = 0.5, show.legend = TRUE) +
  theme_minimal() + 
  scale_color_economist() + 
  scale_fill_economist()

p01 + p02

###############################
## Plots with status reasons ##
###############################
## Distribution of tumor size for each stage, stratified on status with reason
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
  drop_na() %>% 
  ggplot(mapping = aes(treatment,
                       fill = outcome)) +
  geom_bar(alpha = 0.8, position = "fill") +
  facet_wrap(~age_group) +
  theme_minimal()+
  scale_fill_economist()

## Boxplot of tumor size for each outcome stratified on treatment
ggplot(data = prostate_clean_aug,
           mapping = aes(x = tumor_size,
                         y = outcome, 
                         fill = treatment)) +
  geom_boxplot(alpha = 0.5, show.legend = TRUE) +
  theme_minimal() +
  scale_color_economist()


# I think the below should use "geom_bar(alpha = 0.8, position = "fill")"
## Histogram of treatment for alive/dead stratified on age_group
ggplot(data = prostate_clean_aug,
       mapping = aes(treatment,
                     fill = age_group)) +
  geom_histogram(binwidth = 10, 
                 alpha = 0.8, 
                 stat="count") +
  facet_wrap(~ outcome) +
  theme_minimal()+
  theme(axis.text.x=element_text(angle = 30, hjust = 1)) +
  scale_fill_economist()


################# show that younger people respond better
ggplot(prostate_clean_aug, 
       mapping = aes(treatment,
                     age, fill = outcome)) +
  geom_raster(alpha = 0.8) + 
  geom_hline(yintercept = 70, linetype = "dashed")+
  theme_minimal() +
  scale_fill_economist()


#############################################################################
### Plots of significant variables found by logistic regression (dose 1mg) ## ? and is treatment a important variable for outcome
#############################################################################
p1 <- prostate_clean_aug %>% 
  filter(treatment_mg == 1.0) %>% 
  ggplot(mapping = aes(age,
                       color = outcome)) +
  geom_density(show.legend = FALSE) +
  theme_minimal() +
  scale_color_economist()

p2 <- prostate_clean_aug %>% 
  filter(treatment_mg == 1.0) %>% 
  ggplot(mapping = aes(weight_index,
                       color = outcome)) +
  geom_density(show.legend = FALSE) +
  theme_minimal() +
  scale_color_economist()

p3 <- prostate_clean_aug %>% 
  filter(treatment_mg == 1.0) %>% 
  ggplot(mapping = aes(tumor_size,
                       color = outcome)) +
  geom_density(show.legend = FALSE) +
  theme_minimal() +
  scale_color_economist()

p4 <- prostate_clean_aug %>% 
  filter(treatment_mg == 1.0) %>% 
  ggplot(mapping = aes(CVD,
                       fill = outcome)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  scale_fill_economist()

p4 + p1 / p2 / p3 +
  plot_annotation( title = "Significant variables influencing the outcome",
                   subtitle = "From the logistics regression it was found that CVD, age, weight and tumor size had a significant influence on the outcome") +
  plot_layout(guides = "collect")

# Write data --------------------------------------------------------------
write_tsv(...)
ggsave(...)
