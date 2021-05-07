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

# Renaming some the binary values 
prostate_clean_aug <- prostate_clean_aug %>%
  mutate(outcome = case_when(outcome == 0 ~ "Alive",
                             outcome == 1 ~ "Dead"),
         CVD = case_when(CVD == 0 ~ "No",
                         CVD == 1 ~ "Yes"),
         bone_mets = case_when(bone_mets == 0 ~ "No",
                               bone_mets == 1 ~ "Yes"))

# Factorizing variables and adding levels 
prostate_clean_aug <- prostate_clean_aug %>%
  mutate(bone_mets = factor(bone_mets),
         CVD = factor(CVD),
         treatment = factor(treatment),
         stage = factor(stage),
         performance = factor(performance,
                              levels = c("normal activity",
                                         "in bed < 50% daytime",
                                         "in bed > 50% daytime",
                                         "confined to bed")),
         performance_lvl = factor(performance_lvl),
         EKG = factor(EKG,
                      levels = c("normal",
                                 "benign",
                                 "rhythmic disturb & electrolyte ch",
                                 "heart block or conduction def",
                                 "heart strain",
                                 "old MI",
                                 "recent MI")),
         EKG_lvl = factor(EKG_lvl))

  
# Creating a variable with real names for the variables 
variables_names <- c("acid_phosphatase_log" = "log(Acid Phosphatase)",
                     "age" = "Age [years]",
                     "age_group" = "Age Group",
                     "dbp" = "Diastolic Blood Pressure/10", 
                     "EKG_lvl" = "EKG level",
                     "EKG" = "Electrocardiography",
                     "hemoglobin" ="Hemoglobin [g/100ml]",
                     "sbp" ="	Systolic Blood Pressure/10",
                     "tumor_size" = "Tumor Size [cm^2]",
                     "weight_index" = "Weight Index",
                     "performance" = "Performance",
                     "performance_lvl" = "Performance level", 
                     "bone_mets" = "Bone Metastases",
                     "CVD" = "History of Cardiovascular Disease")

# Pivot longer for using facet_wrap to plot several distributions in one plot 
prostate_clean_aug_longer <- prostate_clean_aug %>% 
  select(patient_ID, stage, where(is.numeric), -treatment_mg, -acid_phosphatase) %>% 
  pivot_longer(cols = c(-patient_ID, -stage), names_to = "variables", values_to = "values")


# Visualize data ----------------------------------------------------------

########################################
### Plots of pre-treatment variables ###
########################################


## Plot 1
# Distribution plot for the numeric variables stratified on "stage" 
ggplot(prostate_clean_aug_longer, 
       mapping = aes(values, 
                     fill = stage)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~ variables, 
             scales = "free", 
             nrow = 2,
             labeller = labeller(variables = variables_names)) +
  theme_minimal() + 
  theme(legend.position = c(0.85, 
                            0.1),
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(face = "italic")) +
  labs(x = " ", 
       y = "Density",
       title = "Distribution of the Pre-treatment Numerical Variables",
       subtitle = "Density plots of the numerical variables for the pre-treatment measurements, stratified on the stage of Prostate cancer",
       fill = "Stage of prostate cancer") +
  scale_fill_economist() +
  scale_color_economist()
             

## Plot 2
# Distribution of the categorical variables stratified on "stage" 
# CVD, bone_mets, performance, EKG
prostate_clean_aug %>%
  select(patient_ID, stage, CVD, bone_mets, performance, EKG) %>%
  drop_na() %>% 
  pivot_longer(cols = c(-patient_ID, -stage), names_to = "variables", values_to = "values") %>% 
  ggplot(mapping = aes(values,
                       fill = stage)) +
  geom_bar() +
  facet_wrap(~ variables,
             scale = "free_x",
             nrow = 1,
             labeller = labeller(variables = variables_names)) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 35, 
                                   hjust = 1),
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(face = "italic")) +
  labs(x = " ", 
       y = "Number of cases",
       title = "Distribution of the Pre-treatment Categorical Variables",
       subtitle = "Bar plots of the categorical variables for the pre-treatment measurements, stratified on the stage of Prostate cancer",
       fill = "Stage") +
  scale_fill_economist()

## Plot 3
# Heatmap of correlations between the numeric variables
prostate_clean_aug %>% 
  select(where(is.numeric), -patient_ID, -treatment_mg, -acid_phosphatase) %>% 
  ggcorr(method = c("pairwise", "pearson"),
         label = TRUE, 
         name = "Person Coefficient",
         legend.position = "bottom",
         hjust = 0.8) +
  labs(title = "Heatmap of Correlations between the Numeric Variables") +
  theme(plot.title = element_text(face = "bold", size = 16))

# Overall distribution plot, for numeric value
# NOT TO BE USED FOR PRESENTATION
prostate_clean_aug %>%
  select(where(is.numeric), outcome, -patient_ID, -treatment_mg) %>% 
  ggpairs(., mapping = aes(color = outcome), 
          #columns = c(1:9),
          upper = list(continuous = "blank"),
          diag = list(continuous = wrap("densityDiag", alpha=0.3 )),
          lower = list(continuous = wrap("points", alpha=0.5 ), combo = "box_no_facet"),
          axisLabels = "show") +
  theme(legend.position = "bottom") +
  theme_minimal() 

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


###############################
## Plots with status reasons ##
###############################
## Distribution of tumor size stratified on status with reason
# Investigating if the tumor size have an affect of death reason

# NOT USED FOR PRESENTATION

p03 <- prostate_clean_aug %>% 
  ggplot(mapping = aes(tumor_size,
                       fill = status)) +
  geom_histogram(binwidth = 5) +
  theme_minimal() + 
  scale_color_economist() + 
  scale_fill_economist()

p04 <- prostate_clean_aug %>% 
    ggplot(mapping = aes(tumor_size,
                         fill = outcome)) +
    geom_histogram(binwidth = 5) +
    geom_vline(xintercept = 28,
               linetype = "dashed") +
    theme_minimal() + 
    scale_color_economist() + 
    scale_fill_economist()

p03 / p04 


# Looking at the number of observations within each status-group 
# additionally we see that the majority of the dead people are 'old'
prostate_clean_aug %>% 
  drop_na() %>% 
  ggplot(mapping = aes(status,
                       fill = age_group)) +
  geom_bar()+
  theme_minimal()+
  theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
  scale_fill_economist()

#OR 

prostate_clean_aug %>% 
  ggplot(mapping = aes(status,
                       fill = outcome)) +
  geom_bar(show.legend = FALSE)+
  theme_minimal()+
  theme(axis.text.x=element_text(angle = 25, hjust = 1)) +
  labs(title = "The Status Variable",
       x = "Status",
       y = "Number of cases") +
  scale_fill_economist()

######################################
### Plots of treatment and outcome ###
######################################

## Plot 4
## Distribution of alive/dead
p05 <- prostate_clean_aug %>%   
  ggplot(mapping = aes(outcome,
                       color = outcome,
                       fill = outcome)) +
  geom_bar(alpha = 0.1,
           show.legend = FALSE) +
  theme_minimal() +
  labs( x = "Outcome",
        y = "Number of cases") +
  scale_color_economist() +
  scale_fill_economist()


## Boxplot of tumor size for each outcome stratified on treatment
#Lucille - do we need this plot?
ggplot(data = prostate_clean_aug,
           mapping = aes(x = tumor_size,
                         y = outcome, 
                         fill = treatment)) +
  geom_boxplot(alpha = 0.5, show.legend = TRUE) +
  theme_minimal() +
  scale_color_economist()


# I think the below should use "geom_bar(alpha = 0.8, position = "fill")"
## Histogram of treatment for alive/dead stratified on age_group
p06 <- prostate_clean_aug %>% 
  drop_na() %>%  
  ggplot(mapping = aes(treatment,
                     fill = age_group)) +
  geom_bar() +
  facet_wrap(~ outcome) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 30, 
                                   hjust = 1),
        legend.position = "bottom") +
  labs(x = "Treatment",
       y = "Number of cases",
       fill = "Age Group") +
  scale_fill_economist()
## Plot 5
p05 + p06 +
  plot_annotation(title = "Distribution of Outcome in Relation to Treatment and Age",
                  subtitle = "This figure illustrate the distributen of the suvival-rate in the data set.\nThe distribution in relation to age ang treatment, indicates that the 0.1 mg treatment might be the most efficient.",
                  caption = "Figure A, shows the number of patients alive vs. dead.\nFigure B, shows the number of cases within each outcome group, additionally stratified on age group. ",
                  tag_levels = "A",
                  tag_prefix = "Figure ",
                  theme = theme(plot.title = element_text(face = "bold", 
                                                          size = 16),
                                plot.subtitle = element_text(face = "italic",
                                                             hjust = 0,
                                                             size = 12),
                                plot.caption = element_text(face = "italic",
                                                            hjust = 0,
                                                            size = 12),
                                plot.tag = element_text(size = 12)))

#############################################################################
### Plots of significant variables found by logistic regression (dose 1mg) ##
#############################################################################
## Plot 8
p1 <- prostate_clean_aug %>% 
  filter(treatment_mg == 1.0) %>% 
  ggplot(mapping = aes(age,
                       color = outcome)) +
  geom_density(show.legend = FALSE) +
  labs(x = "Age",
       y= "Density") +
  theme_minimal() +
  scale_color_economist()

p2 <- prostate_clean_aug %>% 
  filter(treatment_mg == 1.0) %>% 
  ggplot(mapping = aes(weight_index,
                       color = outcome)) +
  geom_density(show.legend = FALSE) +
  labs(x = "Weight Index",
       y= "Density") +
  theme_minimal() +
  scale_color_economist()

p3 <- prostate_clean_aug %>% 
  filter(treatment_mg == 1.0) %>% 
  ggplot(mapping = aes(tumor_size,
                       color = outcome)) +
  geom_density(show.legend = FALSE) +
  labs(x = "Tumor Size",
       y= "Density") +
  theme_minimal() +
  scale_color_economist()

p4 <- prostate_clean_aug %>% 
  filter(treatment_mg == 1.0) %>% 
  ggplot(mapping = aes(CVD,
                       fill = outcome)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(x = "History of Cardiovascular Disease", 
         y = "Number of cases") +
  scale_fill_economist()

p4 + p1 / p2 / p3 +
  plot_annotation( title = "Significant Variables Influencing the Outcome",
                   subtitle = "From the logistic regression it was found that the variables 'CVD', 'age', 'weight' and 'tumor size' had a significant influence on the outcome",
                   theme = theme(plot.title = element_text(face = "bold", 
                                                           size = 16),
                                 plot.subtitle = element_text(face = "italic",
                                                              hjust = 0,
                                                              size = 12),
                                 legend.position = "bottom")) +
   plot_layout(guides = "collect") 

# Write data --------------------------------------------------------------
write_tsv(...)
ggsave(...)
