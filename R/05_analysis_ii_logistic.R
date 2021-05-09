# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("generics")
library("ggthemes")

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
prostate_clean_aug <- read_tsv(file = "data/03_prostate_clean_aug.tsv.gz")


# Wrangle data ------------------------------------------------------------
prostate_data_logi <- prostate_clean_aug %>% 
  select(-where(is.character), 
         -patient_ID, 
         -performance_lvl,
         -acid_phosphatase) %>% 
  drop_na %>% 
  mutate(treatment_mg = factor(treatment_mg))

## Subset data according to most significant treatment (1 mg) and create long nested version
prostate_1mg_long_nest <- prostate_data_logi %>% 
  filter(treatment_mg == 1) %>% 
  select(-treatment_mg) %>% 
  pivot_longer(cols = -outcome, 
               names_to = "variable", 
               values_to = "value") %>%
  group_by(variable) %>%
  nest() %>%
  ungroup()


# Model data --------------------------------------------------------------

## Logistic regression based on treatments to find significant treatment dose
log_mod_treatment <- prostate_data_logi %>% 
  glm(outcome ~ treatment_mg,
      data = .,
      family = binomial(link = "logit")) %>% 
  tidy()

## Creating a logistic model for each variable in data for treatment 1 mg
prostate_1mg_log_mod <- prostate_1mg_long_nest %>% 
  mutate(mdl = map(data, ~ glm(outcome ~ value, 
                               data = .x,
                               family = binomial(link = "logit"))))

## Tidy each model and remove rows for intercept
prostate_1mg_log_mod <- prostate_1mg_log_mod %>% 
  mutate(mdl_tidy = map(mdl, tidy, conf.int = TRUE)) %>% 
  unnest(mdl_tidy) %>% 
  filter(str_detect(term, "value"))
  
## Identify significant and non-significant variables
prostate_1mg_log_mod <- prostate_1mg_log_mod %>% 
  mutate(identified_as = case_when(p.value < 0.05 ~ "Significant",
                                   TRUE ~ "Non-significant"))

## Create variable "neg_log10_p", log-transforming the p-values
prostate_1mg_log_mod <- prostate_1mg_log_mod %>%
  mutate(neg_log10_p = -log10(p.value))


# Visualise data ----------------------------------------------------------
# Creating a variable with real names for the variables 
variables_names <- c("acid_phosphatase_log" = "log(Acid Phosphatase)",
                     "age" = "Age",
                     "age_group" = "Age Group",
                     "dbp" = "Diastolic Blood Pressure", 
                     "EKG_lvl" = "EKG level",
                     "EKG" = "Electrocardiography",
                     "hemoglobin" ="Hemoglobin",
                     "sbp" ="	Systolic Blood Pressure",
                     "tumor_size" = "Tumor Size",
                     "weight_index" = "Weight Index",
                     "performance" = "Performance",
                     "performance_lvl" = "Performance level", 
                     "bone_mets" = "Bone Metastases",
                     "CVD" = "History of Cardiovascular Disease",
                     "stage" = "Stage")

## Plot 6
## Manhattan plot
p1 <- prostate_1mg_log_mod %>% 
  ggplot(mapping = aes(x = fct_reorder(variable, neg_log10_p, .desc = TRUE), 
                       y = neg_log10_p,
                       color = identified_as)) +
  geom_point() + 
  geom_hline(yintercept = -log10(0.05), 
             linetype = "dashed") +
  geom_text(aes(x = 10,
                y =  -log10(0.045),
                label = "Significance level, 0.05"),
                color = "black",
            size = 3) +
  labs(x = "Variable", 
       y = "Minus log10(p-value)",
       color = "Identified as:",
       title = "Manhattan plot",
       subtitle = "The Manhattan plot shows the -log10(p-value) versus the variables in the data set.") +
  theme_minimal() +
  scale_x_discrete(labels = variables_names) +
  theme(axis.text.x = element_text(angle = 35,
                                   size = 8, 
                                   hjust = 1),
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(face = "italic")) +
  scale_color_economist()
p1
## Plot 7
## Plot of confidence intervals for effects of variables
p2 <- prostate_1mg_log_mod %>% 
  filter(identified_as == "Significant") %>% 
  ggplot(mapping = aes(x = estimate, 
                       y = fct_reorder(variable, estimate, .desc = TRUE),
                       color = identified_as)) + 
  geom_point() + 
  geom_vline(xintercept = 0, 
             linetype = "dashed" ) + 
  geom_errorbarh(mapping = aes(xmin = conf.low,
                               xmax = conf.high,
                               height = 0.1)) +
  labs(x = "Estimate", 
       y = "Variable",
       color = "Identified as:",
       title = "Confidence Interval plot",
       subtitle = "The confidence interval plot shows the confidence levels of the estimations found in the logistic regression.\nThe plot further illustrates the direction of the effect by each significant variable influencing the outcome.") +
  theme_minimal() +
  scale_y_discrete(labels = variables_names) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", 
                                  size = 16, 
                                  hjust = -0.1),
        plot.subtitle = element_text(face = "italic",
                                     hjust = -0.1)) +
  scale_color_economist()

# Write data --------------------------------------------------------------
write_tsv(x = log_mod_treatment,
          file = "results/log_mod_treatment.tsv.gz")
ggsave(filename = "results/05_plot_Manhattan.png",
       plot = p1)
ggsave(filename = "results/05_plot_CIeffects.png",
       plot = p2)
