# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("broom")

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
prostate_clean_aug <- read_tsv(file = "data/03_prostate_clean_aug.tsv.gz")


###################################

###################################

# Wrangle data ------------------------------------------------------------

## Change the type of four variables to factor
prostate_clean_aug <- prostate_clean_aug %>%
  mutate(patient_ID = factor(patient_ID),
         stage = factor(stage),
         bone_mets = factor(bone_mets),
         CVD = factor(CVD))

## Remove <chr> variables
prostate_logi <- prostate_clean_aug %>% 
  select(-where(is.character)) %>% 
  drop_na

## Subset data - only treatment 1 mg
prostate_1mg <- prostate_logi %>% 
  filter(treatment_mg == "1.0")

## Create long nested data of <fct> variables
# maybe better to use prostate_clean_aug %>% select(where(is.factor)) ? 
long_nested_fct <- prostate_1mg %>%
  select(patient_ID, outcome, stage, CVD, bone_mets) %>%
  pivot_longer(cols = c(-patient_ID, -outcome), 
               names_to = "variable", 
               values_to = "value") %>%
  group_by(variable) %>%
  nest() %>%
  ungroup()

## Create long nested data of <dbl> variables
long_nested_dbl <- prostate_1mg %>%
  select(-stage, -CVD, -bone_mets, -treatment_mg) %>%
  pivot_longer(cols = c(-patient_ID, -outcome), 
               names_to = "variable", 
               values_to = "value") %>%
  group_by(variable) %>%
  nest() %>%
  ungroup()

## Combine to one long nested data set
## why need long_nested_fct? they are factor variables
long_nested_1mg <- bind_rows(long_nested_dbl, long_nested_fct)


# Model data --------------------------------------------------------------

## Logistic regression based on treatments
log_mod <- prostate_logi %>% 
  glm(outcome ~ treatment_mg,
      data = .,
      family = binomial(link = "logit"))
tidy(log_mod)

## Creating a logistic model for each variable
prostate_logistic <- long_nested_1mg%>% 
  mutate(mdl = map(data, ~ glm(outcome ~ value, 
                               data = .x,
                               family = binomial(link = "logit"))))

## Adding statistical variables for intercept and the estimated values for each model
## (std.error, statistic,  p.value, conf.low, conf.high)
prostate_logistic <- prostate_logistic %>% 
  mutate(mdl_tidy = map(mdl, tidy, conf.int = TRUE)) %>% 
  unnest(mdl_tidy)

## Remove rows for intercept
prostate_logistic <- prostate_logistic %>% 
  filter(str_detect(term, "value"))

## Identify significant and non-significant variables
prostate_logistic <- prostate_logistic %>% 
  mutate(identified_as = case_when(p.value < 0.05 ~ "Significant",
                                   TRUE ~ "Non-significant"))


# Visualise data ----------------------------------------------------------
## Create variable "neg_log10_p"
prostate_logistic <- prostate_logistic %>%
  mutate(neg_log10_p = -log10(p.value))

## Manhattan plot
p1 <- prostate_logistic %>% 
  ggplot(mapping = aes(x = fct_reorder(variable, neg_log10_p, .desc = TRUE), 
                       y = neg_log10_p,
                       color = identified_as)) +
  geom_point() + 
  geom_hline(yintercept = - log10(0.05), linetype = "dashed") +
  theme_classic(base_size = 8, base_family = "Avenir") +
  theme(legend.position = "bottom", 
        axis.text.x = element_text(angle = 45 , vjust = 1, hjust = 1)) +
  labs( x = "Variable", y = "Minus log10(p)")

## A confidence interval plot with effect directions
p2 <- prostate_logistic %>% 
  ggplot(mapping = aes(x = estimate, y = fct_reorder(variable, estimate, .desc = TRUE),
                       color = identified_as)) + 
  geom_point() + 
  geom_vline(xintercept = 0, linetype = "dashed" ) + 
  geom_errorbarh(aes(xmin = conf.low,
                     xmax = conf.high,
                     height = 0.1)) +
  theme_classic(base_size = 8, base_family = "Avenir") +
  theme(legend.position = "bottom" ) +
  labs( x = "Estimate", y = "Variable")

<<<<<<< HEAD
p1 + p2 

=======
>>>>>>> c109ffbefe773960511f21a16bf359c9059adbf9
# Write data --------------------------------------------------------------
write_tsv(...)

