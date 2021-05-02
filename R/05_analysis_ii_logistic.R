# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
prostate_clean_aug <- read_rds(file = "data/03_prostate_clean_aug.rds.gz")


###################################
# Tests..
log_mod <- prostate_log %>% 
  drop_na %>% 
  filter(treatment_mg == 0) %>% 
  glm(outcome ~ CVD + tumor_size + age,
      data = .,
      family = binomial(link = "logit"))
tidy(log_mod)
###################################

# Wrangle data ------------------------------------------------------------
## Remove <chr> variables
prostate_log <- prostate_clean_aug %>% 
  select(-treatment, -status, -age_group) %>% 
  filter(treatment_mg == "1.0")

## Create long nested data of <fct> variables
long_nested_fct <- prostate_log %>%
  select(patno, outcome, stage, CVD, bone_mets) %>% ## deleted treatent_mg
  pivot_longer(cols = c(-patno, -outcome), 
               names_to = "variable", 
               values_to = "value") %>%
  group_by(variable) %>%
  nest() %>%
  ungroup()

## Create long nested data of <dbl> variables
long_nested_dbl <- prostate_log %>%
  select(-stage, -CVD, -bone_mets, -treatment_mg) %>%
  pivot_longer(cols = c(-patno, -outcome), 
               names_to = "variable", 
               values_to = "value") %>%
  group_by(variable) %>%
  nest() %>%
  ungroup()

## Combine to one long nested data set
long_nested <- bind_rows(long_nested_dbl, long_nested_fct)


# Model data --------------------------------------------------------------
## Creating a logistic model for each variable
prostate_logistic <- long_nested %>% 
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


# Write data --------------------------------------------------------------
write_tsv(...)

