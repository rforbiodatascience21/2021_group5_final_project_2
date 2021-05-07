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
      family = binomial(link = "logit"))
tidy(log_mod_treatment)

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

## Plot 6
## Manhattan plot
p1 <- prostate_1mg_log_mod %>% 
  ggplot(mapping = aes(x = fct_reorder(variable, neg_log10_p, .desc = TRUE), 
                       y = neg_log10_p,
                       color = identified_as)) +
  geom_point() + 
  geom_hline(yintercept = -log10(0.05), 
             linetype = "dashed") +
  theme_minimal(base_size = 8, 
                base_family = "Avenir") +
  theme(legend.position = "bottom", 
        axis.text.x = element_text(angle = 45 , 
                                   vjust = 1, 
                                   hjust = 1)) +
  scale_fill_economist() +
  scale_color_economist() +
  labs(x = "Variable", 
       y = "Minus log10(p)")
p1
## Plot 7
## Plot of confidence intervals for effects of variables
p2 <- prostate_1mg_log_mod %>% 
  ggplot(mapping = aes(x = estimate, 
                       y = fct_reorder(variable, estimate, .desc = TRUE),
                       color = identified_as)) + 
  geom_point() + 
  geom_vline(xintercept = 0, 
             linetype = "dashed" ) + 
  geom_errorbarh(mapping = aes(xmin = conf.low,
                               xmax = conf.high,
                               height = 0.1)) +
  theme_classic(base_size = 8, 
                base_family = "Avenir") +
  theme(legend.position = "bottom" ) +
  labs(x = "Estimate", y = "Variable")


# Write data --------------------------------------------------------------
ggsave(filename = "results/05_plot_Manhattan.png",
       plot = p1)
ggsave(filename = "results/05_plot_CIeffects.png",
       plot = p2)
