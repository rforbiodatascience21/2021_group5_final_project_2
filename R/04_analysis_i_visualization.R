# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("broom")
#library("cowplot")
library("ggplot2")
#library("GGally")
library("patchwork")

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
prostate_clean_aug <- read_rds(file = "data/03_prostate_clean_aug.rds.gz")


# Wrangle data ------------------------------------------------------------

prostate_clean_aug <- prostate_clean_aug %>% 
  mutate(outcome = case_when(outcome == 0 ~ "alive",
                             outcome == 1 ~ "dead"))
         #outcome = factor(outcome)))

# Visualise data ----------------------------------------------------------

## Distribution of alive/dead
prostate_clean_aug %>% 
  ggplot(mapping = aes(outcome,
                       fill = outcome)) +
  geom_bar(alpha = 0.8, show.legend = FALSE) +
  theme_minimal()

## Distribution of alive/dead for each treatment, scaled for better comparison
## across groups
prostate_clean_aug %>% 
  ggplot(mapping = aes(treatment,
                       fill = outcome)) +
  geom_bar(alpha = 0.8, position = "fill") +
  facet_wrap(~stage)

## Boxplots of tumor_size for each status after treatment with 1mg
prostate_clean_aug %>% 
  #filter(treatment_mg == 0 | treatment_mg == "1.0") %>% 
  ggplot(mapping = aes(tumor_size,
                       fill = outcome)) +
  geom_boxplot() +
  facet_wrap(~treatment_mg) +
  coord_flip()

## Scatterplot of age vs tumor size
prostate_clean_aug %>% 
  ggplot(mapping = aes(age,
                       tumor_size)) +
  geom_point()

## Barplot/histogram of status colored by treatment
prostate_clean_aug %>%
  distinct(patno,stage,treatment,status) %>%
  count(status,treatment) %>%
  ggplot(aes(y = status,x = n))+
  geom_col(aes(fill = treatment),position = "dodge",
           alpha = 0.5)+
  theme_classic(base_family = "Avenir",
                base_size = 12)+
  theme(legend.position = "none")


## Boxplot of age for each status colored by Bone Metastasis
ggplot(data = prostate_clean_aug,
           mapping = aes(x = outcome,
                         y = tumor_size, 
                         fill = treatment)) +
  geom_boxplot(alpha = 0.5, show.legend = TRUE) +
  #scale_fill_manual(labels = c("Non_Metastases", "Metastases"),
                    #values = c("red","blue"))+
  theme_minimal()

## Boxplot of Weight Index for each status colored by Bone Metastasis
ggplot(data = prostate_clean_aug,
           mapping = aes(x = outcome,
                         y=  weight_index, fill= bone_mets)) +
  geom_boxplot(alpha = 0.5) +
  scale_fill_manual(labels = c("Non_Metastases", "Metastases"),
                    values = c("red","blue"))+
  labs(caption = "Data from D.P.Byar(1977)")+
  theme_classic()+
  theme(legend.position = "bottom")

## Boxplot of Acid Phosphatase for each status colored by Bone Metastasis
p3<-ggplot(data = prostate_clean_aug,
           mapping = aes(x = outcome,
                         y= acid_phosphatase, fill = bone_mets)) +
  geom_boxplot(alpha = 0.5) +
  scale_fill_manual(labels = c("Non_Metastases", "Metastases"),
                    values = c("red","blue"))+
  labs(caption = "Data from D.P.Byar(1977)")+
  theme_classic()+
  theme(legend.position = "bottom")


## Boxplot of Tumor Size for each status colored by Bone Metastasis
p4<-ggplot(data = prostate_clean_aug,
           mapping = aes(x = outcome,
                         y= tumor_size, fill = bone_mets)) +
  geom_boxplot(alpha = 0.5) +
  scale_fill_manual(labels = c("Non_Metastases", "Metastases"),
                    values = c("red","blue"))+
  theme_classic()

## Boxplot of Diastolic Blood Pressure for each status colored by Bone Metastasis
p6<-ggplot(data = prostate_clean_aug,
           mapping = aes(x = outcome,
                         y= dbp,fill=bone_mets)) +
  geom_boxplot(alpha = 0.5, show.legend = FALSE) +
  scale_fill_manual(labels = c("Non_Metastases", "Metastases"),
                    values = c("red","blue"))+
  theme_classic()

## Boxplot of Systolic Blood Pressure for each status colored by Bone Metastasis
p7<-ggplot(data = prostate_clean_aug,
           mapping = aes(x = outcome,
                         y= sbp,fill=bone_mets)) +
  geom_boxplot(alpha = 0.5) +
  scale_fill_manual(labels = c("Non_Metastases", "Metastases"),
                    values = c("red","blue"))+
  labs(caption = "Data from D.P.Byar(1977)")+
  theme_classic()+
  theme(legend.position = "bottom")

## Boxplot of Hemoglobin for each status colored by Bone Metastasis
p8<-ggplot(data = prostate_clean_aug,
           mapping = aes(x = outcome,
                         y= hg,fill=bone_mets)) +
  geom_boxplot(alpha = 0.5) +
  scale_fill_manual(labels = c("Non_Metastases", "Metastases"),
                    values = c("red","blue"))+
  labs(caption = "Data from D.P.Byar(1977)")+
  theme_classic()+
  theme(legend.position = "bottom")

# Overview
p1+p2
p4+p6
p7+p8
p3


## Boxplots colored according to treatment
p1 <- ggplot(data = prostate_clean_aug,
           mapping = aes(x = outcome,
                         y= age, fill=treatment_mg)) +
  geom_boxplot(alpha = 0.5, show.legend = FALSE) +
  theme_classic()

p2<-ggplot(data = prostate_clean_aug,
           mapping = aes(x = outcome,
                         y= weight_index,fill=treatment_mg)) +
  geom_boxplot(alpha = 0.5) +
  labs(caption = "Data from D.P.Byar(1977)")+
  theme_classic()+
  theme(legend.position = "bottom")

p3<-ggplot(data = prostate_clean_aug,
           mapping = aes(x = outcome,
                         y= acid_phosphatase,fill=treatment_mg)) +
  geom_boxplot(alpha = 0.5) +
  labs(caption = "Data from D.P.Byar(1977)")+
  theme_classic()+
  theme(legend.position = "bottom")

p4<-ggplot(data = prostate_clean_aug,
           mapping = aes(x = outcome,
                         y= tumor_size,fill=treatment_mg)) +
  geom_boxplot(alpha = 0.5, show.legend = FALSE) +
  theme_classic()

p6<-ggplot(data = prostate_clean_aug,
           mapping = aes(x = outcome,
                         y= dbp,fill=treatment_mg)) +
  geom_boxplot(alpha = 0.5,show.legend = FALSE) +
  theme_classic()

p7<-ggplot(data = prostate_clean_aug,
           mapping = aes(x = outcome,
                         y= sbp,fill=treatment_mg)) +
  geom_boxplot(alpha = 0.5) +
  labs(caption = "Data from D.P.Byar(1977)")+
  theme_classic()+
  theme(legend.position = "bottom")

p8<-ggplot(data = prostate_clean_aug,
           mapping = aes(x = outcome,
                         y= hg,fill=treatment_mg)) +
  geom_boxplot(alpha = 0.5) +
  labs(caption = "Data from D.P.Byar(1977)")+
  theme_classic()+
  theme(legend.position = "bottom")

p1+p2
p4+p8
p6+p7
p3








# Write data --------------------------------------------------------------
write_tsv(...)
ggsave(...)