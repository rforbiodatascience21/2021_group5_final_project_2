# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("patchwork")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data and wrangle----------------------------------------------------
prostate_clean_aug <- read_rds(file = "data/03_prostate_clean_aug.rds.gz")


# Visualise data ----------------------------------------------------------
## Boxplot of age for each status colored by Bone Metastasis
p1<-ggplot(data = prostate_clean_aug,
           mapping = aes(x = outcome,
                         y= age, fill = bone_mets)) +
  geom_boxplot(alpha = 0.5, show.legend = FALSE) +
  scale_fill_manual(labels = c("Non_Metastases", "Metastases"),
                    values = c("red","blue"))+
  theme_classic()

## Boxplot of Weight Index for each status colored by Bone Metastasis
p2<-ggplot(data = prostate_clean_aug,
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

## Not supposed to use this package..
#just a try
library("ggridges")
ggplot(data = longer_data,
       mapping = aes(x = value,
                     y = parameter,
                     fill = parameter)) +
  geom_density_ridges(alpha = 0.5) +
  scale_fill_viridis_d() +
  labs(title = "Distribution for different parameter") +
  theme_minimal(base_family = "Avenir", base_size = 12) +
  theme(legend.position = "bottom") +
  facet_wrap(vars(outcome), ncol = 2)


## Boxplots colored according to treatment

p1<-ggplot(data = prostate_clean_aug,
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






