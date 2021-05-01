# Clear workspace ---------------------------------------------------------
rm(list = ls())

# Load libraries ----------------------------------------------------------
library("tidyverse")
library("patchwork")
# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data and wrangle----------------------------------------------------
my_data <- read_tsv(file = "data/03_my_data_clean_aug.tsv")%>% 
          mutate(outcome = factor(outcome))%>%
          mutate(bm=factor(bm))%>%
          mutate(outcome= case_when(outcome == "0"~"alive",
                                    outcome=="1"~"death"))


# Visualise data ----------------------------------------------------------
#just try to see distribution
p1<-ggplot(data = my_data,
           mapping = aes(x = outcome,
                         y= age, fill=bm)) +
  geom_boxplot(alpha = 0.5, show.legend = FALSE) +
  scale_fill_manual(labels = c("Non_Metastases", "Metastases"),
                    values = c("red","blue"))+
  theme_classic()

p2<-ggplot(data = my_data,
           mapping = aes(x = outcome,
                         y= wt,fill=bm)) +
  geom_boxplot(alpha = 0.5) +
  scale_fill_manual(labels = c("Non_Metastases", "Metastases"),
                    values = c("red","blue"))+
  labs(caption = "Data from D.P.Byar(1977)")+
  theme_classic()+
  theme(legend.position = "bottom")

p3<-ggplot(data = my_data,
           mapping = aes(x = outcome,
                         y= ap,fill=bm)) +
  geom_boxplot(alpha = 0.5) +
  scale_fill_manual(labels = c("Non_Metastases", "Metastases"),
                    values = c("red","blue"))+
  labs(caption = "Data from D.P.Byar(1977)")+
  theme_classic()+
  theme(legend.position = "bottom")

p4<-ggplot(data = my_data,
           mapping = aes(x = outcome,
                         y= sz,fill=bm)) +
  geom_boxplot(alpha = 0.5, show.legend = FALSE) +
  scale_fill_manual(labels = c("Non_Metastases", "Metastases"),
                    values = c("red","blue"))+
  theme_classic()

p5<-ggplot(data = my_data,
           mapping = aes(x = outcome,
                         y= Treatment,fill=bm)) +
  geom_boxplot(alpha = 0.5, show.legend = FALSE) +
  scale_fill_manual(labels = c("Non_Metastases", "Metastases"),
                    values = c("red","blue"))+
  theme_classic()

p6<-ggplot(data = my_data,
           mapping = aes(x = outcome,
                         y= dbp,fill=bm)) +
  geom_boxplot(alpha = 0.5, show.legend = FALSE) +
  scale_fill_manual(labels = c("Non_Metastases", "Metastases"),
                    values = c("red","blue"))+
  theme_classic()

p7<-ggplot(data = my_data,
           mapping = aes(x = outcome,
                         y= sbp,fill=bm)) +
  geom_boxplot(alpha = 0.5) +
  scale_fill_manual(labels = c("Non_Metastases", "Metastases"),
                    values = c("red","blue"))+
  labs(caption = "Data from D.P.Byar(1977)")+
  theme_classic()+
  theme(legend.position = "bottom")

p8<-ggplot(data = my_data,
           mapping = aes(x = outcome,
                         y= hg,fill=bm)) +
  geom_boxplot(alpha = 0.5) +
  scale_fill_manual(labels = c("Non_Metastases", "Metastases"),
                    values = c("red","blue"))+
  labs(caption = "Data from D.P.Byar(1977)")+
  theme_classic()+
  theme(legend.position = "bottom")

p9<-ggplot(data = my_data,
           mapping = aes(x = outcome,
                         y= sg,fill=bm)) +
  geom_boxplot(alpha = 0.5) +
  scale_fill_manual(labels = c("Non_Metastases", "Metastases"),
                    values = c("red","blue"))+
  labs(caption = "Data from D.P.Byar(1977)")+
  theme_classic()+
  theme(legend.position = "bottom")

# make plot easy to see
p1+p2
p4+p9
p6+p7
p5+p8
p3

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

# Treatment plot

my_data <- read_tsv(file = "data/03_my_data_clean_aug.tsv")%>% 
           mutate(Treatment = factor(Treatment))%>%
           mutate(bm=factor(bm))%>%
           mutate(outcome= case_when(outcome == "0"~"alive",
                                     outcome=="1"~"death"))
p1<-ggplot(data = my_data,
       mapping = aes(x = outcome,
                      y= age, fill=Treatment)) +
  geom_boxplot(alpha = 0.5, show.legend = FALSE) +
  theme_classic()
  
p2<-ggplot(data = my_data,
       mapping = aes(x = outcome,
                      y= wt,fill=Treatment)) +
  geom_boxplot(alpha = 0.5) +
  labs(caption = "Data from D.P.Byar(1977)")+
  theme_classic()+
  theme(legend.position = "bottom")

p3<-ggplot(data = my_data,
       mapping = aes(x = outcome,
                      y= ap,fill=Treatment)) +
  geom_boxplot(alpha = 0.5) +
  labs(caption = "Data from D.P.Byar(1977)")+
  theme_classic()+
  theme(legend.position = "bottom")

p4<-ggplot(data = my_data,
       mapping = aes(x = outcome,
                      y= sz,fill=Treatment)) +
  geom_boxplot(alpha = 0.5, show.legend = FALSE) +
  theme_classic()

p5<-ggplot(data = my_data,
       mapping = aes(x = outcome,
                      y= rx,fill=Treatment)) +
  geom_boxplot(alpha = 0.5, show.legend = FALSE) +
  theme_classic()

p6<-ggplot(data = my_data,
       mapping = aes(x = outcome,
                      y= dbp,fill=Treatment)) +
  geom_boxplot(alpha = 0.5,show.legend = FALSE) +
  theme_classic()

p7<-ggplot(data = my_data,
       mapping = aes(x = outcome,
                      y= sbp,fill=Treatment)) +
  geom_boxplot(alpha = 0.5) +
  labs(caption = "Data from D.P.Byar(1977)")+
  theme_classic()+
  theme(legend.position = "bottom")

p8<-ggplot(data = my_data,
       mapping = aes(x = outcome,
                      y= hg,fill=Treatment)) +
  geom_boxplot(alpha = 0.5) +
  labs(caption = "Data from D.P.Byar(1977)")+
  theme_classic()+
  theme(legend.position = "bottom")

p9<-ggplot(data = my_data,
       mapping = aes(x = outcome,
                      y= sg,fill=Treatment)) +
  geom_boxplot(alpha = 0.5) +
  labs(caption = "Data from D.P.Byar(1977)")+
  theme_classic()+
  theme(legend.position = "bottom")

p1+p2
p4+p9
p6+p7
p5+p8
p3






