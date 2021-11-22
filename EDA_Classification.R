
library(gridExtra)
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(lattice)
library(corrplot)
library("GGally")  
library(patchwork)

rm(list=ls())

Input_file <- "OSA_DB_UPM.xlsx"
Data_Directory <- "~/MLLB/DATA/"
df_OSA <- read_excel(paste(Data_Directory, Input_file, sep = ""))

summary(df_OSA)
df_OSA <- df_OSA %>%
  transform(Patient = as.factor(Patient)) %>%
  transform(Gender = as.factor(Gender)) %>%
  transform(Smoker = as.factor(Smoker)) %>%
  transform(Snorer = as.factor(Snorer)) %>%
  transform(OSA = as.factor(OSA))
summary(df_OSA)

df_OSA_rm <- df_OSA %>% filter(OSA %in% c("Healthy", "Severe"))
df_OSA_rm$OSA <- droplevels(df_OSA_rm$OSA)
summary(df_OSA_rm)


################################################################
################# Categorical Attributes #######################
################################################################

p_Gender <- df_OSA %>% group_by(Gender,OSA) %>% summarise(count=n()) %>%
  ggplot(aes(x=Gender, y=count, fill=OSA)) +
  geom_bar(stat="identity") + theme(legend.position="none")

p_Smoker <- df_OSA %>% group_by(Smoker,OSA) %>% summarise(count=n()) %>%
  ggplot(aes(x=Smoker, y=count, fill=OSA)) +
  geom_bar(stat="identity") + theme(legend.position="none")

p_Snorer <- df_OSA %>% group_by(Snorer,OSA) %>% summarise(count=n()) %>%
  ggplot(aes(x=Snorer, y=count, fill=OSA)) +
  geom_bar(stat="identity") + theme(legend.position="none")

combined <- p_Gender + p_Smoker + p_Snorer & theme(legend.position = "bottom")
combined + plot_layout(guides = "collect")

p_Gender <- df_OSA_rm %>% group_by(Gender,OSA) %>% summarise(count=n()) %>%
  ggplot(aes(x=Gender, y=count, fill=OSA)) +
  geom_bar(stat="identity")

p_Smoker <- df_OSA_rm %>% group_by(Smoker,OSA) %>% summarise(count=n()) %>%
  ggplot(aes(x=Smoker, y=count, fill=OSA)) +
  geom_bar(stat="identity")

p_Snorer <- df_OSA_rm %>% group_by(Snorer,OSA) %>% summarise(count=n()) %>%
  ggplot(aes(x=Snorer, y=count, fill=OSA)) +
  geom_bar(stat="identity") 

combined <- p_Gender + p_Smoker + p_Snorer & theme(legend.position = "bottom")
combined + plot_layout(guides = "collect")

##Rate

p_Gender <- df_OSA %>% group_by(Gender) %>% summarise(
  Healthy=sum(OSA=="Healthy")/n(),
  Mild=sum(OSA=="Mild")/n(),
  Moderate=sum(OSA=="Moderate")/n(),
  Severe=sum(OSA=="Severe")/n()) %>% 
  gather("OSA","rate",-Gender ) %>% 
  ggplot(aes(x=Gender, y=rate, fill=OSA)) +
  geom_bar(stat="identity") + theme(legend.position="none")

level_order_smoker <- c('yes', 'no', 'old', 'ns')
p_Smoker <- df_OSA %>% group_by(Smoker) %>% summarise(
  Healthy=sum(OSA=="Healthy")/n(),
  Mild=sum(OSA=="Mild")/n(),
  Moderate=sum(OSA=="Moderate")/n(),
  Severe=sum(OSA=="Severe")/n()) %>% 
  gather("OSA","rate",-Smoker ) %>%
  ggplot(aes(x=factor(Smoker,level=level_order_smoker), y=rate, fill=OSA)) +
  geom_bar(stat="identity") + theme(legend.position="none") + xlab("Smoker") 

level_order_snorer <- c('no with CPAD', 'no', 'ns', 'yes', 'CPAP')
p_Snorer <- df_OSA %>% group_by(Snorer) %>% summarise(
  Healthy=sum(OSA=="Healthy")/n(),
  Mild=sum(OSA=="Mild")/n(),
  Moderate=sum(OSA=="Moderate")/n(),
  Severe=sum(OSA=="Severe")/n()) %>% 
  gather("OSA","rate",-Snorer ) %>% 
  ggplot(aes(x=factor(Snorer,level=level_order_snorer), y=rate, fill=OSA)) +
  geom_bar(stat="identity") + theme(legend.position="none") + xlab("Snorer")

combined <- p_Gender + p_Smoker + p_Snorer & theme(legend.position = "bottom")
combined + plot_layout(guides = "collect")

p_Gender <- df_OSA_rm %>% group_by(Gender) %>% summarise(
              Healthy=sum(OSA=="Healthy")/n(),
              Mild=sum(OSA=="Mild")/n(),
              Moderate=sum(OSA=="Moderate")/n(),
              Severe=sum(OSA=="Severe")/n()) %>% 
  gather("OSA","rate",-Gender ) %>% 
  ggplot(aes(x=Gender, y=rate, fill=OSA)) +
  geom_bar(stat="identity") + theme(legend.position="none")

level_order_smoker <- c('yes', 'no', 'old', 'ns')
p_Smoker <- df_OSA_rm %>% group_by(Smoker) %>% summarise(
              Healthy=sum(OSA=="Healthy")/n(),
              Mild=sum(OSA=="Mild")/n(),
              Moderate=sum(OSA=="Moderate")/n(),
              Severe=sum(OSA=="Severe")/n()) %>% 
  gather("OSA","rate",-Smoker ) %>%
  ggplot(aes(x=factor(Smoker,level=level_order_smoker), y=rate, fill=OSA)) +
  geom_bar(stat="identity") + theme(legend.position="none") + xlab("Smoker") 

level_order_snorer <- c('no with CPAD', 'no', 'ns', 'yes', 'CPAP')
p_Snorer <- df_OSA_rm %>% group_by(Snorer) %>% summarise(
              Healthy=sum(OSA=="Healthy")/n(),
              Mild=sum(OSA=="Mild")/n(),
              Moderate=sum(OSA=="Moderate")/n(),
              Severe=sum(OSA=="Severe")/n()) %>% 
  gather("OSA","rate",-Snorer ) %>% 
  ggplot(aes(x=factor(Snorer,level=level_order_snorer), y=rate, fill=OSA)) +
  geom_bar(stat="identity") + theme(legend.position="none") + xlab("Snorer")

combined <- p_Gender + p_Smoker + p_Snorer & theme(legend.position = "bottom")
combined + plot_layout(guides = "collect")

##### Chi-square-test
##### Non-parametric test

chisq.test(df_OSA$OSA, df_OSA$Gender)
chisq.test(df_OSA$OSA, df_OSA$Smoker)
chisq.test(df_OSA$OSA, df_OSA$Snorer)

chisq.test(df_OSA_rm$OSA, df_OSA_rm$Gender)
chisq.test(df_OSA_rm$OSA, df_OSA_rm$Smoker)
chisq.test(df_OSA_rm$OSA, df_OSA_rm$Snorer)

chisq.test(df_OSA_no_leves$OSA, df_OSA_no_leves$Gender)
chisq.test(df_OSA_no_leves$OSA, df_OSA_no_leves$Smoker)
chisq.test(df_OSA_no_leves$OSA, df_OSA_no_leves$Snorer)


################################################################
################## Numerical Attributes ########################
################################################################

p1 <- xyplot(BMI ~ Cervical, 
       groups =  OSA, data = df_OSA_rm,
       auto.key = list(corner = c(1, 1), cex = 0.7), pch=4)
p2 <- xyplot(Cervical ~ Age, 
       groups =  OSA, data = df_OSA_rm,
       auto.key = list(corner = c(1, 1), cex = 0.7), pch=4)
p3 <- xyplot(Age ~ BMI, 
       groups =  OSA, data = df_OSA_rm,
       auto.key = list(corner = c(1, 1), cex = 0.7), pch=4)

grid.arrange(p1, p2, p3, ncol = 3, nrow=1)

p1 <- ggplot(df_OSA_rm, aes(x = BMI)) +
  geom_histogram(aes(color = OSA), fill = "transparent",
                 position = "identity", bins = 30, alpha = 0.1) +
  scale_color_manual(values = c("#00AF00", "#E7B800")) +
  scale_fill_manual(values = c("#00AF00", "#E7B800"))

p2 <- ggplot(df_OSA_rm, aes(x = Cervical)) +
  geom_histogram(aes(color = OSA), fill = "transparent",
                 position = "identity", bins = 30, alpha = 0.1) +
  scale_color_manual(values = c("#00AF00", "#E7B800")) +
  scale_fill_manual(values = c("#00AF00", "#E7B800"))

p3 <- ggplot(df_OSA_rm, aes(x = Age)) +
  geom_histogram(aes(color = OSA), fill = "transparent",
                 position = "identity", bins = 30, alpha = 0.1) +
  scale_color_manual(values = c("#00AF00", "#E7B800")) +
  scale_fill_manual(values = c("#00AF00", "#E7B800"))

grid.arrange(p1, p2, p3, ncol = 3, nrow=1)

lda(OSA~Weight+BMI+Age+Height+Cervical, data = df_OSA)
lda(OSA~Weight+BMI+Age+Height+Cervical, data = df_OSA_rm)

kruskal.test(Weight ~ OSA, data = df_OSA)$p.value
kruskal.test(Height ~ OSA, data = df_OSA)$p.value 
kruskal.test(BMI ~ OSA, data = df_OSA)$p.value
kruskal.test(Age ~ OSA, data = df_OSA)$p.value  
kruskal.test(Cervical ~ OSA, data = df_OSA)$p.value

kruskal.test(Weight ~ OSA, data = df_OSA_rm)
kruskal.test(Height ~ OSA, data = df_OSA_rm)$p.value 
kruskal.test(BMI ~ OSA, data = df_OSA_rm)$p.value
kruskal.test(Age ~ OSA, data = df_OSA_rm)$p.value  
kruskal.test(Cervical ~ OSA, data = df_OSA_rm)$p.value

################################################################
################## Feature Combination ########################
################################################################

df_OSA_lm <- df_OSA_rm %>% 
  select(Patient, Gender, Smoker, Snorer, OSA, Weight, Height, BMI, Age, Cervical) %>%
  mutate(Gender = as.numeric(Gender), Snorer=as.numeric(Snorer), Smoker=as.numeric(Smoker))

glm.fit <- glm(OSA ~ Weight+BMI+Gender+Smoker+Snorer+Height+Age+Cervical, data = df_OSA_lm, family = binomial)
summary(glm.fit)

glm_Weight.fit <- glm(OSA ~ Weight, data = df_OSA_lm, family=binomial)
summary(glm_Weight.fit)

glm_BMI.fit <- glm(OSA ~ BMI, data = df_OSA_lm, family=binomial)
summary(glm_BMI.fit)

glm_Gender.fit <- glm(OSA ~ Gender, data = df_OSA_lm, family=binomial)
summary(glm_Gender.fit)

glm_Smoker.fit <- glm(OSA ~ Smoker, data = df_OSA_lm, family=binomial)
summary(glm_Smoker.fit)

glm_Snorer.fit <- glm(OSA ~ Snorer, data = df_OSA_lm, family=binomial)
summary(glm_Snorer.fit)

glm_Height.fit <- glm(OSA ~ Height, data = df_OSA_lm, family=binomial)
summary(glm_Height.fit)

glm_Age.fit <- glm(OSA ~ Age, data = df_OSA_lm, family=binomial)
summary(glm_Age.fit)

glm_Cervical.fit <- glm(OSA ~ Cervical, data = df_OSA_lm, family=binomial)
summary(glm_Cervical.fit)

### Best model
glm_best.fit <- glm(OSA ~ Weight+BMI+Gender+Snorer+Height+Age, data = df_OSA_lm, family = binomial)
glm_best.fit$aic
summary(glm_best.fit)

