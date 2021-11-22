
library(gridExtra)
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(lattice)
library(corrplot)
library("GGally")  

rm(list=ls())

## Read the file
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

################################################################
################# Categorical Attributes #######################
################################################################

stat_box_data <- function(y, upper_limit = max(iris$Sepal.Length) * 1.15) {
  return( 
    data.frame(
      y = 115,
      label = paste('N=', length(y))
    )
  )
}

# set the plotting area into a 1*2 array
p_Gender <- df_OSA %>% 
  ggplot(aes(x=reorder(Gender,AHI,na.rm = TRUE), y=AHI)) + 
  geom_boxplot() + labs(x="Gender", y="") +
  stat_summary(fun.data = stat_box_data, geom = "text") 

p_Smoker <- df_OSA %>% 
  ggplot(aes(x=reorder(Smoker,AHI,na.rm = TRUE), y=AHI)) + 
  geom_boxplot() + labs(x="Smoker", y="") +
  stat_summary(fun.data = stat_box_data, geom = "text") 

p_Snorer <- df_OSA %>% 
  ggplot(aes(x=reorder(Snorer,AHI,na.rm = TRUE), y=AHI)) + 
  geom_boxplot() + labs(x="Snorer", y="") +
  stat_summary(fun.data = stat_box_data, geom = "text") 

grid.arrange(p_Gender + labs(y="AHI"), p_Smoker, p_Snorer, ncol=3, nrow=1)

p_Gender <- df_OSA %>%
  ggplot(aes(x=reorder(Gender,AHI,na.rm = TRUE))) +
  geom_bar() + labs(x="Gender")
p_Smoker <- df_OSA %>%
  ggplot(aes(x=reorder(Smoker,AHI,na.rm = TRUE))) +
  geom_bar()  + labs(x="Smoker")
p_Snorer <- df_OSA %>%
  ggplot(aes(x=reorder(Snorer,AHI,na.rm = TRUE))) +
  geom_bar() + labs(x="Snorer")

grid.arrange(p_Gender, p_Smoker, p_Snorer, ncol=3, nrow=1)

## DF without central values

df_OSA_rm <- df_OSA %>% filter(OSA %in% c("Healthy", "Severe"))
p_Gender <- df_OSA_rm %>% 
  ggplot(aes(x=reorder(Gender,AHI,na.rm = TRUE), y=AHI)) + 
  geom_boxplot() + labs(x="Gender", y="") +
  stat_summary(fun.data = stat_box_data, geom = "text") 

p_Smoker <- df_OSA_rm %>% 
  ggplot(aes(x=reorder(Smoker,AHI,na.rm = TRUE), y=AHI)) + 
  geom_boxplot() + labs(x="Smoker", y="") +
  stat_summary(fun.data = stat_box_data, geom = "text") 

p_Snorer <- df_OSA_rm %>% 
  ggplot(aes(x=reorder(Snorer,AHI,na.rm = TRUE), y=AHI)) + 
  geom_boxplot() + labs(x="Snorer", y="") +
  stat_summary(fun.data = stat_box_data, geom = "text") 

grid.arrange(p_Gender + labs(y="AHI"), p_Smoker, p_Snorer, ncol=3, nrow=1)

p_Gender <- df_OSA_rm %>%
  ggplot(aes(x=reorder(Gender,AHI,na.rm = TRUE))) +
  geom_bar() + labs(x="Gender")
p_Smoker <- df_OSA_rm %>%
  ggplot(aes(x=reorder(Smoker,AHI,na.rm = TRUE))) +
  geom_bar()  + labs(x="Smoker")
p_Snorer <- df_OSA_rm %>%
  ggplot(aes(x=reorder(Snorer,AHI,na.rm = TRUE))) +
  geom_bar() + labs(x="Snorer")

grid.arrange(p_Gender, p_Smoker, p_Snorer, ncol=3, nrow=1)


df_to_pvalues <- function(df) {
  t_Gender_AHI <- kruskal.test(Gender ~ AHI, data = df) 
  t_Smoker_AHI <- kruskal.test(Smoker ~ AHI, data = df) 
  t_Snorer_AHI <- kruskal.test(Snorer ~ AHI, data = df) 
  
  kruskal_tests <- list(t_Gender_AHI, t_Smoker_AHI, t_Snorer_AHI)
  chis <- c()
  ps <- c()
  xs <- c()
  ys <- c()
  for (test in kruskal_tests) {
    chis <- append(chis, test$statistic)
    ps <- append(ps, test$p.value)
    tmp <-str_split(test$data.name, " by ")[[1]]
    xs <- append(xs,tmp[1])
    ys <- append(ys,tmp[2])
  }
  
  df_kruskal <- data.frame(x = xs, y = ys, chi = chis, p.value=ps)
  print(df_kruskal)
  df_kruskal %>% ggplot(aes(x=x, y=p.value, col=y)) + 
    geom_point() + labs(x="", y="p-value") +
    geom_hline(yintercept=0.05, linetype="dashed",)
  
}

df_to_pvalues(df_OSA)
df_to_pvalues(df_OSA_rm)

################################################################
################## Numerical Attributes ########################
################################################################

cor(df_OSA[,5:10])
corrplot(cor(df_OSA[,5:10]), method="color", addCoef.col = "black")

blue_pallete <- c("#d0efff", "#2a9df4", "#187bcd", "#1167b1")

df_OSA %>%  ggplot(aes(x=BMI, fill=OSA)) + 
  geom_histogram() + labs(x="BMI")

df_OSA %>% filter(OSA %in% c("Healthy", "Severe")) %>%
  ggplot(aes(x = BMI)) +
  geom_histogram(aes(color = OSA), position = "identity", bins = 30, alpha = 0.1) +
  scale_color_manual(values = blue_pallete) +
  scale_fill_manual(values = blue_pallete)

pairs(df_OSA %>% select(AHI, Weight, Height, BMI, Age, Cervical) %>% filter(AHI<70),
      bg =blue_pallete[df_OSA$OSA], 
      pch=21,
      col = blue_pallete[df_OSA$OSA],
      diag.panel = panel.hist,oma=c(3,3,3,15)) 
par(xpd = TRUE)
legend("bottomright", fill = blue_pallete[df_OSA$OSA], legend = c( levels(df_OSA$OSA)))

pairs(df_OSA_rm %>% select(AHI, Weight, Height, BMI, Age, Cervical) %>% filter(AHI<70),
      bg =blue_pallete[df_OSA$OSA], 
      pch=21,
      col = blue_pallete[df_OSA$OSA],
      diag.panel = panel.hist,oma=c(3,3,3,15)) 
par(xpd = TRUE)
legend("bottomright", fill = blue_pallete[df_OSA$OSA], legend = c( levels(df_OSA$OSA)))


ggpairs(df_OSA %>% select(AHI, Weight, Height, BMI, Age, Cervical) %>% filter(AHI<70))
      

################################################################
################## Feature Combination #########################
################################################################

#df_OSA_Gender = df_OSA %>% select(Patient, Gender, AHI, Weight, Height, BMI, Age, Cervical)
#df_OSA_Gender$Gender = as.numeric(df_OSA_Gender$Gender)
df_OSA_lm <- df_OSA %>% 
  select(Patient, Gender, Smoker, Snorer, AHI, Weight, Height, BMI, Age, Cervical) %>%
  mutate(Gender = as.numeric(Gender), Snorer=as.numeric(Snorer), Smoker=as.numeric(Smoker))

lm.fit=lm(AHI~Weight+BMI+Height+Cervical+Age+Gender+Snorer, data=subset(df_OSA_lm, Snorer %in% c(2,4,5)))
summary(lm.fit)

lm_weight.fit=lm(AHI~Weight, data=df_OSA)
summary(lm_weight.fit)

lm_BMI.fit=lm(AHI~BMI, data=df_OSA)
summary(lm_BMI.fit)

lm_cervial.fit=lm(AHI~Cervical, data=df_OSA)
summary(lm_cervial.fit)

lm_age.fit=lm(AHI~Age, data=df_OSA)
summary(lm_age.fit)

lm_height.fit=lm(AHI~Height, data=df_OSA)
summary(lm_height.fit)

lm_wc.fit=lm(AHI~Weight+Cervical, data=df_OSA)
summary(lm_wc.fit)

lm_snorer.fit=lm(AHI~Snorer, data=subset(df_OSA_lm, Snorer %in% c(2,4,5)))
summary(lm_snorer.fit)

lm_gender.fit=lm(AHI~Gender, data=df_OSA_lm)
summary(lm_gender.fit)

ggplot(aes(y=lm.fit$residuals)) +geom_violin()
boxplot(lm.fit$residuals)

lm.fit=lm(AHI~Weight+BMI+Height+Cervical+Age+Gender, data=df_OSA)
summary(lm.fit)

lm_males.fit=lm(AHI~Weight+BMI+Height+Cervical+Age, data=subset(df_OSA_lm, Gender=1))
lm_females.fit=lm(AHI~Weight+BMI+Height+Cervical+Age+Snorer, data=subset(df_OSA_lm, Gender=2))
summary(lm_males.fit)
summary(lm_females.fit)

df <- data.frame(Gender=factor(),  residual=double())
for (r in lm.fit$residuals) {
  new_row <- data.frame(Gender="all", residual=r);
  df <- rbind(df,new_row)
}
for (r in lm_males.fit$residuals) {
  new_row <- data.frame(Gender="male", residual=r);
  df <- rbind(df,new_row)
}
for (r in lm_females.fit$residuals) {
  new_row <- data.frame(Gender="females", residual=r);
  df <- rbind(df,new_row)
}
summary(df)
df %>% ggplot(aes(x=Gender, y=residual)) + geom_violin() +geom_boxplot(width=.2)
