
library(tidyverse)
library(readxl)
library(dplyr)
library(Rtsne)
library('randomForest')
library(caret) 
library(gridExtra)

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

############################################################
##################### Features Scaling #####################
############################################################

df_OSA_scaled <- df_OSA %>%
  mutate(Weight=scale(Weight)) %>%
  mutate(Age=scale(Age)) %>%
  mutate(Height=scale(Height)) %>%
  mutate(BMI=scale(BMI)) %>%
  mutate(Cervical=scale(Cervical))
  

p1 <- df_OSA %>% 
  ggplot(aes(x=Weight)) + geom_histogram()
p2 <- df_OSA %>% 
  ggplot(aes(x=Age)) + geom_histogram()
p3 <- df_OSA %>% 
  ggplot(aes(x=Height)) + geom_histogram()
p4 <- df_OSA %>% 
  ggplot(aes(x=BMI)) + geom_histogram()
p5 <- df_OSA %>% 
  ggplot(aes(x=Cervical)) + geom_histogram()

p6<- df_OSA_scaled %>% 
  ggplot(aes(x=Weight)) + geom_histogram()
p7<- df_OSA_scaled %>% 
  ggplot(aes(x=Age)) + geom_histogram()
p8<- df_OSA_scaled %>% 
  ggplot(aes(x=Height)) + geom_histogram()
p9<- df_OSA_scaled %>% 
  ggplot(aes(x=BMI)) + geom_histogram()
p10<- df_OSA_scaled %>% 
  ggplot(aes(x=Cervical)) + geom_histogram()

grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10, ncol=5, nrow=2)
############################################################
#################### Features selection ####################
############################################################

set.seed(101)
smp_size <- floor(0.75 * nrow(mtcars))
train_ind <- sample(seq_len(nrow(mtcars)), size = smp_size)
df_OSA.train=df_OSA[train_ind,]
df_OSA.test=df_OSA[-train_ind,]

########################################
########## Wrapping algorithms #########
########################################

control_rfe = rfeControl(functions = rfFuncs, # random forest
                         method = "repeatedcv", # repeated cv
                         repeats = 5, # number of repeats
                         number = 10) # number of folds
set.seed(50)
X_train <- df_OSA %>% select(Weight, Age, BMI, Height, Cervical, Gender, Smoker, Snorer)
Y_train <- df_OSA %>% select(AHI, OSA)

result_rfe_AHI <- 
  rfe(x = X_train[,1:8], y = Y_train[,1], 
      sizes = c(1:8),
      rfeControl = control_rfe)

result_rfe_AHI
predictors(result_rfe_AHI)


result_rfe_OSA <-
  rfe(x = X_train[,1:8], y = Y_train[,2], 
      sizes = c(1:8),
      rfeControl = control_rfe)

result_rfe_OSA
predictors(result_rfe_OSA)

library('Metrics')
library(caTools)


#applying Random Forest
model_rf<-randomForest(AHI ~ Weight+BMI+Height+Cervical+Age+Snorer+Smoker+Gender, data = df_OSA)
model_rf_rm<-randomForest(AHI ~ Weight+BMI+Height+Cervical+Age+Snorer+Smoker+Gender, data = df_OSA_rm)

model_rf_class <-randomForest(OSA ~ Weight+BMI+Height+Cervical+Age+Snorer+Smoker+Gender, data = df_OSA)
model_rf_rm_class <-randomForest(OSA ~ Weight+BMI+Height+Cervical+Age+Snorer+Smoker+Gender, data = df_OSA_rm)

preds<-predict(model_rf,df_OSA.test)
varImpPlot(model_rf) 
importance(model_rf)

model_rf_importance <- function(model_rf) {
  df <- data.frame(Feature=factor(),  Importance=double())
  features <- c("Weight","BMI","Height","Cervical","Age","Snorer","Smoker","Gender")
  i <- 1;
  for (f in features) {
    new_row <- data.frame(Feature=f, Importance=model_rf$importance[i]);
    df <- rbind(df,new_row)
    i <- i+1
  }
  df <- df %>% arrange(desc(Importance))
  print(df)
  df %>% ggplot(aes(x=reorder(Feature,desc(Importance)), y=Importance))+geom_col() +labs(x="Importance")
}

model_rf_importance(model_rf)
model_rf_importance(model_rf_rm)
model_rf_importance(model_rf_class)
model_rf_importance(model_rf_rm_class)

