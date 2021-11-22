library(dplyr)
library(tidyverse)
library(readxl)
library(caret)
library(gridExtra)
library(rpart.plot)

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

######################################################################
######################### Pretask: Test data #########################
######################################################################
set.seed(101)
smp_size <- floor(0.9 * nrow(df_OSA))
train_ind <- sample(seq_len(nrow(df_OSA)), size = smp_size)
df_OSA.train=df_OSA[train_ind,]
df_OSA.test=df_OSA[-train_ind,]

smp_size_rm <- floor(0.9 * nrow(df_OSA_rm))
train_ind_rm <- sample(seq_len(nrow(df_OSA_rm)), size = smp_size_rm)
df_OSA_rm.train=df_OSA_rm[train_ind_rm,]
df_OSA_rm.test=df_OSA_rm[-train_ind_rm,]

# For 10-Fold crossvalidation
train.control <- trainControl(method = "cv", number = 10, savePredictions=TRUE)

######################################################################
#################### Pretask: Get Metric Function ####################
######################################################################

print_metrics <- function(model, df.train, df.test, attrs) {
  
  pred_test  = predict(model, df.test)
  
  df.test$predicted = pred_test
  samples.test = nrow(df.test)
  
  df_data.test <- df.test %>%
    mutate(goodbad = ifelse(OSA == predicted, "good", "bad")) %>%
    group_by(OSA, predicted, goodbad) %>%
    summarise(N = n(), rate = n()/samples.test)
  
  ## Confusion matrix
  p <- df_data.test %>% mutate_all(~replace(., is.na(.), 0)) %>%
    ggplot(mapping = aes(x = reorder(OSA,desc(OSA)), y = predicted, fill = goodbad, alpha = rate)) +
    geom_tile() +
    geom_text(aes(label = N), vjust = .5, fontface  = "bold", alpha = 1) +
    scale_fill_manual(values = c(good = "green", bad = "red")) +
    theme_bw() +
    xlim(rev(levels(df_data.test$OSA))) +
    xlab("Actual OSA") + ylab("Predicted OSA") +
    scale_x_discrete(position = "top") 
  
  tb_data.test <- df_data.test %>% select(OSA, predicted, N) %>% spread(OSA, N) %>% 
      ungroup %>% select(-predicted) %>% mutate_all(~replace(., is.na(.), 0))
  tb_data.test <- as_tibble(tb_data.test)
  accuracy = 0
  for (i in 1:nrow(tb_data.test)) {
    accuracy<-accuracy+tb_data.test[i,i]
  }
  accuracy <- as.numeric(accuracy / samples.test)
  
  Pe = 0
  for (i in 1:nrow(tb_data.test)) {
    Pe <- Pe + sum(tb_data.test[i,1:nrow(tb_data.test)])*sum(tb_data.test[1:nrow(tb_data.test),i])
  }
  Pe <- Pe /samples.test^2
  
  kappa = (accuracy-Pe)/(1-Pe)
  
  ci_acc <- 1.96 * sqrt(accuracy*(1-accuracy)/samples.test)
  print(paste("accuracy: ","$", round(accuracy,3), "+-", round(ci_acc,3),"$"))
  
  ci_kappa <- 1.96 * sqrt(accuracy*(1-accuracy)/(1-Pe)/(1-Pe)/samples.test)
  print(paste("kappa: ","$",round(kappa,3), " +-", round(ci_kappa,3),"$"))
  
  return(p)
  
}

get_metrics <- function(model, df.train, df.test, lvs, model_name) {
  
  pred_test <- predict(model, df.test)
  
  df.test$predicted = pred_test
  samples.test = nrow(df.test)
  
  df_data.test <- df.test %>%
    mutate(goodbad = ifelse(OSA == predicted, "good", "bad")) %>%
    group_by(OSA, predicted, goodbad) %>%
    summarise(N = n(), rate = n()/samples.test)
  
  tb_data.test <- df_data.test %>% select(OSA, predicted, N) %>% spread(OSA, N) %>% 
    ungroup %>% select(-predicted) %>% mutate_all(~replace(., is.na(.), 0))
  tb_data.test <- as_tibble(tb_data.test)
  
  accuracy = 0
  for (i in 1:nrow(tb_data.test)) {
    accuracy<-accuracy+tb_data.test[i,i]
  }
  accuracy <- as.numeric(accuracy / samples.test)
  
  Pe = 0
  for (i in 1:nrow(tb_data.test)) {
    Pe <- Pe + sum(tb_data.test[i,1:nrow(tb_data.test)])*sum(tb_data.test[1:nrow(tb_data.test),i])
  }
  Pe <- Pe /samples.test^2
  
  kappa = (accuracy-Pe)/(1-Pe)
  
  ci_acc <- 1.96 * sqrt(accuracy*(1-accuracy)/samples.test)
  ci_kappa <- 1.96 * sqrt(accuracy*(1-accuracy)/(1-Pe)/(1-Pe)/samples.test)
  
  res <- data.frame(Model=model_name, lvs=lvs, acc=accuracy, ci_acc=ci_acc, kappa=kappa, ci_kappa=ci_kappa)
  return(res)
}

######################################################################
########################### Reguralization ###########################
######################################################################

set.seed(99)
regularization_model <- train(OSA ~Cervical + Weight + BMI + Gender + Height + Snorer + Smoker + Age, data = df_OSA.train,
                              method = "regLogistic", trControl=train.control)

regularization_model_rm <-  train(OSA ~Cervical + Weight + BMI + Gender + Height + Snorer + Smoker + Age, data = df_OSA_rm.train,
                                  method = "regLogistic", trControl=train.control)

#regularization_model
#regularization_model_rm

print_metrics(regularization_model, df_OSA.train, df_OSA.test)
print_metrics(regularization_model_rm, df_OSA_rm.train, df_OSA_rm.test)


######################################################################
################################# KNN ################################
######################################################################

set.seed(98)
knn_model <- train(OSA ~Cervical + Weight + BMI + Gender + Height + Snorer + Smoker + Age, data = df_OSA.train,
                              method = "knn", tuneLength=30, preProcess = c("center","scale"))

knn_model_rm <-  train(OSA ~Cervical + Weight + BMI + Gender + Height + Smoker + Age, data = df_OSA_rm.train,
                                  method = "knn", tuneLength=30, preProcess = c("center","scale"))

#knn_model
#knn_model_rm

print_metrics(knn_model, df_OSA.train, df_OSA.test)
print_metrics(knn_model_rm, df_OSA_rm.train, df_OSA_rm.test)

######################################################################
######################## Tree based methods  #########################
######################################################################

set.seed(99)
tree <- rpart(OSA ~ Cervical + Weight + BMI + Gender + Height + Snorer + Smoker + Age, data = df_OSA.train, method="class")
tree_rm <- rpart(OSA ~ Cervical + Weight + BMI + Gender + Height + Snorer + Smoker + Age, data = df_OSA_rm.train, method="class")
rpart.plot(tree)
rpart.plot(tree_rm)

rf_model <- train(OSA ~Cervical + Weight + BMI + Gender + Height + Snorer + Smoker + Age, data = df_OSA.train,
                  method = "rf", trControl=train.control, tuneLength=10)
rf_model_rm <- train(OSA ~Cervical + Weight + BMI + Gender + Height + Snorer + Smoker + Age, data = df_OSA_rm.train,
                  method = "rf", trControl=train.control, tuneLength=10)

xgb_tree <- train(OSA ~Cervical + Weight + BMI + Gender + Height + Snorer + Smoker + Age, data = df_OSA.train,
                method = "xgbTree", trControl=train.control, tuneLength=4)
xgb_tree_rm <- train(OSA ~Cervical + Weight + BMI + Gender + Height + Snorer + Smoker + Age, data = df_OSA_rm.train,
                     method = "xgbTree", trControl=train.control, tuneLength=4)


print_metrics(tree, df_OSA.train, df_OSA.test)
print_metrics(tree_rm, df_OSA_rm.train, df_OSA_rm.test)
print_metrics(rf_model, df_OSA.train, df_OSA.test)
print_metrics(rf_model_rm, df_OSA_rm.train, df_OSA_rm.test)
get_metrics(xgb_tree, df_OSA.train, df_OSA.test)
get_metrics(xgb_tree_rm, df_OSA_rm.train, df_OSA_rm.test)

######################################################################
###################### Support Vector Machine  #######################
######################################################################

set.seed(98)
svm_model <- train(OSA ~Cervical + Weight + BMI + Gender + Height + Smoker + Age, data = df_OSA.train,
                   method = "svmLinear", trControl=train.control, preProcess = c("center","scale"),
                   tuneGrid = expand.grid(C = seq(0.1, 2, length = 20)))
svm_model_rm <- train(OSA ~Cervical + Weight + BMI + Gender + Height + Smoker + Age, data = df_OSA_rm.train,
                          method = "svmLinear", trControl=train.control, preProcess = c("center","scale"),
                          tuneGrid = expand.grid(C = seq(0.1, 2, length = 20)))

svm_poly_model <- train(OSA ~Cervical + Weight + BMI + Gender + Height + Smoker + Age, data = df_OSA.train,
                        method = "svmPoly", trControl=train.control, preProcess = c("center","scale"),
                        tuneLength=4)
svm_poly_model_rm <- train(OSA ~Cervical + Weight + BMI + Gender + Height + Smoker + Age, data = df_OSA_rm.train,
                               method = "svmPoly", trControl=train.control, preProcess = c("center","scale"),
                               tuneLength=4)

svm_radial_model <- train(OSA ~Cervical + Weight + BMI + Gender + Height + Smoker + Age, data = df_OSA.train,
                          method = "svmRadial", trControl=train.control, preProcess = c("center","scale"),
                          tuneLength=20)
svm_radial_model_rm <- train(OSA ~Cervical + Weight + BMI + Gender + Height + Smoker + Age, data = df_OSA_rm.train,
                                 method = "svmRadial", trControl=train.control, preProcess = c("center","scale"),
                                 tuneLength=20)

#svm_model
#svm_model_rm
#svm_poly_model
#svm_poly_model_rm
#svm_radial_model
#svm_radial_model_rm

print_metrics(svm_model, df_OSA.train, df_OSA.test)
#print_metrics(svm_model_rm, df_OSA_rm.train, df_OSA_rm.test)
print_metrics(svm_poly_model, df_OSA.train, df_OSA.test)
#print_metrics(svm_poly_model_rm, df_OSA_rm.train, df_OSA_rm.test)
#print_metrics(svm_radial_model, df_OSA.train, df_OSA.test)
#print_metrics(svm_radial_model_rm, df_OSA_rm.train, df_OSA_rm.test)


######################################################################
############################ Comparison  #############################
######################################################################
df_comparison = data.frame(Model=c(), lvs=c(), acc=c(), ci_acc=c(), kappa=c(), ci_kappa=c())

df_comparison <- rbind(df_comparison, get_metrics(regularization_model, df_OSA.train, df_OSA.test, "All", "Regularized LR"))
df_comparison <- rbind(df_comparison, get_metrics(knn_model,            df_OSA.train, df_OSA.test, "All", "kNN"))
df_comparison <- rbind(df_comparison, get_metrics(tree,                 df_OSA.train, df_OSA.test, "All", "Tree"))
df_comparison <- rbind(df_comparison, get_metrics(rf_model,             df_OSA.train, df_OSA.test, "All", "Random Forest"))
df_comparison <- rbind(df_comparison, get_metrics(xgb_tree,             df_OSA.train, df_OSA.test, "All", "XGBTree"))
df_comparison <- rbind(df_comparison, get_metrics(svm_model,            df_OSA.train, df_OSA.test, "All", "SVM Linear"))
df_comparison <- rbind(df_comparison, get_metrics(svm_poly_model,       df_OSA.train, df_OSA.test, "All", "SVM Polynomial"))
df_comparison <- rbind(df_comparison, get_metrics(svm_radial_model,     df_OSA.train, df_OSA.test, "All", "SVM Radial"))

df_comparison <- rbind(df_comparison, get_metrics(regularization_model_rm, df_OSA_rm.train, df_OSA_rm.test, "H&S", "Regularized LR"))
df_comparison <- rbind(df_comparison, get_metrics(knn_model_rm,            df_OSA_rm.train, df_OSA_rm.test, "H&S", "kNN"))
df_comparison <- rbind(df_comparison, get_metrics(tree_rm,                 df_OSA_rm.train, df_OSA_rm.test, "H&S", "Tree"))
df_comparison <- rbind(df_comparison, get_metrics(rf_model_rm,             df_OSA_rm.train, df_OSA_rm.test, "H&S", "Random Forest"))
df_comparison <- rbind(df_comparison, get_metrics(xgb_tree_rm,             df_OSA_rm.train, df_OSA_rm.test, "H&S", "XGBTree"))
df_comparison <- rbind(df_comparison, get_metrics(svm_model_rm,            df_OSA_rm.train, df_OSA_rm.test, "H&S", "SVM Linear"))
df_comparison <- rbind(df_comparison, get_metrics(svm_poly_model_rm,       df_OSA_rm.train, df_OSA_rm.test, "H&S", "SVM Polynomial"))
df_comparison <- rbind(df_comparison, get_metrics(svm_radial_model_rm,     df_OSA_rm.train, df_OSA_rm.test, "H&S", "SVM Radial"))

df_comparison <- df_comparison %>%
  gather("metric", "value", acc, kappa) %>%
  mutate(ci = ifelse(metric=="acc", ci_acc,ci_kappa)) %>%
  select(Model, lvs, metric, value, ci)

p1 <- df_comparison %>% 
  filter(lvs=="All", metric=="acc") %>% 
  ggplot(aes(x=reorder(Model,value), y=value, color = metric)) +
  geom_line() + geom_point() +
  geom_errorbar(aes(ymin=value-ci, ymax=value+ci), width=.4) +
  theme(axis.text.x = element_text(angle = 30, vjust = 0.5)) +
  labs(x="Model", y="Accuracy")+
  ylim(0,1) +
  theme(legend.position = "none")

p2 <- df_comparison %>% 
  filter(lvs=="All", metric=="kappa") %>% 
  ggplot(aes(x=reorder(Model,value), y=value, color = metric)) +
  geom_line() + geom_point() +
  geom_errorbar(aes(ymin=ifelse(value-ci>0,value-ci,0), ymax=value+ci), width=.4) +
  theme(axis.text.x = element_text(angle = 30, vjust = 0.5)) +
  labs(x="Model", y="kappa")+
  ylim(0,1) +
  theme(legend.position = "none")

p3 <- df_comparison %>% 
  filter(lvs=="H&S", metric=="acc") %>% 
  ggplot(aes(x=reorder(Model,value), y=value, color = metric)) +
  geom_line() + geom_point() +
  geom_errorbar(aes(ymin=value-ci, ymax=value+ci), width=.4) +
  theme(axis.text.x = element_text(angle = 30, vjust = 0.5)) +
  labs(x="Model", y="Accuracy")+
  ylim(0,1) +
  theme(legend.position = "none")

p4 <-df_comparison %>% 
  filter(lvs=="H&S", metric=="kappa") %>% 
  ggplot(aes(x=reorder(Model,value), y=value, color = metric)) +
  geom_line() + geom_point() +
  geom_errorbar(aes(ymin=ifelse(value-ci>0,value-ci,0), ymax=value+ci), width=.4) +
  theme(axis.text.x = element_text(angle = 30, vjust = 0.5)) +
  labs(x="Model", y="kappa")+
  ylim(0,1) +
  theme(legend.position = "none")

grid.arrange(p1,p2,p3,p4, ncol=2, nrow=2)

