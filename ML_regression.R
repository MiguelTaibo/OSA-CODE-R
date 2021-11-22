library(dplyr)
library(tidyverse)
library(readxl)
library(caret)
library(gridExtra)
library(rpart.plot)
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

######################################################################
######################### Pretask: Test data #########################
######################################################################
set.seed(101)
smp_size <- floor(0.90 * nrow(df_OSA))
train_ind <- sample(seq_len(nrow(df_OSA)), size = smp_size)
df_OSA.train=df_OSA[train_ind,]
df_OSA.test=df_OSA[-train_ind,]

# For 10-Fold crossvalidation
train.control <- trainControl(method = "cv", number = 10, savePredictions=TRUE)

######################################################################
#################### Pretask: Get Metric Function ####################
######################################################################

print_metrics <- function(model, df.train, df.test) {
  pred_train = predict(model, df.train)
  pred_test  = predict(model, df.test)
  
  MSE_Predict_train <- mean((df.train$AHI - pred_train)^2)
  MSE_Predict_test <- mean((df.test$AHI - pred_test)^2)
  MSE_Naive_test <- mean((df.test$AHI - mean(df.test$AHI))^2)
  
  print(paste("MSE Naive TEST   Predictor: ", round(MSE_Naive_test,0)))
  print(paste("MSE ", model$modelInfo$label ," TRAIN Predictor: ", round(MSE_Predict_train,0)))
  print(paste("MSE ", model$modelInfo$label ," TEST  Predictor: ", round(MSE_Predict_test,0)))
  
  MAE_Predict_train <- mean(abs(df.train$AHI - pred_train))
  MAE_Predict_test <- mean(abs(df.test$AHI - pred_test))
  MAE_Naive_test <- mean(abs(df.test$AHI - mean(df.test$AHI)))

  print(paste("MAE Naive TEST   Predictor: ", round(MAE_Naive_test,2)))
  print(paste("MAE ", model$modelInfo$label ," TRAIN Predictor: ", round(MAE_Predict_train,2)))
  print(paste("MAE ", model$modelInfo$label ," TEST  Predictor: ", round(MAE_Predict_test,2)))
  
  R_Squared_train <- 1-MSE_Predict_train/MSE_Naive_test
  R_Squared_test <- 1-MSE_Predict_test/MSE_Naive_test
  print(paste("R-Squared  ", "Naive" ," TRAIN Predictor: ", 0))
  print(paste("R-Squared ", model$modelInfo$label ," TRAIN Predictor: ", round(R_Squared_train,4)))
  print(paste("R-Squared ", model$modelInfo$label ," TEST  Predictor: ", round(R_Squared_test,4)))
  
}

get_metrics <- function(model, df.train, df.test, attrs, modelName) {
  pred_train = predict(model, df.train)
  pred_test  = predict(model, df.test)
  
  MSE_Predict_train <- mean((df.train$AHI - pred_train)^2)
  MSE_Predict_test <- mean((df.test$AHI - pred_test)^2)
  MSE_Naive_test <- mean((df.test$AHI - mean(df.test$AHI))^2)
  
  MAE_Predict_train <- mean(abs(df.train$AHI - pred_train))
  MAE_Predict_test <- mean(abs(df.test$AHI - pred_test))
  MAE_Naive_test <- mean(abs(df.test$AHI - mean(df.test$AHI)))
  
  R_Squared_train <- 1-MSE_Predict_train/MSE_Naive_test
  R_Squared_test <- 1-MSE_Predict_test/MSE_Naive_test
  
  res <- data.frame(Model=modelName, attrs=attrs,
                    MSE=MSE_Predict_test,
                    MAE=MAE_Predict_test,
                    R_Squared=R_Squared_test)
  return(res)
  
}


to_latex_row <- function(model, df.train, df.test) {
  pred_train = predict(model, df.train)
  pred_test  = predict(model, df.test)
  
  MSE_Predict_train <- mean((df.train$AHI - pred_train)^2)
  MSE_Predict_test <- mean((df.test$AHI - pred_test)^2)
  MSE_Naive_test <- mean((df.test$AHI - mean(df.test$AHI))^2)
  
  MAE_Predict_train <- mean(abs(df.train$AHI - pred_train))
  MAE_Predict_test <- mean(abs(df.test$AHI - pred_test))
  MAE_Naive_test <- mean(abs(df.test$AHI - mean(df.test$AHI)))
  
  R_Squared_train <- 1-MSE_Predict_train/MSE_Naive_test
  R_Squared_test <- 1-MSE_Predict_test/MSE_Naive_test
  
  sep = " & "
  res = paste(sep, round(MSE_Predict_train,0), sep, round(MAE_Predict_train,2), sep, round(R_Squared_train,4), 
              sep, round(MSE_Predict_test,0), sep ,round(MAE_Predict_test,2), sep, round(R_Squared_test,4))
  print(res)
}

all <- c("Weight","Cervical",  "Height",  "Age",  "BMI",  "Snorer",  "Smoker",  "Gender")
subset <- c("Cervical", "Weight",  "BMI",  "Gender",  "Height")


######################################################################
######################### Linear Regression ##########################
######################################################################
set.seed(1)
lm_all.model.train <- train(AHI ~Cervical + Weight + BMI + Gender + Height + Snorer + Smoker + Age, data = df_OSA.train,
                               method = "lm", trControl=train.control)

lm_subset.model.train <- train(AHI ~Cervical + Weight + BMI + Gender + Height, data = df_OSA.train,
                        method = "lm", trControl=train.control)

lm_all.model.train
lm_subset.model.train

to_latex_row(lm_all.model.train, df_OSA.train, df_OSA.test)
to_latex_row(lm_subset.model.train, df_OSA.train, df_OSA.test)

######################################################################
########################## reguralization  ###########################
######################################################################
set.seed(2)
# Training models
lasso.model.train <- train(AHI ~Cervical + Weight + BMI + Gender + Height + Snorer + Smoker + Age, data = df_OSA.train,
                            method = "lasso", trControl=train.control, tuneLength = 81)
lasso_subset.model.train <- train(AHI ~Cervical + Weight + BMI + Gender + Height, data = df_OSA.train,
                               method = "lasso", trControl=train.control,  tuneLength = 81)
ridge.model.train <- train(AHI ~Cervical + Weight + BMI + Gender + Height + Snorer + Smoker + Age, data = df_OSA.train,
                           method = "ridge", trControl=train.control, tuneLength = 81)
ridge_subset.model.train <- train(AHI ~Cervical + Weight + BMI + Gender + Height, data = df_OSA.train,
                           method = "ridge", trControl=train.control, tuneLength = 201)

# Hyperparameter search
p1 <- lasso.model.train$results %>% 
        ggplot(aes(x=fraction, y=RMSE)) + 
        geom_line() +
        geom_point(
          data = lasso.model.train$results %>% filter(min(RMSE)==RMSE),
          aes(x=fraction, y=RMSE), size=3, color="red" )  + ylim(16.1,16.6)
p2 <-lasso_subset.model.train$results %>% 
        ggplot(aes(x=fraction, y=RMSE)) + 
        geom_line() +
        geom_point(
          data = lasso_subset.model.train$results %>% filter(min(RMSE)==RMSE),
          aes(x=fraction, y=RMSE), size=3, color="red" ) + ylim(16.1,16.6)
p3 <- ridge.model.train$results %>% 
        ggplot(aes(x=lambda, y=RMSE)) + 
        geom_line() +
        geom_point(
          data = ridge.model.train$results %>% filter(min(RMSE)==RMSE),
          aes(x=lambda, y=RMSE), size=3, color="red" ) + 
        scale_x_continuous(trans='log10') + ylim(16.1,16.6)
p4 <- ridge_subset.model.train$results %>% 
        ggplot(aes(x=lambda, y=RMSE)) + 
        geom_line() +
        geom_point(
          data = ridge_subset.model.train$results %>% filter(min(RMSE)==RMSE),
          aes(x=lambda, y=RMSE), size=3, color="red" )+
        scale_x_continuous(trans='log10') + ylim(16.1,16.6)
grid.arrange(p1,p2,p3,p4, ncol=4, nrow=1)

# Metrics
to_latex_row(lasso.model.train, df_OSA.train, df_OSA.test)
to_latex_row(lasso_subset.model.train, df_OSA.train, df_OSA.test)
to_latex_row(ridge.model.train, df_OSA.train, df_OSA.test)
to_latex_row(ridge_subset.model.train, df_OSA.train, df_OSA.test)


######################################################################
######################## Tree based methods  #########################
######################################################################
set.seed(4)
tree <- rpart(AHI ~Cervical + Weight + BMI + Gender + Height + Snorer + Smoker + Age, data = df_OSA.train)
tree_subset <- rpart(AHI ~Cervical + Weight + BMI + Gender + Height, data = df_OSA.train)
rpart.plot(tree)

rf_model <- train(AHI ~Cervical + Weight + BMI + Gender + Height + Snorer + Smoker + Age, data = df_OSA.train,
                  method = "rf", trControl=train.control, tuneLength=10)
rf_model_subset <- train(AHI ~Cervical + Weight + BMI + Gender + Height, data = df_OSA.train,
                         method = "rf", trControl=train.control, tuneLength=10)

xgbTree <- train(AHI ~Cervical + Weight + BMI + Gender + Height + Snorer + Smoker + Age, data = df_OSA.train,
                          method = "xgbTree", trControl=train.control, tuneLength = 2)
xgbTree_subset <- train(AHI ~Cervical + Weight + BMI + Gender + Height, data = df_OSA.train,
                                 method = "xgbTree", trControl=train.control,  tuneLength = 10)

to_latex_row(tree, df_OSA.train, df_OSA.test)
to_latex_row(tree_subset, df_OSA.train, df_OSA.test)
to_latex_row(rf_model, df_OSA.train, df_OSA.test)
to_latex_row(rf_model_subset, df_OSA.train, df_OSA.test)
to_latex_row(xgbTree, df_OSA.train, df_OSA.test)
to_latex_row(xgbTree_subset, df_OSA.train, df_OSA.test)


######################################################################
###################### Support Vector Machine  #######################
######################################################################
set.seed(5)
svm_model <- train(AHI ~Cervical + Weight + BMI + Gender + Height + Snorer + Smoker + Age, data = df_OSA.train,
                  method = "svmLinear", trControl=train.control, preProcess = c("center","scale"),
                  tuneGrid = expand.grid(C = seq(0.1, 2, length = 20)))
svm_model_subset <- train(AHI ~Cervical + Weight + BMI + Gender + Height, data = df_OSA.train,
                          method = "svmLinear", trControl=train.control, preProcess = c("center","scale"),
                          tuneGrid = expand.grid(C = seq(0.1, 2, length = 20)))

svm_poly_model <- train(AHI ~Cervical + Weight + BMI + Gender + Height + Snorer + Smoker + Age, data = df_OSA.train,
                   method = "svmPoly", trControl=train.control, preProcess = c("center","scale"),
                   tuneLength=4)
svm_poly_model_subset <- train(AHI ~Cervical + Weight + BMI + Gender + Height, data = df_OSA.train,
                          method = "svmPoly", trControl=train.control, preProcess = c("center","scale"),
                          tuneLength=4)

svm_radial_model <- train(AHI ~Cervical + Weight + BMI + Gender + Height + Snorer + Smoker + Age, data = df_OSA.train,
                        method = "svmRadial", trControl=train.control, preProcess = c("center","scale"),
                        tuneLength=20)
svm_radial_model_subset <- train(AHI ~Cervical + Weight + BMI + Gender + Height, data = df_OSA.train,
                               method = "svmRadial", trControl=train.control, preProcess = c("center","scale"),
                               tuneLength=20)

svm_model
svm_model_subset
svm_poly_model
svm_poly_model_subset
svm_radial_model
svm_radial_model_subset

to_latex_row(svm_model, df_OSA.train, df_OSA.test)
to_latex_row(svm_model_subset, df_OSA.train, df_OSA.test)
to_latex_row(svm_poly_model, df_OSA.train, df_OSA.test)
to_latex_row(svm_poly_model_subset, df_OSA.train, df_OSA.test)
to_latex_row(svm_radial_model, df_OSA.train, df_OSA.test)
to_latex_row(svm_radial_model_subset, df_OSA.train, df_OSA.test)


######################################################################
############################ Comparison  #############################
######################################################################
df_comparison = data.frame(Model=c(), attrs=c(),
                           MSE=c(),
                           MAE=c(),
                           R_Squared=c())

df_comparison <- rbind(df_comparison, get_metrics(lm_all.model.train, df_OSA.train, df_OSA.test, "All", "Linear Regression"))
df_comparison <- rbind(df_comparison, get_metrics(lasso.model.train,  df_OSA.train, df_OSA.test, "All", "The lasso"))
df_comparison <- rbind(df_comparison, get_metrics(ridge.model.train,  df_OSA.train, df_OSA.test, "All", "Ridge Regression"))
df_comparison <- rbind(df_comparison, get_metrics(rf_model,           df_OSA.train, df_OSA.test, "All", "Random Forest"))
df_comparison <- rbind(df_comparison, get_metrics(xgbTree,            df_OSA.train, df_OSA.test, "All", "XGBTree"))
df_comparison <- rbind(df_comparison, get_metrics(svm_model,          df_OSA.train, df_OSA.test, "All", "SVM Linear"))
df_comparison <- rbind(df_comparison, get_metrics(svm_poly_model,     df_OSA.train, df_OSA.test, "All", "SVM Polynomial"))
df_comparison <- rbind(df_comparison, get_metrics(svm_radial_model,   df_OSA.train, df_OSA.test, "All", "SVM Radial"))

df_comparison <- rbind(df_comparison, get_metrics(lm_subset.model.train,    df_OSA.train, df_OSA.test, "Subset", "Linear Regression"))
df_comparison <- rbind(df_comparison, get_metrics(lasso_subset.model.train, df_OSA.train, df_OSA.test, "Subset", "The lasso"))
df_comparison <- rbind(df_comparison, get_metrics(ridge_subset.model.train, df_OSA.train, df_OSA.test, "Subset", "Ridge Regression"))
df_comparison <- rbind(df_comparison, get_metrics(rf_model_subset,          df_OSA.train, df_OSA.test, "Subset", "Random Forest"))
df_comparison <- rbind(df_comparison, get_metrics(xgbTree_subset,           df_OSA.train, df_OSA.test, "Subset", "XGBTree"))
df_comparison <- rbind(df_comparison, get_metrics(svm_model_subset,         df_OSA.train, df_OSA.test, "Subset", "SVM Linear"))
df_comparison <- rbind(df_comparison, get_metrics(svm_poly_model_subset,    df_OSA.train, df_OSA.test, "Subset", "SVM Polynomial"))
df_comparison <- rbind(df_comparison, get_metrics(svm_radial_model_subset,  df_OSA.train, df_OSA.test, "Subset", "SVM Radial"))

## Tidy the new data frame
df_comparison <- df_comparison %>%
  mutate(RMSE = sqrt(MSE)) %>%
  gather("metric", "value", MSE, MAE, R_Squared, RMSE) %>%
  mutate(metric=as.factor(metric))

p1 <- df_comparison %>% 
  filter(metric=="RMSE") %>% 
  ggplot(aes(x=reorder(Model,value), y=value, group = attrs, color=attrs)) +
  geom_line() + geom_point() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(x="Model", y="RMSE")+
  ylim(16.7,18.7) + theme(legend.position = "none")

p2 <- df_comparison %>% 
  filter(metric=="MAE") %>% 
  ggplot(aes(x=reorder(Model,value), y=value, group = attrs, color=attrs)) +
  geom_line() + geom_point() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(x="Model", y="MAE") + theme(legend.position = "none")

p3 <- df_comparison %>% 
  filter(metric=="R_Squared") %>% 
  ggplot(aes(x=reorder(Model,desc(value)), y=value, group = attrs, color=attrs)) +
  geom_line() + geom_point() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(x="Model", y="R-Squared") + theme(legend.position = "none")

combined <- p1 + p2 & theme(legend.position = "bottom")
combined + plot_layout(guides = "collect")
