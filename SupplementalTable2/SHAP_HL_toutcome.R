options(scipen=999)
library(tidyverse)
library(dplyr)
library(haven)
library(glmnet)
library(caret)
library(xgboost)
 
#setwd('O:/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/Update_202503/Analysis/RESULTS/HL_outcome/')
#indata <- read_sas('O:/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/data/allvars_for_shap1.sas7bdat') 
setwd('/home/data/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/Update_202503/Analysis/RESULTS/HL_outcome/')
indata <- read_sas('/home/data/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/data/allvars_for_shap1.sas7bdat') 


sampleweight <- indata$weight_lsw3_or_lsw4_norm1

skip <- c('masterid','MASTERID','secu','str','weight_lsw3_or_lsw4_norm1','fold','riskcat','risk_saonly','risk_hlonly','risk_sdonly','risk_2plus','SA_outcomef','hl_final')


Y <- indata$hl_final

x <- indata[,!colnames(indata) %in% skip] 
x_mat <- model.matrix(~-1 + ., x)
fold <- indata$fold

set.seed(1)
cv_lasso <- cv.glmnet(
  x = x_mat,
  y = Y,
  type.measure = 'deviance',
  folds = 10,
  foldid = fold,
  family = 'binomial',
  alpha = 1,
  weights = sampleweight,
  nlambda = 100
)

saveRDS(cv_lasso,'HL_outcome.rds')


#lasso_coefs <- cv_lasso$glmnet.fit$beta[,cv_lasso$index['min',]] %>% data.frame() %>% rename_with(.cols = 1 ~'coef') %>% rownames_to_column(var = 'var')
lasso_coefs <- cv_lasso$glmnet.fit$beta[,cv_lasso$index['min',]] %>% data.frame() %>% rownames_to_column(var = 'var')
colnames(lasso_coefs)[2] <- 'coef'



pred_lasso <- predict(cv_lasso, x_mat, s = 'lambda.min', type = 'response')

#shap_data <- data.frame(
#  pred_100k  = as.numeric(pred_lasso) * 100000,
#  x[,lasso_coefs$var]
#)


betas <- lasso_coefs[lasso_coefs$coef != '0', ]

shap_data <- data.frame(
  pred_100k  = as.numeric(pred_lasso) * 100000,
  x[,betas$var]
)


write.csv(lasso_coefs,'HL_outcome_lasso_coefs.csv',row.names=F)
write.csv(shap_data,'HL_outcome_shapdata.csv',row.names=F)

tuneControl <- trainControl(
  method = 'cv',
  number = 10
)

xgbGrid <- expand.grid(
  nrounds = 100,
  max_depth = 2:5,
  eta = c(0.01, 0.05, 0.1, 0.2, 0.3),
  gamma = 1,
  subsample = (5:9)/10,
  colsample_bytree = (5:9)/10,
  rate_drop = c(0,5,10)/100,
  skip_drop = c(0,0.5),
  min_child_weight = 1
)

set.seed(2)
xgbGrid_25 <- xgbGrid[sample(1:nrow(xgbGrid),25),]

set.seed(3)
xgbFit <- train(
  pred_100k ~ ., data = shap_data,
  method = 'xgbDART',
  trControl = tuneControl,
  tuneGrid = xgbGrid_25,
  weights = sampleweight 
)

saveRDS(xgbFit,'xgbFit_HL_outcome.rds')

#xgbFit <- readRDS('xgbFit_HL_outcome.rds')

#shap_data<-read.csv('HL_outcome_shapdata.csv')
 
xgb_shap <- predict(xgbFit$finalModel, as.matrix(shap_data[,xgbFit$finalModel$xNames]), predcontrib = TRUE)

xgb_shap <- xgb_shap[,-ncol(xgb_shap)] # Drop BIAS col
xgb_shap <- xgb_shap[,which(apply(xgb_shap, 2, function(x) all(x) != 0))]  # Keep important vars

# Total mean absolute SHAP
mean_abs_shap_total <- rowSums(xgb_shap) %>% abs() %>% mean()
mean_abs_shap_total

# Read in labels. Assumes there are columns called "var" and "category"
var_labels <- read_csv('var_labels.csv') %>%
  filter(var %in% colnames(xgb_shap))

demo_shap <- xgb_shap[,colnames(xgb_shap) %in% var_labels$var[var_labels$category == '1. Demographics']]
army_shap <- xgb_shap[,colnames(xgb_shap) %in% var_labels$var[var_labels$category == '2. Army career']]
psy_shap <- xgb_shap[,colnames(xgb_shap) %in% var_labels$var[var_labels$category == '3. Psychopathological risk factors']]
phys_shap <- xgb_shap[,colnames(xgb_shap) %in% var_labels$var[var_labels$category == '4. Physical disorders']] 
stress_shap <- xgb_shap[,colnames(xgb_shap) %in% var_labels$var[var_labels$category == '5. Stressors']]

# Mean absolute SHAP by category
rowSums(demo_shap) %>% abs() %>% mean()
rowSums(army_shap) %>% abs() %>% mean()
rowSums(psy_shap) %>% abs() %>% mean()
rowSums(phys_shap) %>% abs() %>% mean() 
rowSums(stress_shap) %>% abs() %>% mean()

# Mean absolute SHAP by variable, descending order within categories 
shap_table <- apply(xgb_shap, 2, function(x) mean(abs(x))) %>% data.frame() %>%
  rename_with(.cols = 1, ~ 'mean_abs_shap_100k') %>% rownames_to_column('var') %>%
  left_join(var_labels) %>% select(var, category, label, mean_abs_shap_100k) %>%
  arrange(category, -mean_abs_shap_100k)

write.csv(xgb_shap,'HL_outcome_xgb_shap.csv',row.names=F)
write.csv(shap_table,'HL_outcome_shap_table.csv',row.names=F)