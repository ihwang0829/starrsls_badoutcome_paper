

options(scipen=999)

library(tidyverse)
library(dplyr)
library(haven)
library(glmnet)
library(SuperLearner)
library(stringr)

#source('O:/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/code_from_va/WeightedBrierScore.R')
#source('O:/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/code_from_va/output_ici.R')
#source('O:/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/code_from_va/adj_pred.R')
#source('O:/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/code_from_va/SL.wsvm_corrected.R')
#source('O:/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/code_from_va/SL.nnet.1.R')
#source('O:/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/code_from_va/wrapper_SL_xgboost37.R')
#source('O:/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/code_from_va/create.Learner.grid.R')


#setwd('O:/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/data/cvtemps/SA/')

source('/home/data/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/code_from_va/WeightedBrierScore.R')
source('/home/data/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/code_from_va/output_ici.R')
source('/home/data/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/code_from_va/adj_pred.R')
source('/home/data/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/code_from_va/SL.wsvm_corrected.R')
source('/home/data/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/code_from_va/SL.nnet.1.R')
source('/home/data/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/code_from_va/wrapper_SL_xgboost37.R')
source('/home/data/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/code_from_va/create.Learner.grid.R')

setwd('/home/data/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/data/cvtemps/SA/')

cvAUCs <- read_sas('sa_cvaucs.sas7bdat')

keep_vars <- cvAUCs$Predictor



train_data <- read_sas('_sa_forcvauc.sas7bdat')

x_train <- train_data[,colnames(train_data) %in% keep_vars] %>%
  lapply(as.numeric) %>% data.frame()
Y_train <- train_data$SA_outcomef
weight_train <- train_data$weight_lsw3_or_lsw4_norm1

X <- x_train
Y <- Y_train
weight <- weight_train
 
validRows <- lapply(unique(train_data$fold), function(x) which(train_data$fold == x)) 


# Load data 
cvsl_sl <- readRDS('sacvsl.rds')

cv_pred_sl <- as.numeric(cvsl_sl$SL.predict)

summary(cvsl_sl$SL.predict)
summary(cv_pred_sl) 
str(cv_pred_sl)
#cv_pred_sl <- as.numeric(cvsl_sl$Z[,1])

# Stack 1
## Make some objects
idx_stack <- which(train_data$stack == 1)

stack_data <- train_data[idx_stack,]

validRows_stack <- lapply(
  unique(stack_data$fold),
  function(x)
    which(stack_data$fold == x)
)

Y_stack <- stack_data$SA_outcomef

weight_stack <- stack_data$weight_lsw3_or_lsw4_norm1

cv_pred_sl_stack <- cv_pred_sl[idx_stack]


summary(cv_pred_sl[idx_stack])
summary(cv_pred_sl)

fold_weight_stack <- lapply(
  validRows_stack,
  function(i)
    sum(weight_stack[i])
) %>% unlist()



summary(cv_pred_sl_stack)
str(cv_pred_sl_stack)
## Brier score
Brier_lasso_stack <- lapply(
  validRows_stack,
  function(i)
    WeightedBrierScore(
      x = Y_stack[i],
      pred = cv_pred_sl_stack[i],
      weight = weight_stack[i]
    )
) %>% unlist()

CV_Brier_lasso_stack <- weighted.mean(Brier_lasso_stack, w = fold_weight_stack)
CV_Brier_lasso_stack

## ICI
ici_lasso_stack <- lapply(
  validRows_stack,
  function(i)
    output_ici(Y=Y_stack[i], pred=cv_pred_sl_stack[i], pop_weight=weight_stack[i])
)

all_calib_lasso_stack <- lapply(
  ici_lasso_stack,
  function(x)
    data.frame(x$calib) %>% t()
) %>% do.call('rbind',.)

cv_ici_lasso_stack <- apply(
  all_calib_lasso_stack,
  2,
  weighted.mean,
  w = fold_weight_stack
)
cv_ici_lasso_stack


# Stack 2
## Make some objects
idx_stack <- which(train_data$stack == 2)

stack_data <- train_data[idx_stack,]

validRows_stack <- lapply(
  unique(stack_data$fold),
  function(x)
    which(stack_data$fold == x)
)

Y_stack <- stack_data$SA_outcomef

weight_stack <- stack_data$weight_lsw3_or_lsw4_norm1

cv_pred_sl_stack <- cv_pred_sl[idx_stack]

fold_weight_stack <- lapply(
  validRows_stack,
  function(i)
    sum(weight_stack[i])
) %>% unlist()

summary(cv_pred_sl_stack)
## Brier score
Brier_lasso_stack <- lapply(
  validRows_stack,
  function(i)
    WeightedBrierScore(
      x = Y_stack[i],
      pred = cv_pred_sl_stack[i],
      weight = weight_stack[i]
    )
) %>% unlist()

CV_Brier_lasso_stack <- weighted.mean(Brier_lasso_stack, w = fold_weight_stack)
CV_Brier_lasso_stack

## ICI
ici_lasso_stack <- lapply(
  validRows_stack,
  function(i)
    output_ici(Y=Y_stack[i], pred=cv_pred_sl_stack[i], pop_weight=weight_stack[i])
)

all_calib_lasso_stack <- lapply(
  ici_lasso_stack,
  function(x)
    data.frame(x$calib) %>% t()
) %>% do.call('rbind',.)

cv_ici_lasso_stack <- apply(
  all_calib_lasso_stack,
  2,
  weighted.mean,
  w = fold_weight_stack
)
cv_ici_lasso_stack


# Stack 3
## Make some objects
idx_stack <- which(train_data$stack == 3)

stack_data <- train_data[idx_stack,]

validRows_stack <- lapply(
  unique(stack_data$fold),
  function(x)
    which(stack_data$fold == x)
)

Y_stack <- stack_data$SA_outcomef

weight_stack <- stack_data$weight_lsw3_or_lsw4_norm1

cv_pred_sl_stack <- cv_pred_sl[idx_stack]

fold_weight_stack <- lapply(
  validRows_stack,
  function(i)
    sum(weight_stack[i])
) %>% unlist()

## Brier score
Brier_lasso_stack <- lapply(
  validRows_stack,
  function(i)
    WeightedBrierScore(
      x = Y_stack[i],
      pred = cv_pred_sl_stack[i],
      weight = weight_stack[i]
    )
) %>% unlist()

CV_Brier_lasso_stack <- weighted.mean(Brier_lasso_stack, w = fold_weight_stack)
CV_Brier_lasso_stack

## ICI
ici_lasso_stack <- lapply(
  validRows_stack,
  function(i)
    output_ici(Y=Y_stack[i], pred=cv_pred_sl_stack[i], pop_weight=weight_stack[i])
)

all_calib_lasso_stack <- lapply(
  ici_lasso_stack,
  function(x)
    data.frame(x$calib) %>% t()
) %>% do.call('rbind',.)

cv_ici_lasso_stack <- apply(
  all_calib_lasso_stack,
  2,
  weighted.mean,
  w = fold_weight_stack
)
cv_ici_lasso_stack
