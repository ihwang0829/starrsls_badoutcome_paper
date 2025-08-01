

options(scipen=999)

library(tidyverse)
library(dplyr)
library(haven)
library(SuperLearner)
library(stringr)


#source('O:/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/code_from_va/SL.wsvm_corrected.R')
#source('O:/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/code_from_va/SL.nnet.1.R')
#source('O:/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/code_from_va/wrapper_SL_xgboost37.R')
#source('O:/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/code_from_va/create.Learner.grid.R')
#source('O:/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/code_from_va/WeightedBrierScore.R')
#source('O:/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/code_from_va/output_ici.R')
#source('O:/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/code_from_va/adj_pred.R')


#setwd('O:/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/data/cvtemps/EM/yr1/')

source('/home/data/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/code_from_va/WeightedBrierScore.R')
source('/home/data/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/code_from_va/output_ici.R')
source('/home/data/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/code_from_va/adj_pred.R')
source('/home/data/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/code_from_va/SL.wsvm_corrected.R')
source('/home/data/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/code_from_va/SL.nnet.1.R')
source('/home/data/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/code_from_va/wrapper_SL_xgboost37.R')
source('/home/data/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/code_from_va/create.Learner.grid.R')

setwd('/home/data/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/data/cvtemps/EM/yr1/')

cvAUCs <- read_sas('em_cvaucs.sas7bdat')

keep_vars <- cvAUCs$Predictor



train_data <- read_sas('_em_forcvauc.sas7bdat')

x_train <- train_data[,colnames(train_data) %in% keep_vars] %>%
  lapply(as.numeric) %>% data.frame()
Y_train <- train_data$unempoutcome
weight_train <- train_data$weight_lsw3_or_lsw4_norm1

X <- x_train
Y <- Y_train
weight <- weight_train

validRows <- lapply(unique(train_data$fold), function(x) which(train_data$fold == x))

cvsl <- readRDS('emcvsl.rds')
cvslcoef <- coef(cvsl)

write.csv(cvslcoef, file =  "emSL-coef_slcvS.csv" )

###############################
# Export pred probs for CV-AUC
###############################
cv_pred  <- as.numeric(cvsl$SL.predict)

out_predcvsl <- data.frame(
  train_data[,c('masterid','stack','fold','weight_lsw3_or_lsw4_norm1','unempoutcome')],
  cv_pred = cv_pred
)

write.csv(out_predcvsl,'emcv_pred_cvsl.csv',row.names=F)


##########################
# Weighted CV Brier score
##########################
fold_weight <- lapply(
  validRows,
  function(i)
    sum(weight[i])
) %>% unlist()


Brier_cvsl <- lapply(
  validRows,
  function(i)
    WeightedBrierScore(x = Y[i], pred = cv_pred[i], weight = weight[i])
) %>% unlist()

CV_Brier_cvsl <- weighted.mean(Brier_cvsl, w = fold_weight)
CV_Brier_cvsl


#########
# CV ICI
#########
ici_cvsl <- lapply(
  validRows,
  function(i)
    output_ici(Y=Y[i], pred=cv_pred[i], pop_weight=weight[i])
)

all_calib_cvsl <- lapply(
  ici_cvsl,
  function(x)
    data.frame(x$calib) %>% t()
) %>% do.call('rbind',.)

cv_ici_cvsl <- apply(
  all_calib_cvsl,
  2,
  weighted.mean,
  w = fold_weight
)
cv_ici_cvsl


#################
# pq calibration
#################
prev <- weighted.mean(Y, weight)

cv_pred_pq <- adj_pred(cv_pred, p_pop = prev, w = weight)

Brier_cv_cvsl_pq <- lapply(
  validRows,
  function(i)
    WeightedBrierScore(x = Y[i], pred = cv_pred_pq[i], weight = weight[i])
) %>% unlist()

CV_Brier_cv_cvsl_pq <- weighted.mean(Brier_cv_cvsl_pq, w = fold_weight)
CV_Brier_cv_cvsl_pq

ici_cv_cvsl_pq <- lapply(
  validRows,
  function(i)
    output_ici(Y=Y[i], pred=cv_pred_pq[i], pop_weight=weight[i])
)

all_calib_cv_cvsl_pq <- lapply(
  ici_cv_cvsl_pq,
  function(x)
    data.frame(x$calib) %>% t()
) %>% do.call('rbind',.)

cv_ici_cv_cvsl_pq <- apply(
  all_calib_cv_cvsl_pq,
  2,
  weighted.mean,
  w = fold_weight
)
cv_ici_cv_cvsl_pq

