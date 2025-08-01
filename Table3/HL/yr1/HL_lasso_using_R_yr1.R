
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


#setwd('O:/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/data/cvtemps/HL/yr1/')

source('/home/data/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/code_from_va/WeightedBrierScore.R')
source('/home/data/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/code_from_va/output_ici.R')
source('/home/data/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/code_from_va/adj_pred.R')
source('/home/data/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/code_from_va/SL.wsvm_corrected.R')
source('/home/data/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/code_from_va/SL.nnet.1.R')
source('/home/data/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/code_from_va/wrapper_SL_xgboost37.R')
source('/home/data/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/code_from_va/create.Learner.grid.R')

setwd('/home/data/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/data/cvtemps/HL/yr1/')

cvAUCs <- read_sas('hl_cvaucs.sas7bdat')

keep_vars <- cvAUCs$Predictor


full_data <- read_sas('_hl_forcvauc.sas7bdat')


x <- full_data[,colnames(full_data) %in% keep_vars] %>%
  lapply(as.numeric) %>% data.frame()
Y <- full_data$hl_final
weight <- full_data$weight_lsw3_or_lsw4_norm1
validRows <- lapply(unique(full_data$fold), function(x) which(full_data$fold == x))


###################
# Create screeners
###################

# Correlation screener to remove high correlations
screen.corr <- function(Y,
                        X,
                        obsWeights,
                        r = 0.8,
                        ...)
{
  corr <- cov.wt(X, wt = obsWeights, cor = T)$cor
  
  if(any(corr[upper.tri(corr, diag = FALSE)] >= r))
  {
    high_corr <- caret::findCorrelation(corr, cutoff = r)
    
    whichVariable <- colnames(X) %in% colnames(X[,-high_corr])
  }
  else
    whichVariable <- rep(TRUE, ncol(X))
  
  return(whichVariable)
}


##################
# Create learners
##################

lrnr.lasso <- create.Learner('SL.glmnet', params = list(alpha=1), name_prefix = 'LASSO')

SL.library <- list(c(lrnr.lasso$names, 'screen.corr'))

length(validRows)
###################
# Run SuperLearner
###################
cvControl <- SuperLearner.CV.control(V = 10L, validRows = validRows)

options(mc.cores = 10)

set.seed(2501062, "L'Ecuyer-CMRG")
sl <- mcSuperLearner(Y = Y, X = x, family = binomial(), obsWeights = weight,
                     SL.library = SL.library, cvControl = cvControl,
                     verbose = TRUE)

saveRDS(sl,'all_lasso_sl.rds')


###############################
# Export pred probs for CV-AUC
###############################
cv_pred_lasso <- as.numeric(sl$Z[,1])

out_pred <- data.frame(
  full_data[,c('masterid','stack','fold','weight_lsw3_or_lsw4_norm1','hl_final')],
  cv_pred_lasso = cv_pred_lasso
)

write.csv(out_pred,'cv_pred_lasso.csv',row.names=F)


##########################
# Weighted CV Brier score
##########################
fold_weight <- lapply(
  validRows,
  function(i)
    sum(weight[i])
) %>% unlist()


Brier_lasso <- lapply(
  validRows,
  function(i)
    WeightedBrierScore(x = Y[i], pred = cv_pred_lasso[i], weight = weight[i])
) %>% unlist()

CV_Brier_lasso <- weighted.mean(Brier_lasso, w = fold_weight)
CV_Brier_lasso


#########
# CV ICI
#########
ici_lasso <- lapply(
  validRows,
  function(i)
    output_ici(Y=Y[i], pred=cv_pred_lasso[i], pop_weight=weight[i])
)

all_calib_lasso <- lapply(
  ici_lasso,
  function(x)
    data.frame(x$calib) %>% t()
) %>% do.call('rbind',.)

cv_ici_lasso <- apply(
  all_calib_lasso,
  2,
  weighted.mean,
  w = fold_weight
)
cv_ici_lasso


#################
# pq calibration
#################
prev <- weighted.mean(Y, weight)

cv_pred_lasso_pq <- adj_pred(cv_pred_lasso, p_pop = prev, w = weight)

Brier_lasso_pq <- lapply(
  validRows,
  function(i)
    WeightedBrierScore(x = Y[i], pred = cv_pred_lasso_pq[i], weight = weight[i])
) %>% unlist()

CV_Brier_lasso_pq <- weighted.mean(Brier_lasso_pq, w = fold_weight)
CV_Brier_lasso_pq

ici_lasso_pq <- lapply(
  validRows,
  function(i)
    output_ici(Y=Y[i], pred=cv_pred_lasso_pq[i], pop_weight=weight[i])
)

all_calib_lasso_pq <- lapply(
  ici_lasso_pq,
  function(x)
    data.frame(x$calib) %>% t()
) %>% do.call('rbind',.)

cv_ici_lasso_pq <- apply(
  all_calib_lasso_pq,
  2,
  weighted.mean,
  w = fold_weight
)
cv_ici_lasso_pq

 