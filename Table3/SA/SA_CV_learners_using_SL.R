
options(scipen=999)
library(tidyverse)
library(dplyr)
library(haven)
library(glmnet)
library(SuperLearner)
library(stringr)

#source('O:/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/code_from_va/SL.wsvm_corrected.R')
#source('O:/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/code_from_va/SL.nnet.1.R')
#source('O:/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/code_from_va/wrapper_SL_xgboost37.R')
#source('O:/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/code_from_va/create.Learner.grid.R')
#source('O:/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/code_from_va/WeightedBrierScore.R')
#source('O:/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/code_from_va/output_ici.R')
#source('O:/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/code_from_va/adj_pred.R')


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


full_data <- read_sas('_sa_forcvauc.sas7bdat')

x <- full_data[,colnames(full_data) %in% keep_vars] %>%
  lapply(as.numeric) %>% data.frame()
Y <- full_data$SA_outcomef
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

# LASSO based screener
screen.lasso.sub <- function(Y,
                             X,
                             family,
                             obsWeights,
                             subs=c(1:ncol(X)),
                             alpha = 1,
                             minscreen = 5, 
                             nfolds = 5,
                             nlambda = 100,
                             dfmax = ncol(X) + 1,
                             parallel = FALSE,
                             ...)
{
  SuperLearner:::.SL.require('glmnet')
  
  X_lasso <- lapply(X, as.numeric) %>% do.call('cbind',.)
  
  if(!is.matrix(X_lasso)) {
    X_lasso <- model.matrix(~ -1 + ., X_lasso)
  }
  # X_lasso[,-subs] <- 0  ## Set the variables not in the subset to 0 ##
  
  fitCV <- glmnet::cv.glmnet(x = X_lasso,
                             y = Y,
                             family = family$family,
                             weights = obsWeights,
                             nfolds = nfolds,
                             lambda = NULL,
                             type.measure = 'deviance', 
                             alpha = alpha,
                             nlambda = nlambda,
                             dfmax = dfmax,
                             parallel = parallel,
                             ...)
  
  whichVariable <- (as.numeric(coef(fitCV$glmnet.fit, s = fitCV$lambda.min))[-1] != 0)
  # the [-1] removes the intercept
  if (sum(whichVariable) < minscreen) {
    warning("fewer than minscreen variables passed the glmnet screen, increased lambda to allow minscreen variables")
    sumCoef <- apply(as.matrix(fitCV$glmnet.fit$beta), 2, function(x) sum((x != 0)))
    newCut <- which.max(sumCoef >= minscreen)
    whichVariable <- (as.matrix(fitCV$glmnet.fit$beta)[, newCut] != 0)
  }
  
  return(whichVariable)
}

# Ranger-based screener
screen.ranger.sub <- function (Y, X, family,subs = c(1:ncol(X)), nVar = ncol(X), 
                               ntree = 1000, maxdepth = 8, mtry = 10,
                               splitrule = NULL,
                               obsWeights,  ...)
{
  nested_vars <- grep('^nst_',colnames(X),value=TRUE)
  
  if(length(nested_vars)>0)
    X_ranger <- X[,!colnames(X) %in% nested_vars]
  else
    X_ranger <- X
  # X_ranger[,-subs] <- 0   ## Set the variables not in subset to 0 ##
  
  SuperLearner:::.SL.require('ranger')
  if (family$family == "gaussian") {
    rank.rf.fit <- ranger::ranger(Y ~ ., data = X_ranger,  num.tree = ntree, 
                                  max.depth = maxdepth, splitrule = splitrule,
                                  mtry = ifelse(mtry<ncol(X),mtry,),
                                  importance="impurity_corrected",
                                  case.weights = obsWeights)
  }
  if (family$family == "binomial") {
    rank.rf.fit <- ranger::ranger(as.factor(Y) ~ ., data = X_ranger, num.tree = ntree, 
                                  max.depth = maxdepth, splitrule = splitrule,
                                  mtry = ifelse(mtry<ncol(X),mtry,),
                                  importance="impurity_corrected",
                                  case.weights = obsWeights)
  }
  whichRanger <- as.vector(rank(-rank.rf.fit$variable.importance[1:ncol(X_ranger)]) <= nVar)
  
  whichVariable <- colnames(X) %in% colnames(X_ranger)[whichRanger]
  
  return(whichVariable)
}

# BART-based screener
screen.dbarts.nvar <- function(Y, X, nvar = 5, ntree = 20,
                               obsWeights,
                               nthread = 1,
                               ...)
{
  model = 
    dbarts::bart(x.train = X,
                 y.train = Y,
                 # We need to pass newX in directly due to lack of prediction.
                 x.test = X,
                 ntree = ntree,
                 weights = obsWeights,
                 keeptrees = TRUE,
                 nthread = nthread)
  
  dbarts.var.imp <- embarcadero::varimp(model,plot = FALSE)
  
  whichVariable <- as.vector(rank(-rank(dbarts.var.imp[,2]),) <= nvar)
  
  return(whichVariable)
}

# CV-AUC screener
screen.cvAUC <- function(Y, X, obsWeights, family, id, ...)
{
  nested_vars <- grep('^nst_',colnames(X),value=TRUE)
  
  if(length(nested_vars) > 0)
    X_cvAUC <- X[,!colnames(X) %in% nested_vars]
  else
    X_cvAUC <- X
  
  cv_SLs <- lapply(
    X_cvAUC,
    function(x)
      SuperLearner(
        X = data.frame(x), Y = Y, obsWeights = obsWeights, family = family,
        SL.library = 'SL.glm', id = id,
        cvControl = SuperLearner.CV.control(V=10L))
  )
  cv_preds <- lapply(cv_SLs, function(x) x$Z) %>% do.call('cbind',.) %>% data.frame()
  colnames(cv_preds) <- colnames(X_cvAUC)
  
  fit_cvAUCs <- mapply(
    function(x,sl)
      cvAUC::ci.pooled.cvAUC(x, Y, folds = sl$validRows, ids = id),
    x = cv_preds,
    sl = cv_SLs
  )
  
  cvAUCs <- unlist(t(fit_cvAUCs)[,'cvAUC'])
  cvAUCs_ci <- do.call('rbind',t(fit_cvAUCs)[,'ci'])
  
  whichVariable <- colnames(X) %in% colnames(X_cvAUC)[which(cvAUCs_ci[,1] > 0.5)]
  
  return(whichVariable)
}


#####################
# Two-part screeners
#####################

# LASSO
screen.corr.lasso <- function(Y, X, r=0.8, family, obsWeights, dfmax = ncol(X) + 1, parallel = FALSE, ...)
{
  whichCorr <- screen.corr(Y = Y, X = X, r = r, obsWeights = obsWeights, ...)
  
  .X_small <- X[,whichCorr]
  
  whichLASSO <- screen.lasso.sub(Y = Y, X = .X_small, family = family, obsWeights = obsWeights, dfmax = dfmax, minscreen = 1, parallel = parallel, ...)
  
  whichVariable <- colnames(X) %in% colnames(.X_small[,whichLASSO])
  
  return(whichVariable)
}

# ranger 
screen.corr.ranger <- function(Y, X, r=0.8, family, obsWeights, nVar = 30, num.threads = 1, ...)
{
  whichCorr <- screen.corr(Y = Y, X = X, r = r, obsWeights = obsWeights, ...)
  
  .X_small <- X[,whichCorr]
  
  whichRanger <- screen.ranger.sub(Y = Y, X = .X_small, family = family, obsWeights = obsWeights, nVar = nVar, num.threads = num.threads, ...)
  
  whichVariable <- colnames(X) %in% colnames(.X_small[,whichRanger])
  
  return(whichVariable)
}


#################################
# Create hyperparameter settings
#################################

set.seed(24091111) 

n_real_Y <- length(unique(full_data$masterid[Y == 1]))  # Person-level count 

## xgboost
n_tunes <- 12

tune.grid.xgb <- expand.grid(
  ntrees = 5000L,
  max_depth = 2:5L,
  minobspernode = as.integer(n_real_Y*c(0.05,0.1,0.2)),
  shrinkage = c(0.0001,0.001,0.01,0.1,0.2,0.3),
  gamma = c(0,0.01,0.1,1,2,5),
  subsample = seq(0.5,0.8,0.1),
  colsample_bytree = seq(0.5,1.0,0.1),
  colsample_bynode = seq(0.5,1.0,0.1),
  early_stopping_rounds = 50L
) %>% data.frame()

selected.tune.xgb <- rbind(
  tune.grid.xgb[tune.grid.xgb$max_depth == 2,][sample(sum(tune.grid.xgb$max_depth == 2),as.integer(n_tunes/4)),],
  tune.grid.xgb[tune.grid.xgb$max_depth == 3,][sample(sum(tune.grid.xgb$max_depth == 3),as.integer(n_tunes/4)),],
  tune.grid.xgb[tune.grid.xgb$max_depth == 4,][sample(sum(tune.grid.xgb$max_depth == 4),as.integer(n_tunes/4)),],
  tune.grid.xgb[tune.grid.xgb$max_depth == 5,][sample(sum(tune.grid.xgb$max_depth == 5),as.integer(n_tunes/4)),]
)


## Ranger
n_tunes <- 12

nvar <- 25

tune.grid.ranger1 <- expand.grid(
  num.trees = 5000L,
  max.depth = 2:5L,
  min.node.size = as.integer(n_real_Y*c(0.05,0.1,0.2)),
  mtry = as.integer(c(max(2,sqrt(nvar)/2),sqrt(nvar),min(nvar,sqrt(nvar)*2)))
) %>% data.frame()

selected.tune.ranger1 <- rbind(
  tune.grid.ranger1[tune.grid.ranger1$max.depth == 2,][sample(sum(tune.grid.ranger1$max.depth == 2),as.integer(n_tunes/3/4)),],
  tune.grid.ranger1[tune.grid.ranger1$max.depth == 3,][sample(sum(tune.grid.ranger1$max.depth == 3),as.integer(n_tunes/3/4)),],
  tune.grid.ranger1[tune.grid.ranger1$max.depth == 4,][sample(sum(tune.grid.ranger1$max.depth == 4),as.integer(n_tunes/3/4)),],
  tune.grid.ranger1[tune.grid.ranger1$max.depth == 5,][sample(sum(tune.grid.ranger1$max.depth == 5),as.integer(n_tunes/3/4)),]
)


nvar <- 50

tune.grid.ranger2 <- expand.grid(
  num.trees = 5000L,
  max.depth = 2:5L,
  min.node.size = as.integer(n_real_Y*c(0.05,0.1,0.2)),
  mtry = as.integer(c(max(2,sqrt(nvar)/2),sqrt(nvar),min(nvar,sqrt(nvar)*2)))
) %>% data.frame()

selected.tune.ranger2 <- rbind(
  tune.grid.ranger2[tune.grid.ranger2$max.depth == 2,][sample(sum(tune.grid.ranger2$max.depth == 2),as.integer(n_tunes/3/4)),],
  tune.grid.ranger2[tune.grid.ranger2$max.depth == 3,][sample(sum(tune.grid.ranger2$max.depth == 3),as.integer(n_tunes/3/4)),],
  tune.grid.ranger2[tune.grid.ranger2$max.depth == 4,][sample(sum(tune.grid.ranger2$max.depth == 4),as.integer(n_tunes/3/4)),],
  tune.grid.ranger2[tune.grid.ranger2$max.depth == 5,][sample(sum(tune.grid.ranger2$max.depth == 5),as.integer(n_tunes/3/4)),]
)


nvar <- 100

tune.grid.ranger3 <- expand.grid(
  num.trees = 5000L,
  max.depth = 2:5L,
  min.node.size = as.integer(n_real_Y*c(0.05,0.1,0.2)),
  mtry = as.integer(c(max(2,sqrt(nvar)/2),sqrt(nvar),min(nvar,sqrt(nvar)*2)))
) %>% data.frame()

selected.tune.ranger3 <- rbind(
  tune.grid.ranger3[tune.grid.ranger3$max.depth == 2,][sample(sum(tune.grid.ranger3$max.depth == 2),as.integer(n_tunes/3/4)),],
  tune.grid.ranger3[tune.grid.ranger3$max.depth == 3,][sample(sum(tune.grid.ranger3$max.depth == 3),as.integer(n_tunes/3/4)),],
  tune.grid.ranger3[tune.grid.ranger3$max.depth == 4,][sample(sum(tune.grid.ranger3$max.depth == 4),as.integer(n_tunes/3/4)),],
  tune.grid.ranger3[tune.grid.ranger3$max.depth == 5,][sample(sum(tune.grid.ranger3$max.depth == 5),as.integer(n_tunes/3/4)),]
)


## SVM
## Radial kernel specified later in grid.wsvm

n_tunes <- 6

tune.grid.wsvm <- expand.grid(
  cost = 10^(0:8),
  gamma = 0.1^(0:8)
) %>% data.frame()

selected.tune.wsvm <- tune.grid.wsvm[sample(nrow(tune.grid.wsvm),n_tunes),]


## Tune nnet
## #weights (limited to 1000) = size * (nvars + 1) + (size + 1)
## lasso screeners aren't exact, so add 5 to nvars to be safe

n_tunes <- 6 
buffer <- 10


nvar <- 25 
max_size <- 1
while(max_size * (nvar + 1 + buffer) + (max_size + 1) < 1000)
{
  max_size <- max_size + 1
}

tune.grid.nnet1 <- expand.grid(
  size = as.integer(1:max_size),
  decay = c(0,0.1^(1:8))
) %>% data.frame()

selected.tune.nnet1 <- tune.grid.nnet1[sample(nrow(tune.grid.nnet1),as.integer(n_tunes/3)),]


nvar <- 50
max_size <- 1
while(max_size * (nvar + 1 + buffer) + (max_size + 1) < 1000)
{
  max_size <- max_size + 1
}

tune.grid.nnet2 <- expand.grid(
  size = as.integer(1:max_size),
  decay = c(0,0.1^(1:8))
) %>% data.frame()

selected.tune.nnet2 <- tune.grid.nnet2[sample(nrow(tune.grid.nnet2),as.integer(n_tunes/3)),]


nvar <- 100
max_size <- 1
while(max_size * (nvar + 1 + buffer) + (max_size + 1) < 1000)
{
  max_size <- max_size + 1
}

tune.grid.nnet3 <- expand.grid(
  size = as.integer(1:max_size),
  decay = c(0,0.1^(1:8))
) %>% data.frame()

selected.tune.nnet3 <- tune.grid.nnet3[sample(nrow(tune.grid.nnet3),as.integer(n_tunes/3)),]


## Tune glmnet
tune.grid.glmnet <- data.frame(
  alpha = c(0.1, 0.25, 0.5, 0.75, 0.9)
)


##################
# Create learners
##################
screen.corr.lasso.1 <- function(...) {
  screen.corr.lasso (dfmax = 25,  ...)
}

screen.corr.lasso.2 <- function(...) {
  screen.corr.lasso (dfmax = 50,  ...)
}

screen.corr.lasso.3 <- function(...) {
  screen.corr.lasso (dfmax = 100, ...)
}

screen.corr.ranger.1 <- function(...) {
  screen.corr.ranger (nVar = 25,  ...)
}

screen.corr.ranger.2 <- function(...) {
  screen.corr.ranger (nVar = 50,  ...)
}

screen.corr.ranger.3 <- function(...) {
  screen.corr.ranger (nVar = 100, ...)
}


grid.xgb <- create.Learner.grid("SL.xgboost_cv", detailed_names = TRUE, 
                                tunegrid = selected.tune.xgb, name_prefix = 'SL.xgboost')

grid.ranger1 <- create.Learner.grid("SL.ranger", detailed_names = TRUE, 
                                    tunegrid = selected.tune.ranger1)
grid.ranger2 <- create.Learner.grid("SL.ranger", detailed_names = TRUE, 
                                    tunegrid = selected.tune.ranger2)
grid.ranger3 <- create.Learner.grid("SL.ranger", detailed_names = TRUE, 
                                    tunegrid = selected.tune.ranger3)

grid.wsvm <- create.Learner.grid("SL.wsvm", detailed_names = TRUE, 
                                 params = list(kernel = "radial"),
                                 tunegrid = selected.tune.wsvm)

grid.nnet1 <- create.Learner.grid("SL.nnet.1", detailed_names = TRUE, 
                                  tunegrid = selected.tune.nnet1, name_prefix = 'SL.nnet')
grid.nnet2 <- create.Learner.grid("SL.nnet.1", detailed_names = TRUE, 
                                  tunegrid = selected.tune.nnet2, name_prefix = 'SL.nnet')
grid.nnet3 <- create.Learner.grid("SL.nnet.1", detailed_names = TRUE, 
                                  tunegrid = selected.tune.nnet3, name_prefix = 'SL.nnet')

grid.glmnet <- create.Learner.grid("SL.glmnet", detailed_names = TRUE,
                                   tunegrid = tune.grid.glmnet)


grid.xgb.screen <- lapply(grid.xgb$names,c,"screen.corr.ranger.1","screen.corr.ranger.2","screen.corr.ranger.3")

grid.ranger.screen1 <- lapply(grid.ranger1$names,c,"screen.corr.ranger.1")
grid.ranger.screen2 <- lapply(grid.ranger2$names,c,"screen.corr.ranger.2")
grid.ranger.screen3 <- lapply(grid.ranger3$names,c,"screen.corr.ranger.3")

grid.wsvm.screen <- lapply(grid.wsvm$names,c,"screen.corr.lasso.1","screen.corr.lasso.2","screen.corr.lasso.3")

grid.nnet.screen1 <- lapply(grid.nnet1$names,c,"screen.corr.lasso.1")
grid.nnet.screen2 <- lapply(grid.nnet2$names,c,"screen.corr.lasso.2")
grid.nnet.screen3 <- lapply(grid.nnet3$names,c,"screen.corr.lasso.3")

grid.glmnet.screen <- lapply(grid.glmnet$names, c, "screen.corr.lasso.1","screen.corr.lasso.2","screen.corr.lasso.3")

lrnr.lasso <- create.Learner('SL.glmnet', params = list(alpha=1), name_prefix = 'LASSO')

# Can skip polymars and WSVM because they are slow and often do not get weights
SL.library <- c(list(c(lrnr.lasso$names,'screen.corr')),
                grid.glmnet.screen,
                list(c('SL.polymars','screen.corr.lasso.1','screen.corr.lasso.2','screen.corr.lasso.3')),
                grid.wsvm.screen,
                grid.nnet.screen1,grid.nnet.screen2,grid.nnet.screen3,
                grid.ranger.screen1,grid.ranger.screen2,grid.ranger.screen3,
                grid.xgb.screen)


###################
# Run SuperLearner
###################
control <- SuperLearner.control(saveCVFitLibrary = TRUE)
cvControl <- SuperLearner.CV.control(V = 10L, validRows = validRows)

set.seed(1)
sl <- SuperLearner(Y = Y, X = x, family = binomial(), obsWeights = weight,
                   SL.library = SL.library,
                   control = control, cvControl = cvControl,
                   verbose = TRUE)

saveRDS(sl,'san_cv_learners_using_sl.rds')

#sl <- readRDS('san_cv_learners_using_sl.rds')

###############################
# Export pred probs for CV-AUC
###############################
cv_preds <- sl$Z
colnames(cv_preds) <- paste0('lrnr',1:ncol(cv_preds))

lrnr_xwalk <- data.frame(
  var = colnames(cv_preds),
  lrnr = sl$libraryNames
)

out_pred <- data.frame(
  full_data[,c('masterid','fold','SA_outcomef','weight_lsw3_or_lsw4_norm1')],
  cv_preds
)

write.csv(out_pred,'san_cv_preds.csv',row.names=F)
write.csv(lrnr_xwalk,'san_learner_xwalk.csv',row.names=F)

# Use CV-AUC and the crosswalk to identify best of each algorithm
# Do Brier score and ICI only for the best
# Most straightforward way would be to subset data
 
# We know LASSO is first learner
cv_pred_lasso <- cv_preds[,which(lrnr_xwalk$var == 'lrnr1')] %>% as.numeric()

#   best elastic net  
cv_pred_glmnet <- cv_preds[,which(lrnr_xwalk$var == 'lrnr2')] %>% as.numeric()  

#   best nnet  
cv_pred_nnet <- cv_preds[,which(lrnr_xwalk$var == 'lrnr43')] %>% as.numeric()  

#   best xgboost  
cv_pred_xgboost <- cv_preds[,which(lrnr_xwalk$var == 'lrnr83')] %>% as.numeric()  

#   best random forest  
cv_pred_ranger <- cv_preds[,which(lrnr_xwalk$var == 'lrnr49')] %>% as.numeric()  

#   best wsvm  
cv_pred_wsvm <- cv_preds[,which(lrnr_xwalk$var == 'lrnr29')] %>% as.numeric()  

#   best polymar  
cv_pred_poly <- cv_preds[,which(lrnr_xwalk$var == 'lrnr17')] %>% as.numeric()  


# LASSO

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



# elastic net

##########################
# Weighted CV Brier score
##########################
Brier_glmnet <- lapply(
  validRows,
  function(i)
    WeightedBrierScore(x = Y[i], pred = cv_pred_glmnet[i], weight = weight[i])
) %>% unlist()

CV_Brier_glmnet <- weighted.mean(Brier_glmnet, w = fold_weight)
CV_Brier_glmnet


#########
# CV ICI
#########
ici_glmnet <- lapply(
  validRows,
  function(i)
    output_ici(Y=Y[i], pred=cv_pred_glmnet[i], pop_weight=weight[i])
)

all_calib_glmnet <- lapply(
  ici_glmnet,
  function(x)
    data.frame(x$calib) %>% t()
) %>% do.call('rbind',.)

cv_ici_glmnet <- apply(
  all_calib_glmnet,
  2,
  weighted.mean,
  w = fold_weight
)
cv_ici_glmnet

# Neural network

##########################
# Weighted CV Brier score
##########################
Brier_nnet <- lapply(
  validRows,
  function(i)
    WeightedBrierScore(x = Y[i], pred = cv_pred_nnet[i], weight = weight[i])
) %>% unlist()

CV_Brier_nnet <- weighted.mean(Brier_nnet, w = fold_weight)
CV_Brier_nnet


#########
# CV ICI
#########
ici_nnet <- lapply(
  validRows,
  function(i)
    output_ici(Y=Y[i], pred=cv_pred_nnet[i], pop_weight=weight[i])
)

all_calib_nnet <- lapply(
  ici_nnet,
  function(x)
    data.frame(x$calib) %>% t()
) %>% do.call('rbind',.)

cv_ici_nnet <- apply(
  all_calib_nnet,
  2,
  weighted.mean,
  w = fold_weight
)
cv_ici_nnet


# randomforest

##########################
# Weighted CV Brier score
##########################
Brier_ranger <- lapply(
  validRows,
  function(i)
    WeightedBrierScore(x = Y[i], pred = cv_pred_ranger[i], weight = weight[i])
) %>% unlist()

CV_Brier_ranger <- weighted.mean(Brier_ranger, w = fold_weight)
CV_Brier_ranger


#########
# CV ICI
#########
ici_ranger <- lapply(
  validRows,
  function(i)
    output_ici(Y=Y[i], pred=cv_pred_ranger[i], pop_weight=weight[i])
)

all_calib_ranger <- lapply(
  ici_ranger,
  function(x)
    data.frame(x$calib) %>% t()
) %>% do.call('rbind',.)

cv_ici_ranger <- apply(
  all_calib_ranger,
  2,
  weighted.mean,
  w = fold_weight
)
cv_ici_ranger

# xgboost

##########################
# Weighted CV Brier score
##########################
Brier_xgboost <- lapply(
  validRows,
  function(i)
    WeightedBrierScore(x = Y[i], pred = cv_pred_xgboost[i], weight = weight[i])
) %>% unlist()

CV_Brier_xgboost <- weighted.mean(Brier_xgboost, w = fold_weight)
CV_Brier_xgboost


#########
# CV ICI
#########
ici_xgboost <- lapply(
  validRows,
  function(i)
    output_ici(Y=Y[i], pred=cv_pred_xgboost[i], pop_weight=weight[i])
)

all_calib_xgboost <- lapply(
  ici_xgboost,
  function(x)
    data.frame(x$calib) %>% t()
) %>% do.call('rbind',.)

cv_ici_xgboost <- apply(
  all_calib_xgboost,
  2,
  weighted.mean,
  w = fold_weight
)
cv_ici_xgboost


# wsvm

##########################
# Weighted CV Brier score
##########################
Brier_wsvm <- lapply(
  validRows,
  function(i)
    WeightedBrierScore(x = Y[i], pred = cv_pred_wsvm[i], weight = weight[i])
) %>% unlist()

CV_Brier_wsvm <- weighted.mean(Brier_wsvm, w = fold_weight)
CV_Brier_wsvm


#########
# CV ICI
#########
ici_wsvm <- lapply(
  validRows,
  function(i)
    output_ici(Y=Y[i], pred=cv_pred_wsvm[i], pop_weight=weight[i])
)

all_calib_wsvm <- lapply(
  ici_wsvm,
  function(x)
    data.frame(x$calib) %>% t()
) %>% do.call('rbind',.)

cv_ici_wsvm <- apply(
  all_calib_wsvm,
  2,
  weighted.mean,
  w = fold_weight
)
cv_ici_wsvm


# poly

##########################
# Weighted CV Brier score
##########################
Brier_poly <- lapply(
  validRows,
  function(i)
    WeightedBrierScore(x = Y[i], pred = cv_pred_poly[i], weight = weight[i])
) %>% unlist()

CV_Brier_poly <- weighted.mean(Brier_poly, w = fold_weight)
CV_Brier_poly


#########
# CV ICI
#########
ici_poly <- lapply(
  validRows,
  function(i)
    output_ici(Y=Y[i], pred=cv_pred_poly[i], pop_weight=weight[i])
)

all_calib_poly <- lapply(
  ici_poly,
  function(x)
    data.frame(x$calib) %>% t()
) %>% do.call('rbind',.)

cv_ici_poly <- apply(
  all_calib_poly,
  2,
  weighted.mean,
  w = fold_weight
)
cv_ici_poly