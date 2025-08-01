
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
 
#setwd('O:/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/data/cvtemps/EM/')

source('/home/data/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/code_from_va/WeightedBrierScore.R')
source('/home/data/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/code_from_va/output_ici.R')
source('/home/data/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/code_from_va/adj_pred.R')
source('/home/data/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/code_from_va/SL.wsvm_corrected.R')
source('/home/data/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/code_from_va/SL.nnet.1.R')
source('/home/data/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/code_from_va/wrapper_SL_xgboost37.R')
source('/home/data/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/code_from_va/create.Learner.grid.R')

setwd('/home/data/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/data/cvtemps/EM/')

cvAUCs <- read_sas('em_cvaucs.sas7bdat')

keep_vars <- cvAUCs$Predictor

  
train_data <- read_sas('_em_forcvauc.sas7bdat')

x_train <- train_data[,colnames(train_data) %in% keep_vars] %>%
  lapply(as.numeric) %>% data.frame()
Y_train <- train_data$unempoutcome
weight_train <- train_data$weight_lsw3_or_lsw4_norm1
validRows <- lapply(unique(train_data$fold), function(x) which(train_data$fold == x))


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

n_real_Y <- length(unique(train_data$masterid[Y_train == 1]))  # Person-level count 


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


# Can skip polymars and WSVM because they are slow and often do not get weights
SL.library <- c(list('SL.mean'),
                grid.glmnet.screen,
                list(c('SL.polymars','screen.corr.lasso.1','screen.corr.lasso.2','screen.corr.lasso.3')),
                grid.wsvm.screen,
                grid.nnet.screen1,grid.nnet.screen2,grid.nnet.screen3,
                grid.ranger.screen1,grid.ranger.screen2,grid.ranger.screen3,
                grid.xgb.screen)


###################
# Run SuperLearner
###################
cvControl <- SuperLearner.CV.control(V = 10L, validRows = validRows)
innerCvControl <- list(SuperLearner.CV.control(V = 10L))

options(mc.cores = 10)

set.seed(24091112, "L'Ecuyer-CMRG")
cvsl <- CV.SuperLearner(Y = Y_train, X = x_train, family = binomial(), obsWeights = weight_train,
                        SL.library = SL.library, cvControl = cvControl, innerCvControl = innerCvControl,
                        verbose = TRUE, parallel = 'multicore')

saveRDS(cvsl,'emcvsl.rds')
 