
options(scipen=999)
library(tidyverse)
library(dplyr)
library(haven)
library(glmnet)
library(caret)
library(xgboost)
source('O:/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/Update_202503/Analysis/RESULTS/beeswarms_plots/summary_plot.1.R')
source('O:/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/Update_202503/Analysis/RESULTS/beeswarms_plots/mkWeightedBeeswarmData.R')

setwd('O:/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/Update_202503/Analysis/RESULTS/risk_hlonly/')
indata <- read_sas('O:/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/data/allvars_for_shap.sas7bdat') 


sampleweight <- indata$weight_lsw3_or_lsw4_norm1

xgbFit <- readRDS('xgbFit_hlonly.rds')
shap_data<-read.csv('hlonly_shapdata.csv')
shap_table <- read.csv('hlonly_shap_table.csv')

xgb_shap <- predict(xgbFit$finalModel, as.matrix(shap_data[,xgbFit$finalModel$xNames]), predcontrib = TRUE)

xgb_shap <- xgb_shap[,-ncol(xgb_shap)] # Drop BIAS col
xgb_shap <- xgb_shap[,which(apply(xgb_shap, 2, function(x) all(x != 0)))]  # Keep important vars

x <- indata[,colnames(xgb_shap)] %>% data.frame()
x_mat <- model.matrix(~-1 + ., x)

mean_abs_shap_total <- rowSums(xgb_shap) %>% abs() %>% mean()

setwd('O:/Harvard/Hwang/STARRSLS/AnalyticFiles_NEW/6_LSW4_variables/Separation_analysis_makedata/Update_202503/Analysis/RESULTS/Final_beeswarms/') 

#xgb_shap1<-read.csv('SA_outcome_xgb_shap1.csv')

shap_data1 <- read.csv('hlonly_shapdata1.csv')
#shap_data2 <- read.csv('HL_outcome_shapdata1.csv')
#shap_data3 <- read.csv('saonly_shapdata1.csv')
#shap_data4 <- read.csv('hlonly_shapdata1.csv')
#shap_data5 <- read.csv('sdonly_shapdata1.csv')
#shap_data6 <- read.csv('risk_2plus_shapdata1.csv')


#x <- indata[,colnames(xgb_shap1)] %>% data.frame()
#x_mat1 <- model.matrix(~-1 + ., x)

 bs_vars <- list(names(shap_data1))
 
 
 o1_labels <- c( 
   'Outpx pain dx 12m ',
   '1+ biological child',
   '# negative drug tests 12m',
   'Pain-related txt 12m ',
   'Victim of any crime 12m',
   '# parents born in US',
   'Age first joined Army: 17-21',
   'Lifetime broad mania',
   'Months in last Army episode ',
   'Lifetime panic disorder',
   'Deployment: Bullied/hazed',
   'Army rank, 6 category ',
   'Army rank, 4 category ',
   'Lifetime suicide plan',
   'Lifetime suicide ideation',
   'ACEs: Bruised by family',
   'Bad discharge ',
   'Lifetime PTSD',
   'Married',
   'Engaged' 
 )
 
 

set.seed(1)
bs_data <- lapply(
  bs_vars,
  function(i)
    mkWeightedBeeswarmData(
      observed = x_mat[,i],
      shap =xgb_shap[,i],
      weight = sampleweight,
      exact = FALSE,
      data_points = 10000
    )
)

#then plots without labels to check the order of variables

bs_plots <- lapply(
  bs_data,
  function(x)
    summary_plot.1(
      variable_values = x$observed,
      shap_values = x$shap / mean_abs_shap_total * 100,
      num_vars = 20,
      pt_size = 0.1
    ) +
    labs(title = NULL, x = NULL, y = NULL) +
    scale_x_continuous(labels = NULL,
                       limits = c(min(xgb_shap[,unlist(bs_vars)]) / mean_abs_shap_total * 100,
                                  max(xgb_shap[,unlist(bs_vars)]) / mean_abs_shap_total * 100)) +
    #    scale_y_discrete(labels = NULL) +
    geom_vline(aes(xintercept = 0), color = 'black', size = 0.5) +
    theme(legend.position = 'none',
          axis.line.x = element_blank(),
          axis.ticks = element_blank())
)

bs_axis <- summary_plot.1(
  variable_values = bs_data[[1]]$observed,
  shap_values = bs_data[[1]]$shap / mean_abs_shap_total * 100,
  num_vars = 20,
  colorscale = c('#FFFFFF','#FFFFFF','#FFFFFF'),
  pt_size = 0.1
) +
  labs(title = NULL, x = 'SHAP value (%)', y = NULL) +
  scale_x_continuous(labels = scales::number_format(accuracy = 0.1),
                     limits = c(min(xgb_shap[,unlist(bs_vars)]) / mean_abs_shap_total * 100,
                                max(xgb_shap[,unlist(bs_vars)]) / mean_abs_shap_total * 100)) +
  scale_y_discrete(labels = NULL) +
  geom_vline(aes(xintercept = 0), color = 'black', size = 0.5) +
  theme(axis.ticks.y = element_blank(),
        legend.position = 'none')
bs_plots

#then run with labels
 

bs_plots <- lapply(
  bs_data,
  function(x)
    summary_plot.1(
      variable_values = x$observed,
      shap_values = x$shap / mean_abs_shap_total * 100,
      num_vars = 20,
      pt_size = 0.1
    ) +
    labs(title = NULL, x = 'SHAP value (%) of the Mean', y = NULL) +
    scale_x_continuous(labels = scales::number_format(accuracy = 0.1),
                       limits = c(min(xgb_shap[,unlist(bs_vars)]) / mean_abs_shap_total * 100,
                                  max(xgb_shap[,unlist(bs_vars)]) / mean_abs_shap_total * 100)) +
    scale_y_discrete(labels =  o1_labels) +
    geom_vline(aes(xintercept = 0), color = 'black', size = 0.5) +
    theme(legend.position = 'none' ))

bs_axis <- summary_plot.1(
  variable_values = bs_data[[1]]$observed,
  shap_values = bs_data[[1]]$shap / mean_abs_shap_total * 100,
  num_vars = 20,
  colorscale = c('#FFFFFF','#FFFFFF','#FFFFFF'),
  pt_size = 0.1
) +
  labs(title = NULL, x = 'SHAP value (%) of the Mean', y = NULL) +
  scale_x_continuous(labels = scales::number_format(accuracy = 0.1),
                     limits = c(min(xgb_shap[,unlist(bs_vars)]) / mean_abs_shap_total * 100,
                                max(xgb_shap[,unlist(bs_vars)]) / mean_abs_shap_total * 100)) +
  scale_y_discrete(labels = o1_labels) +
  geom_vline(aes(xintercept = 0), color = 'black', size = 0.5) +
  theme(legend.position = 'none')


bs_plots
