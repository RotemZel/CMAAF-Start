#02-09-2023 wheat price forecast; regions, production data
url("http://www.sthda.com/english/articles/38-regression-model-validation/158-regression-model-accuracy-metrics-r-square-aic-bic-cp-and-more/")

# table of contents:
# 0. general settings
# 1. data import and preparation of summarising environment 
# 2. run linear regression with the Akaike’s Information Criteria:
### AIC search for appropriate subsets (removes non-contributing variables) 
### direction = control the search method (default = "backward")
### Forward stepwise: start with null, add best predictor one by one.
### Backward stepwise: start with full model, remove worst predictor one by one.
#-- lower AIC = better model
# ! stepwise AIC aims to min. AIC, not to min the forecasting errors.
### AIC is a version of AIC corrected for small sample sizes. 
# 3. Summarise & Save

#### load packages ####
library(broom) #summarizes key information about models
library(caret) # for relative importance
library(tidyverse)

setwd("C:/Users/zelingher/OneDrive - IIASA/IIASA/price_forecasting/wheat_prices")
source("master_cmaf_wheat.R")

setwd(wd_region_analyse_proc)
rm(list=setdiff(ls(), 
                c('wd_main', 'wd_region_analyse_proc',
                  'pmonth')))

print(paste('month =', month.name[pmonth]))

################################################################################
#--------------------- 1. preparation of input ~ output information 
################################################################################
aic_opt <- function(lm_mod){
  aic_mod <- broom::glance(lm_mod)$AIC
  return((aic_mod))
}

error <- function(obs, pred){
  abs(obs - pred)
}

#define rmse function
rmse <- function(predicted,observed){ 
  sqrt(mean(
    (as.numeric(predicted) - as.numeric(observed)
    )^2))
}

#-------------------------------------------------------------------------------------------------
# 1.1. define main characters to loop on ###
#-------------------------------------------------------------------------------------------------  

### General parameters:
# analyseing model
mod_name = "lm"

#geographic scale
geo = "region"

# model input (default)
def_input = "production"

# m
m = pmonth

# to select a formula-based model by AIC.
# aic_d
direction = c("both", "backward", "forward")

#-------------------------------------------------------------------------------------------------
# 1.2. define main characters to loop on ###
#-------------------------------------------------------------------------------------------------  
load("analyse_data.RData")
colnames(my_data[[def_input]])

df <- my_data[[def_input]][my_data[[def_input]][,'month']==pmonth,]

variables <- colnames(df[,-(1:3)])
variables

# learning pmonth
n_obs <- as.numeric(nrow(df))

#-------------------------------------------------------------------------------------------------
# 1.3. create main environments to save results on ###
#-------------------------------------------------------------------------------------------------
rolls <- numeric(5*(n_obs))

# create a matrix to record all obs.~pred. prices
records <- matrix(data = rolls, ncol = 5)
colnames(records) = c("obs.", "pred.", "error", "year_test", 
                      "extra")

# importance ranking:
### MODEL #  var | inc_mse | f.model | tree_seq | month | LO_year | depth
rank_ <- matrix(NA, ncol = 10, nrow = 0) %>% as.data.frame() 
colnames(rank_) = c("var", "contribution", "analyse_model",  
                    "month", "year_test", "geo_scale", "d_input", 
                    "tree_seq", "depth_mtry", "extra")

################################################################################
#----------------------------- Run the model   ---------------------------------
################################################################################

# create a regression formula of Price ~ x's
reg_form <- formula( #P(t) = f{P(t-1), x(t)}
  paste(colnames(df)[3],paste(colnames(df)[-(1:3)],collapse='+'),
        sep='~'))

print(reg_form)

set.seed(4)
for (i in 1:n_obs) 
{
  print(paste0('analyse for ', 
               df[i,'year'], '/', df[n_obs,'year']))
  
  training <- df[-i,-(1:2)] # -c(date)
  testing <- matrix(df[i,-(1:2)], nrow = 1)
  
  colnames(testing) = colnames(training)
  
  observed <- testing[1,1] # observed price in year i
  year_test <- df[i,'year'] # year to analyse
  
  situation <- cbind(run = 1:length(direction), direction)
  situation <- as.matrix(situation)
  
  lm_temp_ <- lm(reg_form, 
                 data = as.data.frame(training))
  
  #---------------- temperate results saved here -------------------------

  MOD <- list() # save xgboost models here
  
  #-------------------------------------------------------------------------------------------------
  # traditional linear model, Akaike information criterion (AIC)###
  #-------------------------------------------------------------------------------------------------  
  for (run in 1:nrow(situation))
  {
    print(paste(year_test, 'direction =', run))
    aic_d = situation[run,'direction']
    #chooses only most important vars (AIC)
    temp_ <- step(lm_temp_, direction = aic_d,
                  trace = 0) #remove trace to get information
    
    MOD[[run]] <- temp_
  }
  
  situation <- cbind(run = 1:length(direction), 
                     aic = sapply(X = MOD, FUN = aic_opt))
  
  situation <- situation[which.min(situation[,'aic']),]
  
  temp_ <- MOD[[situation['run']]]
  
  # predict price of year i
  pred_ <- predict.lm(temp_, 
                      newdata = as.data.frame(testing),
                      type = "response")
  
  records[i,] <- c(observed, pred_,
                   error(observed, pred_),
                   year_test, 
                   situation['aic'])
  
  # variable importance in training years
  x = varImp(temp_)
  x = sort(x$Overall, decreasing = T) %>% as.data.frame()
  colnames(x) = "contribute"
  
  x = x %>% tibble::rownames_to_column() %>% 
    dplyr::rename(var = rowname) %>%
    dplyr::mutate(forecast_model = mod_name,  
                  month = m, 
                  year_i = year_test, 
                  g_scale = geo, d_input = def_input,
                  tree_seq = NA, depth_mtry = NA,
                  extra = situation['aic'])
  
  rank_ <- rbind(rank_, x)
}

records <- records %>%
  as.data.frame() %>%
  mutate(forecast_model = mod_name, month = m, .before = year_test) %>%
  mutate(g_scale = geo, d_input = def_input, 
         tree_seq = NA, depth_mtry = NA, .after = year_test)

################################################################################
#-------------- 4. Summarise & Save
################################################################################

titles <- c(paste0('rank_',mod_name,'_',m,'.RData'),
            paste0('records_',mod_name,'_',m,'.RData'))

files <- list(rank_, records)

setwd(wd_region_analyse_proc)
save(rank_, file = titles[1])
save(records, file = titles[2])