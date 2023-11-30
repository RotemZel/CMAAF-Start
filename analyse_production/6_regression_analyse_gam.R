#02-09-2023 wheat price forecast; regions, production data
#browseURL("https://www.r-bloggers.com/2017/07/generalized-additive-models/")
#browseURL('https://www.mainard.co.uk/post/why-mgcv-is-awesome/#generalized-additive-models-gams')


# table of contents:
# 0. general settings
# 1. data import and preparation of summarising environment 
# 2. test distribution of p: normal/not normal
# 3. run gam() regression with multiple versions, save as seperated models:
### formula: 
##### a. linear regression (default) → method = 'glm.fit'
##### b. nonlinear (smooth) interactions between p~X → if norm.dist: method = [glm.fit', 'GCV.Cp']  
##### c. nonlinear (tensor) two-dimensional interactions between p~X. 
# 3. Summarise & Save

#### load packages ####
#library(broom) # Summarizes key information about models
require(mgcv) # load the GAMs library
library(caret)  # For relative importance
library(tidyverse)

setwd("C:/Users/zelingher/OneDrive - IIASA/IIASA/price_forecasting/wheat_prices")
source("master_cmaf_wheat.R")

setwd(wd_region_analyse_proc)
rm(list=setdiff(ls(), 
                c('wd_main', 'wd_region_analyse_proc',
                  'pmonth')))

print(paste('month =', month.name[pmonth]))
pmonth=5
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
mod_name = "gam"

#geographic scale
geo = "region"

# model input (default)
def_input = "production"

# m
m = pmonth

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

# GAM allows different model structures, including linear and nonlinear components. 
# create a regression formula of Price ~ x's
reg_form_group <- list(
  # linear relationship (default)
  linear =  formula( #P(t) = f{P(t-1), x(t)}
  paste(colnames(df)[3],paste(colnames(df)[-(1:3)],
                              collapse = '+'), sep = '~')),
  # Using s() for a smooth term:
  smooth = formula( 
    paste(colnames(df)[3], '~ s(', paste(colnames(df)[4:15], 
    collapse = ') + s('), ')'))
  # Using te() for a tensor product smooth term:
  # tensor = formula( 
  #   paste(colnames(df)[3], '~ te(', paste(colnames(df)[4:15],
  #                                         collapse = ', '), ')'))
  )

method_setter <- function(form){ # defines the reg_form
  
  # Shapiro-Wilk test for normality:
  # null hypothesis = normally distribution (p-value > 0.05)
  obs. = df[,3]
  dist = shapiro.test(obs.)
  
  if (reg_form == as.formula(reg_form_group$linear)) {
    method = 'glm.fit'
  } else if (reg_form != as.formula(reg_form_group$linear) & dist$p.value > 0.05) {
    method = 'GCV.Cp'
  } else (print('Error'))
  
  return(method)
}

#-------------------------------------------------------------------------------------------------
# 1.3. create main environments to save results on ###
#-------------------------------------------------------------------------------------------------
rolls <- numeric(length(reg_form_group)*5*(n_obs))

# create a matrix to record all obs.~pred. prices
records <- matrix(data = rolls, ncol = 5)
colnames(records) = c("obs.", "pred.", "error", "year_test", 
                      "extra") # extra = glm.fit/GCV.Cp

# importance ranking:
### MODEL #  var | inc_mse | f.model | tree_seq | month | LO_year | depth
rank_ <- matrix(NA, ncol = 10, nrow = 0) %>% as.data.frame() 
colnames(rank_) = c("var", "contribution", "analyse_model",  
                    "month", "year_test", "geo_scale", "d_input", 
                    "tree_seq", "depth_mtry", "extra")

# matrix to save parameters (distinguish between models)
# run | depth_mtry | tree_seq | extra (formula, method)
reg_methods <- c('glm.fit', 'GCV.Cp')
situation <- cbind(run = 1:length(reg_form_group), formula = names(reg_form_group))
situation <- as.matrix(situation)

################################################################################
#----------------------------- 2. Run the model --------------------------------
################################################################################

set.seed(4)
for (run in 1:nrow(situation)) 
  {
  reg_form = reg_form_group[run]
  method = method_setter(situation[run,'formula'])
  print(reg_form)
  
  for (i in 1:n_obs) 
  {
    print(paste0('analyse for ', 
                 df[i,'year'], '/', df[n_obs,'year']))
    
    training <- df[-i,-(1:2)] # -c(date)
    testing <- matrix(df[i,-(1:2)], nrow = 1)
    
    colnames(testing) = colnames(training)
    
    observed <- testing[1,1] # observed price in year i
    year_test <- df[i,'year'] # year to analyse

    # gam: Generalized additive models ###
    #-------------------------------------------------------------------------------------------------
    temp_ <- gam(reg_form, # s(),te(),ti()
                    family = gaussian,
                    method = method,
                    #select =
                      #control = gam_control,
                      data = as.data.frame(training),
                   discrete = T)

    # predict price of year i
    pred_ <- predict.gam(temp_,
                         newdata = as.data.frame(testing),
                         type = 'response')
    
    records[i+n_obs*(run-1),] <- c(observed, pred_,
                                   error(observed, pred_),
                                   year_test,
                                   run)

    # relative importance in training years
    # calculate relative importance scaled to 100
    x = varImp(temp_)
    #x = sort(x$Overall, decreasing = T) %>% as.data.frame()
    colnames(x) = "contribute"

    x = x %>% tibble::rownames_to_column() %>%
      dplyr::rename(var = rowname) %>%
      dplyr::mutate(forecast_model = mod_name,
                    month = m,
                    year_i = year_test,
                    g_scale = geo, d_input = def_input,
                    tree_seq = NA, depth_mtry = NA,
                    extra = reg_methods[run])

    rank_ <- rbind(rank_, x)
  }
}

records <- records %>%
  as.data.frame() %>%
  mutate(extra = reg_methods[extra]) %>%
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