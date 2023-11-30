################################################################################
# General explanation:                                                         #
# This part uses as default:                                                   #
## 1. region production + stocks                                              #
## 2. region yield + stocks                                                   #
## 3. country production + stocks                                           #
## 4. country yield + stocks                                                #
## 5. usa states production + stocks                                           #
## 6. usa states yield + stocks                                                #
## 7. tbats (univariate time series model)                                     #
## discover possible impacts of monthly factors over global wheat price        #
#                                                                              #       
# -----------------------------------------------------------------------------#
# Main r script                                                                #
################################################################################

# !!! Important remarks !!!:
# a) Before running the R code, specify a main working directory ('wd_main') or refer to a project (wheat)
# b) run section 2

wd_general <- "C:/Users/zelingher/OneDrive - IIASA/IIASA/price_forecasting/general_data"
# define a specified main working directory 
wd_main <- "C:/Users/zelingher/OneDrive - IIASA/IIASA/price_forecasting/wheat_prices"
#wd_main <- "C:/Users/zelingher/Documents/temp_rotem"
setwd(wd_main) # set directory

dir.create("wheat_data")
wd_data <- paste0(wd_main,"/wheat_data")

################################################################################
#---------------- 1. create folders and specify directories for region -------- 

# define a specified main working directory 
wd_region <- paste0(wd_main,"/wheat_region")
#wd_main <- "C:/Users/zelingher/Documents/temp_rotem"
setwd(wd_region) # set directory

# create additional folders and working directories 


#------------ I. price analyse and model building 
# I.1 input = production

## mid-processed data, not ready to use
dir.create("analyse_production")
wd_region_analyse_production <- paste0(wd_region,"/analyse_production")
setwd(wd_region_analyse_production)

# I.2 input = yield
setwd(wd_region) # set directory

## mid-processed data, not ready to use
dir.create("analyse_yield")
wd_region_analyse_yield <- paste0(wd_region,"/analyse_yield")
setwd(wd_region_analyse_yield)

# I.3 Summary: processed/analysed data of price analyse
dir.create("analyse_processed")
wd_region_analyse_proc <- paste0(wd_region,"/analyse_processed")

setwd(wd_main)


#------------ II. price forecast and model building 
# II.1 input = production

## mid-processed data, not ready to use
dir.create("forecast_production")
wd_region_forecast_production <- paste0(wd_region,"/forecast_production")
setwd(wd_region_forecast_production)

# II.2 input = yield
setwd(wd_region) # set directory

## mid-processed data, not ready to use
dir.create("forecast_yield")
wd_region_forecast_yield <- paste0(wd_region,"/forecast_yield")
setwd(wd_region_forecast_yield)

# II.3 Summary: processed/analysed data of price forecast
dir.create("forecast_processed")
wd_region_proc <- paste0(wd_region,"/forecast_processed")

setwd(wd_main)



################################################################################
#---------------- 2. create folders and specify directories for country -------- 

# define a specified main working directory 
wd_country <- paste0(wd_main,"/wheat_country")
#wd_main <- "C:/Users/zelingher/Documents/temp_rotem"
setwd(wd_country) # set directory

# create additional folders and working directories 


#------------ I. price analyse and model building 
# I.1 input = production

## mid-processed data, not ready to use
dir.create("analyse_production")
wd_country_analyse_production <- paste0(wd_country,"/analyse_production")
setwd(wd_country_analyse_production)

# I.2 input = yield
setwd(wd_country) # set directory

## mid-processed data, not ready to use
dir.create("analyse_yield")
wd_country_analyse_yield <- paste0(wd_country,"/analyse_yield")
setwd(wd_country_analyse_yield)

# I.3 Summary: processed/analysed data of price analyse
dir.create("analyse_processed")
wd_country_analyse_proc <- paste0(wd_country,"/analyse_processed")

setwd(wd_main)


#------------ II. price forecast and model building 
# II.1 input = production

## mid-processed data, not ready to use
dir.create("forecast_production")
wd_country_forecast_production <- paste0(wd_country,"/forecast_production")
setwd(wd_country_forecast_production)

# II.2 input = yield
setwd(wd_country) # set directory

## mid-processed data, not ready to use
dir.create("forecast_yield")
wd_country_forecast_yield <- paste0(wd_country,"/forecast_yield")
setwd(wd_country_forecast_yield)

# II.3 Summary: processed/analysed data of price forecast
dir.create("forecast_processed")
wd_country_proc <- paste0(wd_country,"/forecast_processed")

setwd(wd_main)

################################################################################
#------------- 3. create folders and specify directories for USA -------------- 

# # define a specified main working directory
# wd_usa <- paste0(wd_main,"/wheat_usa")
# #wd_main <- "C:/Users/zelingher/Documents/temp_rotem"
# setwd(wd_usa) # set directory
# 
# ## input data
# dir.create("data")
# wd_usa_data <- paste0(wd_usa,"/data")
# 
# #------------ I. price forecast and model building 
# # I.1 input = production
# 
# ## mid-processed data, not ready to use
# dir.create("production")
# wd_usa_production <- paste0(wd_usa,"/production")
# setwd(wd_usa_production)
# 
# 
# setwd(wd_main)

################################################################################
#---------- 4. create folders and specify directories to open forecasts -------- 
dir.create("wheat_after_forecast")
wd_open <- paste0(wd_main,"/wheat_after_forecast")

### processed
setwd(wd_open)
dir.create("processed")
wd_proc <- paste0(wd_main,"/wheat_after_forecast/processed")

setwd(wd_main)

################################################################################
#--------- 5. create folders and specify directories for TBATS analysis -------- 

setwd(wd_main) # set directory

## mid-processed data, not ready to use
dir.create("ts_forecast")
wd_ts <- paste0(wd_main, "/wheat_ts")
setwd(wd_ts)

#### processed/analysed data of ts
dir.create("processed")
wd_ts_proc <- paste0(wd_ts,"/processed")

setwd(wd_main)


################################################################################
#------------------------------- 5. call r scripts
################################################################################

# setwd(wd_forecast) # set directory
# list.files()
# source("1_prepare_forecast.R")
# source("2_regression_rf.R")
# source("3_regression_gbm.R")
# source("5_regression_cart.R")
# source("6_regression_sumup.R")
# source("7_regression_formula_builder.R")