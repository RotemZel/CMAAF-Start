# browseURL("https://www.analyticssteps.com/blogs/data-scraping-r-programming-part-1")

################################################################################
# R Code to download default data:                                             #
# 0. dependent var (obs_price): global wheat price, tested and adapted         #
#    Prices are deflated using agriculture price index, basis 2010             #
# 1. Other monthly variables to explain p:                                     #
#   1.1  price indices: transform into relative annual change (RAC)            #
#   1.2  price of other AC that may impact: deflate and transform to RAC       #
# 2. VAR Estimation: testing for optimal lag length 1 year lag limitation      #
### 2.1. estimation of VAR on complete data and storing of results
### 2.2. choose the most relevant Price~x lag for each x
# 3. Modify data according to VAR results #
################################################################################

library(data.table)
library(gdata) ## or library(usethis)
library(httr)
library(readxl)
library(readr)
library(tidyverse)

setwd("C:/Users/zelingher/OneDrive - IIASA/IIASA/price_forecasting/wheat_prices")
source("master_cmaf_wheat.R")

rm(list=setdiff(ls(), 
                c('wd_main', 'wd_data', 
                  'wd_general')))

# upload dependent variable
setwd(wd_data)
load("obs_price.RData")
obs_price <- obs_price %>% 
  dplyr::select(date, obs.) %>% drop_na()

xvars <- list()

# ---------------- set functions 
# define date class
set_date <- function(df, column){
  colnames(df)[column] = 'date'
  df$date = sub(x = df$date, pattern = "M", replacement = "-01-")
  df$date = as.Date(df$date, "%Y-%d-%m")
  return(df)
}

# calculate relative change
rel_change <- function(x){
  rel_x <- (x-lag(x))/lag(x)
  return(rel_x)
}

# process raw prices to fit analysis
set_price <- function(df, df_index, year_basis){
  library(lubridate)
  
  base_index <- df_index %>%
    filter(year(date) == 2010)
  
  new_df <- df %>%
    inner_join(y = df_index, by = 'date')
  
  new_df <- split(new_df, f = month(new_df$date))
  base_index <- split(base_index, f = month(base_index$date))
  
  for(i in 1:12){
    new_df[[i]]$real_p = as.numeric(base_index[[i]][,2])*(new_df[[i]][,2]/new_df[[i]][,3])
    # relative change
    new_df[[i]]$p = rel_change(new_df[[i]]$real_p)
  }
  
  new_df <- do.call(rbind, new_df)
  new_df <- arrange(new_df, date)
  
  return(new_df)
}

# # process geographic based codes 
# setwd(wd_general)
# load("geo_codes.RData")
# geo_codes <- geo_codes %>%
#   dplyr::select(c(fips_name, fao_code, cmaf_country, cmaf_region, former, ratio_wheat_former)) %>%
#   rename(ratio_former = ratio_wheat_former)
# 
# 
# setwd(paste0(wd_data,"/raw_data"))
# # get actual year trade
# load("wheat_year.RData")

#--------------------- 1. World Bank: Download and filter global prices --------------------------- 
url_prices <- 'https://thedocs.worldbank.org/en/doc/5d903e848db1d1b83e0ec8f744e55570-0350012021/related/CMO-Historical-Data-Monthly.xlsx'
GET(url_prices, write_disk(tf <- tempfile(fileext = ".xlsx")))

# nominal prices
raw_price <- read_xlsx(tf, sheet = "Monthly Prices", skip = 6)
raw_price <- set_date(df = raw_price, column = 1)

nominal_ac <- raw_price %>%
  dplyr::select(date, SOYBEANS, MAIZE)

# price indices
price_index <- read_xlsx(tf, sheet = "Monthly Indices", skip = 9)
price_index <- set_date(price_index, 1)
price_index <- price_index %>%
  dplyr::select(date, iENERGY, iAGRICULTURE, iFERTILIZERS)

# independent variables: price indices (pi)
xvars_pi <- split(price_index, f = month(price_index$date))
for (i in 1:12) {
  xvars_pi[[i]] <- xvars_pi[[i]] %>%
    mutate_at(.vars = -1, rel_change)
}
xvars_pi <- arrange(do.call(rbind, xvars_pi), date)
xvars[[length(xvars)+1]] <- as.data.frame(xvars_pi)

# independent variables: price of other agricultural commodities (pac)
xvars_pac = list()
for (i in 2:3) {
  pac <- set_price(df = nominal_ac[,c(1,i)],
                   df_index = price_index[,c(1,3)],
                   year_basis = 2010)
  pdate <- set_date(df = pac, column = 1)
  p <- as.data.frame(as.matrix(pac$p))
  
  xvars_pac[[i-1]] <- cbind(pdate[,1], p)
}
# combine data frames stored in a list
xvars_pac <- arrange(Reduce(full_join,xvars_pac), date)
xvars_pac$date = xvars_pi$date

xvars[[length(xvars)+1]] <- xvars_pac

# get all variables into one dataset (start with xvars)
xvars <- do.call(left_join, xvars)
nvars = ncol(xvars)-1
#--------------------- 2. VAR Estimation: testing for optimal lag length <= 1 year --------------------------- 
# browseURL("http://eclr.humanities.manchester.ac.uk/index.php/R_TS_VAR")

# VAR is useful to predict multiple TS variables using one model
# VAR = multivariate time series model (t= 1,2,...,T) 
# VAR relates Y_T with past obs (Y_t) and past obs of other vars (X_t).
# Here, we investigate Y (obs.) as a function of X ➔  Reduced form VAR
# Assume obs. explained by X and not otherwise (checked next) ➔  No Recursive VAR 
# No restrictions ➔  No Structural VAR (⧣ SVAR)

# testing for optimal lag length (d*) based on obs.
# using the AIC (aim to min. AIC)
# cases <- numeric(4*11*5)
# AIC <- matrix(cases, ncol = 4)
AIC <- matrix(NA, ncol = 4, nrow = 0) %>% as.data.frame()
colnames(AIC) = c("month", "feature", "lag", "AIC")

for (i in 1:12) {
  x = 1
  
  while (x <= ncol(xvars)-2) {
    feature = colnames(xvars[2+x])
    print(paste0("feature = ", feature))
    
    for(d in 1:11){
      # choose 1 X at a time and convert to TS
      test_d <- xvars[ ,c(1:2, 2+x)] %>%
        dplyr::arrange(date) %>%
        # set feature d months backward, to get lagged effects
        dplyr::rename_with(~'feature', .cols = 3) %>%
        dplyr::mutate(feature = lag(feature, n = d)) %>%
        dplyr::mutate(month = month(date), year = year(date), .before = 1, .keep = 'unused') %>%
        drop_na()
      
      test_d <- test_d %>%
        # leave only prices of month i in the data
        dplyr::filter(month == i) %>%
        ts(start = test_d$year[1], frequency = 12)
      
      # Lag selection (with no constant, no trend)
      lag_d <- vars::VARselect(y = test_d, lag.max = d, type = "none")
      AIC[nrow(AIC)+1,] <- c(i, feature, d, lag_d$criteria[1])
    }
    x = x+1
  }
}

# leave only optimal lag for each feature
AIC <- AIC %>%
  dplyr::filter(AIC != "-Inf") %>%
  dplyr::mutate(AIC = as.numeric(AIC),
                month = as.numeric(month)) %>%
  dplyr::group_by(month, feature) %>%
  dplyr::slice_min(AIC, n = 1) %>% ungroup()


# -------------------------- 3. Organise dataset ----------------------------- #
#                          According to optimal lags                           #

xvars_monthly <- list()

for (i in 1:12) {
  #create (complete) data to modify
  df_temp <- xvars
  
  x = 1
  while (x <= ncol(xvars)-2) {
    
    feature = colnames(df_temp[2+x])
    print(paste0("feature = ", feature))
    
    # get relevant lag
    lag_x <- AIC %>%
      dplyr::filter(month == i & 
                      feature == colnames(df_temp[2+x])) %>%
      dplyr::select(c(3)) %>% as.numeric()
    
    df_temp <- df_temp %>%
      # set feature d months backward, to get lagged effects
      dplyr::rename_with(~'feature', .cols = 2+x) %>%
      dplyr::mutate(feature = lag(feature, n = lag_x))
    
    colnames(df_temp) <- colnames(xvars)
    
    x = x+1
  }
  
  df_temp <- df_temp %>%
    dplyr::filter(month(date) == i) %>%
    dplyr::filter(!if_all(c(3:ncol(df_temp)), is.na))
  
  cases = nrow(df_temp)
  
  if(cases > 44) {
    xvars_monthly[[i]] <- df_temp
  } else {
    xvars_monthly[[i]] <- NA
    print("drop it")
  }     
}

xvars_monthly <- do.call(rbind, xvars_monthly)

# remove completely empty rows
xvars_monthly <- xvars_monthly[rowSums(is.na(xvars_monthly)) != ncol(xvars_monthly),]
xvars_monthly <- xvars_monthly %>%
  dplyr::arrange(date)

# save as data to use
setwd(wd_data)
save(xvars_monthly, file = 'xvars_monthly.RData')