# 31-08-2023 TBATS forecasting
# TBATS is an univariate TS model. 
# Predictions are done using different steps-ahead forecasting (lag)
# Records matrix is different than in other models

library(tidyverse)
library(forecast)

####DATA IMPORT & PREPERATION#####
setwd("C:/Users/zelingher/OneDrive - IIASA/IIASA/price_forecasting/wheat_prices")
source("master_cmaf_wheat.R")
setwd(wd_data)

rm(list=setdiff(ls(), 
                c('wd_main', 'wd_data',
                  'wd_ts', 'wd_ts_proc',
                  'lags')))

load('obs_price.RData')
my_data = obs_price %>%
  dplyr::select(date, obs.) %>%
  drop_na() %>%
  mutate(month = month(date),
         year = year(date), 
         .before = 1, .keep = 'unused') %>%
  as.matrix()

# f
f = "tbats"

# define for the ts()
year_1 <- min(my_data[,'year'])
begin <- 43 + lags

# Define core functions:
standard_deviation <- function(x) {
  n <- length(x)
  if(n <= 1) return(NA)
  sd <- sqrt(sum((x - mean(x))^2) / (n - 1))
  return(sd)
}

rmse <- function(predicted,obs.){ #define rmse function
  sqrt(mean(
    (as.numeric(predicted) - as.numeric(obs.)
    )^2))
}

# TBATS results matrix
records <- matrix(numeric(7*nrow(my_data)), ncol = 7)

################################################################################
####################### 2. TIME SERIES FORECASTING #############################
################################################################################

ts_prices <- ts(my_data[,'obs.'], start = c(year_1), frequency = 12)

l = lags

set.seed(4)
for (i in (begin+l):nrow(my_data))
{
  print(paste(i,"out of", nrow(my_data), 'for month =', month.name[my_data[i,'month']]))
  # create a time series class train set
  ### --- first option is for actual forecast
  train <- ts(my_data[1:(i - l),'obs.'],
              # 12 observations per year
              start = c(year_1), frequency = 12)
  observed <- my_data[i,'obs.']
  month <- my_data[i,'month']
  year_oob <- my_data[i,'year']
  
  temp_ <- tbats(y = train, seasonal.periods = c(12)) # 12 months in a year
  
  # predict price of month t + lag
  # perform l steps ahead prediction; last one is the actual forecast
  pred_ <- forecast::forecast(temp_, h = l, #h = lags ahead to predict
                    level = 95, model = tbats) 
  
  record_ <- c(observed, 
               as.numeric(pred_$mean[l]), 
               as.numeric(pred_$lower[l]), 
               as.numeric(pred_$upper[l]), month, year_oob, 
               l) # extra note 
  
  records[i-(begin+l)+1,] <- record_
}

records <- records %>%
  as.data.frame() %>%
  rename_all(.funs = ~c('obs.', 'pred.', 'lower', 'upper',
                        'month', 'year', 'extra')) %>%
  filter(extra > 0) %>%
  mutate(g_scale=NA, d_input=NA, tree_seq=NA, depth_mtry=NA, .before =extra) %>%
  mutate(forecast_model = f)

# calculate overall error
# rmse = function(predicted,obs.)
# rmse matrix # rmse | sd.price | lag | month
rmsep <- records %>%
  group_by(month) %>%
  dplyr::mutate(sd.price = standard_deviation(obs.),
                rmse = rmse(pred., obs.)) %>%
  dplyr::distinct(rmse, sd.price, month, extra) %>%
  dplyr::mutate(rmse = rmse, sd.price = sd.price, 
                forecast_model = f,
                month = month, 
                g_scale = NA, d_input = NA,
                RA = 1-(rmse/sd.price))

################################################################################
#-------------- 3. Summarise & Save
################################################################################
setwd(wd_ts_proc)

titles <- c(paste0('rmse_tbats_',l,'.RData'),
            paste0('records_tbats_',l,'.RData'))

files <- list(rmsep, records)

save(rmsep, file = titles[1])
save(records, file = titles[2])