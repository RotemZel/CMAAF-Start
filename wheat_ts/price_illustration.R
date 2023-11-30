# Prices for proposes of illustration
## This file contains only creation of tables and figures, using raw or processed data


# table of contents:
# 0. general settings
# 1. data import and preparation of summarising environment 
# 2. first run - ML models (RF, GBM): versions for t * c * j 
# 5. Summarise & Save

################################################################################
#------------------------------- 0. general settings
################################################################################

#### load packages ####
library(forecast)
library(tidyverse)
#library(TSstudio) # For TS Analysis and Forecasting


# upload price indices
price_index <- readxl:: read_excel("G:/My Drive/IIASA/price_forecasting/maize_prices/data/raw_data/global_price.xlsx", 
               sheet = "Indices_change")[ ,c(1,6,8)]

price_index <- price_index %>% tidyr::drop_na() %>%
      dplyr::rename(Date = `...1`) 


real_price_index <- readxl:: read_excel("G:/My Drive/IIASA/price_forecasting/maize_prices/data/raw_data/global_price.xlsx", 
                                   sheet = "price_indices")[ ,c(1,6,8)]

#### load data and define WD
setwd(wd_data)
raw_price <- readxl::read_excel("GlobalPrice.xlsx")[,-c(1:3,5,9)] # datasets to use

setwd(wd_ts_proc)

################################################################################
#--------------------- 1. Price data preparation 
################################################################################

price_df <- raw_price %>%
     dplyr::rename("Maize Price" = "deflated, 2010",
                   "Price Change" = Price) %>%
                  dplyr::filter(`Maize Price` > 0)

viz_index <- price_df %>%
  ggplot(aes(x = Date)) +
  geom_hline(yintercept = 100, alpha = 0.4, size = 0.7) + 
  geom_line(aes(y = `Price index, cereals`), colour = "#ff8000", size = 0.9) + #orange
  labs(y = "Grains price index",
       title = "Grains price index, in USD (2010 = 100)") 

viz_price <- price_df %>%
  ggplot(aes(x = Date)) +
  geom_line(aes(y = `Maize Price`), size = 0.9, colour = "#009933") + # green
  labs(y = "Price",
       title = "Maize price in CME market (USD per tonne)") 

viz_change <- price_df %>% drop_na() %>%
  ggplot(aes(x = Date)) +
  geom_hline(yintercept = 0, alpha = 0.4) +
  #geom_hline(yintercept = -20, colour = scales::alpha("red", alpha=0.4), size = 1) +
  #geom_hline(yintercept = 20, colour = scales::alpha("red", alpha=0.4), size = 1) +
  # for presentation size = 1.2#change scale ; blue
  geom_line(aes(y = `Price Change`*100), colour = "#0086b3", size = 0.9) +
  labs(y = "Price (% change / year)",
       title = "Annual price change, maize in CME (%)") 

setwd(wd_ts_figures)
viz_top <- cowplot::plot_grid(viz_change, viz_index, ncol = 2)  
cowplot::plot_grid(viz_top, viz_price, ncol = 1)

# note that in the considered time there were several big global price shocks


# transform dataset into time series: {y,x1,x2,...,xn}
# y = Price, x = ts[,-Price]
start_ts <- lubridate::year(price_df$Date)[1]

price_ts <- price_df %>%
  dplyr::select(-Date) %>% 
  ts(start = c(start_ts), frequency = 12)

# inspect data (zoom in plot)
plot(price_ts)
