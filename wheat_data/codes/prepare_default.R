# browseURL("https://www.analyticssteps.com/blogs/data-scraping-r-programming-part-1")
# unpack()
################################################################################
# R Code to download default data:                                             #
# 1. global price from World Bank, Pink Sheet                                  #
#    Prices are deflated using agriculture price index, basis 2010             #
#    1.1 test p for stationarity with augmented Dickey Fuller (ADF)            #
#     TS is stationary if has no trend and depicts a constant variance over    #
#          time and follows autocorrelation structure over a period constantly.#
#       #### null hypothesis = TS not stationary (pv>5%)                       #
#       #### alternative = TS is stationary (pv<5%)                            #
#    wheat price must be stationary (test ADF). Only stationary is saved       #
# 2. Production/yield data:                                                    #
#   2.1  organise countries/region (K): convert country codes and adapt to    # 
#        current national situations.                                          #
#   2.2  Divide USSR, Yugoslavia (≤1991) between X k's for production/area     #
#        multiply row X times to give each country Y% of production            #
#        Assumption: equal yield
#    Production (1000 Metric Tonnes) ; Area Harvested (1000 Ha)                #
#    !!! NOTE: year = market year, i.e,                                        #
#        Afghanistan 2018 Corn =	JUL 2018 To JUN	2019, 07/2018-06/2019        #
#    !!! Countries: individual EU countries/China                              #
#        e.g., USSR countries  ➔  yield =  Σ(production/area_harvested) 
# 3. merge production + price data to create the default data set to work with #
################################################################################

library(data.table)
library(gdata) ## or library(usethis)
library(httr)
library(readxl)
library(readr)
library(tidyverse)
library(vars) # contains "urca" for ADF test for data with no trend

setwd("C:/Users/zelingher/OneDrive - IIASA/IIASA/price_forecasting/wheat_prices")
source("master_cmaf_wheat.R")

rm(list=setdiff(ls(), 
                c('wd_main', 'wd_data',
                  'wd_general')))

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

# process geographic based codes 
setwd(wd_general)
load("geo_codes.RData")
geo_codes <- geo_codes %>%
  dplyr::select(c(fips_name, fao_code, cmaf_country, cmaf_region, former, ratio_wheat_former)) %>%
  rename(ratio_former = ratio_wheat_former)

setwd(paste0(wd_data,"/raw_data"))
# get local year trade
load("wheat_year.RData")

#--------------------- 1. World Bank: Download and filter global prices --------------------------- 
url_prices <- 'https://thedocs.worldbank.org/en/doc/5d903e848db1d1b83e0ec8f744e55570-0350012021/related/CMO-Historical-Data-Monthly.xlsx'
GET(url_prices, write_disk(tf <- tempfile(fileext = ".xlsx")))

# nominal prices
raw_price <- read_xlsx(tf, sheet = "Monthly Prices", skip = 6)
raw_price <- set_date(df = raw_price, column = 1)

nominal_ac <- raw_price %>%
  dplyr::select(date, SOYBEANS, MAIZE, WHEAT_US_HRW)

# price indices
price_index <- read_xlsx(tf, sheet = "Monthly Indices", skip = 9)
price_index <- set_date(price_index, 1)
price_index <- price_index %>%
  dplyr::select(date, iENERGY, iAGRICULTURE, iFERTILIZERS)

# define dependent variable: wheat price
obs_price <- set_price(df = nominal_ac[,c(1,4)],
                       df_index = price_index[,c(1,3)],
                       year_basis = 2010)

real_p <- as.matrix(obs_price$real_p)
obs. <- as.matrix(obs_price$p)
new_p <- as.data.frame(cbind(real_p,obs.))
colnames(new_p) = c('real_p','obs.')

obs_price <- cbind(obs_price[,1:(ncol(obs_price)-2)], new_p)

# --------------------- 1.1. test for stationarity --------------------------- #
#                using Augmented Dickey Fuller (ADF)                           #
# browseURL("https://atsa-es.github.io/atsa-labs/sec-boxjenkins-aug-dickey-fuller.html")
# browseURL("https://kevinkotze.github.io/ts-6-tut/")
for (i in 1:12){
  print(paste('month',i))
  
  pi = obs_price$obs.[month(obs_price$date) == i]
  pi = pi[!is.na(pi)]
  
  adf_test <- urca::ur.df(pi,
    type = "none", selectlags = c("AIC")) 
  # Value of test-statistic is: (=teststat)
  ## Interpret Critical values for test statistics (cval): 
  ## teststat < tau ➔  TS is stationary
  adf_info <- summary(adf_test)
  print(adf_info@teststat)
  print(adf_info@cval)
  if (adf_info@teststat[1] < adf_info@cval[1]) {
    print(paste0(month.name[i], "'s price is stationary"))
  } else {
    obs_price$obs.[month(obs_price$date) == i] <- NA 
    print(paste0(month.name[i], "'s price is non stationary ➔  drop"))
  }
}

#--------------------- 2. FAO STAT: Download and filter regional production/yield --------------------------- 
item_code = 15 # wheat
element_code = c(5312, 5510, 5419) # 5312	Area Harvested; 5510	Production; 5419	Yield
element = c('area', 'production', 'yield')

url_faocrop <- ("https://fenixservices.fao.org/faostat/static/bulkdownloads/Production_Crops_Livestock_E_All_Data_(Normalized).zip")
#download.file(url_faocrop, "Production_Crops_Livestock_E_All_Data_(Normalized).zip")
#unzip("Production_Crops_Livestock_E_All_Data_(Normalized).zip")
faocrop_raw <- read_csv("Production_Crops_Livestock_E_All_Data_(Normalized).csv")
colnames(faocrop_raw) <- gsub(" ", "", colnames(faocrop_raw))
head(faocrop_raw)

# filter to have only wheat
crop_wheat <- faocrop_raw %>%
  dplyr::select(1,3,4,6,7,10,12) %>%
  rename_all(.funs = ~c('fao_code','country','AC_code','AC','feature_head','year','value')) %>%
  filter(AC_code == item_code & feature_head %in% element_code) %>% # wheat
  # convert countries to CMAF standard
  # keep obs. from x that have matching in y
  inner_join(y = geo_codes[,2:4], by = 'fao_code') %>%
  mutate(feature_head = case_when(feature_head == 5419 ~ 'yield',
                                  feature_head == 5312 ~ 'area',
                                  feature_head == 5510 ~ 'production'))

crop_wheat <- crop_wheat[,5:ncol(crop_wheat)]

# set_production by ex countries (e.g., FSU12)
ex_countries <- geo_codes %>%
  filter(ratio_former > 0) %>%
  dplyr::select(cmaf_country,former,ratio_former) %>%
  rename_all(.funs = ~c('country','cmaf_country','ratio_former')) %>%
  left_join(y = crop_wheat, by = 'cmaf_country') %>%
  # for each country: [production, area] = %/former_country; yield = former_country
  mutate(value = case_when(feature_head == 'yield' ~ value, # this is yield
                           feature_head %in% c('area', 'production') ~ value*ratio_former)) %>% 
  dplyr::select(-c(cmaf_country, ratio_former)) %>%
  rename(cmaf_country = country) %>%
  relocate(cmaf_country, .after = 4)

ex_coun_title <- unique(geo_codes$former[geo_codes$ratio_former>0])  

# merge with given FAO data
crop_wheat <- crop_wheat %>%
  filter(! (cmaf_country %in% ex_coun_title)) %>%
  rbind(ex_countries) %>%
  distinct(feature_head, cmaf_country, year, value, .keep_all = T)

# set_production by united countries (e.g., Serbia and Montenegro)
un_countries <- crop_wheat %>%
  group_by(year, feature_head, cmaf_country) %>%
  filter(n() > 1 & feature_head != 'yield') %>%
  # in case of united countries, aggregate production/ area
  mutate(value = sum(value)) %>% 
  distinct(feature_head, cmaf_country, year, value, .keep_all = T) %>%
  pivot_wider(names_from = feature_head, values_from = value) %>%
  mutate(yield = production/area, 
         note = 99) %>%
  pivot_longer(cols = any_of(element), names_to = 'feature_head', values_to = 'value') %>%
  ungroup() %>% dplyr::select(feature_head, year, value, cmaf_country, cmaf_region, note)

un_coun_title <- un_countries %>%
  distinct(year, cmaf_country, .keep_all = F)
  
# merge with given FAO data
crop_wheat <- crop_wheat %>%
  mutate(note = 0) %>%
  rbind(un_countries) %>%
  filter(n() == 1 | note == 99, 
         .by = c(feature_head, cmaf_country, year)) %>% dplyr::select(-note)

# detect biggest producer by region (to set trade year)
top_production_region <- crop_wheat %>%
  filter(feature_head == 'production') %>% 
  group_by(cmaf_region, cmaf_country) %>%
  summarise(production = sum(value)) %>%
  slice_max(order_by = production, n = 1) %>%
  arrange(cmaf_region)

# detect 20 biggest producer countries
top_production_country <- crop_wheat %>%
  filter(feature_head == 'production') %>% #production
  group_by(cmaf_country) %>%
  summarise(production = sum(value)) %>%
  slice_max(order_by = production, n = 20) %>%
  arrange(cmaf_country)

# Divide into region/country data
default_country <- crop_wheat %>%
  dplyr::select(-cmaf_region) %>%
  filter(cmaf_country %in% top_production_country$cmaf_country) %>%
  arrange(cmaf_country) %>%
  group_by(feature_head) %>% filter(feature_head != 'area') %>%
  pivot_wider(names_from = cmaf_country, values_from = value) %>% ungroup()


default_region <- crop_wheat %>%
  dplyr::select(-cmaf_country) %>%
  filter(feature_head != 'yield') %>%
  # aggregate production/ area
  group_by(year, feature_head, cmaf_region) %>%
  mutate(value = sum(value)) %>% 
  distinct(feature_head, cmaf_region, year, value, .keep_all = T) %>%
  pivot_wider(names_from = feature_head, values_from = value) %>%
  mutate(yield = production/area) %>%
  pivot_longer(cols = any_of(element), names_to = 'feature_head', values_to = 'value') %>%
  ungroup() %>%
  arrange(cmaf_region) %>%
  group_by(feature_head) %>% filter(feature_head != 'area') %>%
  pivot_wider(names_from = cmaf_region, values_from = value) %>% ungroup()


# Create annual relative Production/Yield change datasets
dates <- obs_price$date
years <- year(dates)

dates <- as.data.frame(cbind(years, dates))
dates$dates <- as.Date(dates[,2])
colnames(dates) = c('year', 'date')

# trade year of relevant entities
trade_country = trade_year[,-2] %>%
  filter(cmaf_country %in% top_production_country$cmaf_country) %>%
  arrange(cmaf_country)

trade_region = trade_year %>%
  filter(cmaf_country %in% top_production_region$cmaf_country) %>%
  arrange(cmaf_region) %>% dplyr::select(-cmaf_country)

default_country <- split(default_country, f = default_country$feature_head)
default_region <- split(default_region, f = default_region$feature_head)

for (i in 1:2){ #production/yield
  default_country[[i]] <- default_country[[i]] %>%
    mutate_at(.vars = -c(1:2), rel_change) %>%
    # add monthly dates to dataset
    full_join(y = dates, by = 'year') %>%
    relocate(date, .before = 1) %>%
    mutate(month = month(date), .after = 1) %>%
    filter(year >= 1962)
  
  df = default_country[[i]][,1:4]
  
  for (k in 1:nrow(trade_country)) {
    n_months = as.data.frame(rep(99, trade_country[k,2]-1))
    colnames(n_months) = colnames(default_country[[i]][,k+4])
    k_col = rbind(n_months, default_country[[i]][,k+4] %>% drop_na())
    
    n_end = as.data.frame(rep(99, nrow(df)-nrow(k_col)))
    colnames(n_end) = colnames(k_col)
    
    k_col = rbind(k_col, n_end)
      
    df <- cbind(df, k = k_col)
    
    default_country[[i+2]] <- df
  }
  default_country[[i+2]]$feature_head <- default_country[[i+2]]$feature_head[1]
  
  
  default_region[[i]] <- default_region[[i]] %>%
    mutate_at(.vars = -c(1:2), rel_change) %>%
    # add monthly dates to dataset
    full_join(y = dates, by = 'year') %>%
    relocate(date, .before = 1) %>%
    mutate(month = month(date), .after = 1) %>%
    filter(year >= 1962)
  
  df = default_region[[i]][,1:4]
  
  for (k in 1:nrow(trade_region)) {
    n_months = as.data.frame(rep(99, trade_region[k,2]-1))
    colnames(n_months) = colnames(default_region[[i]][,k+4])
    k_col = rbind(n_months, default_region[[i]][,k+4] %>% drop_na())
    
    n_end = as.data.frame(rep(99, nrow(df)-nrow(k_col)))
    colnames(n_end) = colnames(k_col)
    
    k_col = rbind(k_col, n_end)
    
    df <- cbind(df, k = k_col)
    
    default_region[[i+2]] <- df
    
    default_region[[i+2]]$feature_head <- default_region[[i+2]]$feature_head[1]
  }
}

default_country <- rbind(default_country[[3]], default_country[[4]])
default_region <- rbind(default_region[[3]], default_region[[4]])


#--------------------- 3. USDA PSD: Download and filter stack data --------------------------- 
# process geographic based codes
setwd(wd_general)
load("geo_codes.RData")
geo_codes <- geo_codes %>%
  dplyr::select(c(fips_code, fao_code, cmaf_country, cmaf_region, former, ratio_wheat_former_stocks)) %>%
  rename(ratio_former = ratio_wheat_former_stocks)


setwd(paste0(wd_data,"/raw_data"))

url_usdapsd <- 'https://apps.fas.usda.gov/psdonline/downloads/psd_grains_pulses_csv.zip'
#download.file(url_usdapsd, "psd_grains_pulses_csv.zip")
#unzip("psd_grains_pulses_csv.zip")
usdapsd_raw <- read_csv("psd_grains_pulses.csv")
#colnames(usdapsd_raw) <- gsub(" ", "", colnames(usdapsd_raw))
colnames(usdapsd_raw) <- tolower(colnames(usdapsd_raw))
head(usdapsd_raw)

item_code = 'Wheat' # 0410000
element_code = 020 # Beginning Stocks
element = 'Beginning Stocks'

# independent variables: price indices (pi)
# filter to have only wheat
stocks_wheat <- usdapsd_raw %>%
  dplyr::select(commodity_description, country_code, country_name, market_year, attribute_description, value) %>%
  rename_all(.funs = ~c('AC','fips_code','country','year','feature_head','value')) %>%
  #mutate(.vars = c('AC_code','month','feature_head'), .funs = ~as.numeric())
  filter(AC == item_code & feature_head == element) %>% # wheat
  # convert countries to CMAF standard
  # left_join() keeps all observations in x
  left_join(y = geo_codes[,c(1,3,4)], by = 'fips_code') %>%
  drop_na()

stocks_wheat <- stocks_wheat %>%
  dplyr::select(year, cmaf_country, cmaf_region, value)


# set_stocks by ex countries (e.g., FSU12)
ex_countries <- geo_codes %>%
  filter(ratio_former > 0) %>%
  dplyr::select(cmaf_country,former,ratio_former) %>%
  rename_all(.funs = ~c('country','cmaf_country','ratio_former')) %>%
  left_join(y = stocks_wheat, by = 'cmaf_country') %>%
  # for each country: %/former_country; 
  mutate(value = value*ratio_former) %>% 
  dplyr::select(-c(cmaf_country, ratio_former)) %>%
  rename(cmaf_country = country) %>%
  relocate(cmaf_country, .after = 4)

ex_coun_title <- unique(geo_codes$former[geo_codes$ratio_former>0])

# merge with given PSD data
stocks_wheat <- stocks_wheat %>%
  filter(! (cmaf_country %in% ex_coun_title)) %>%
  rbind(ex_countries) %>%
  distinct(cmaf_country, year, value, .keep_all = T)


# set_stocks by united countries (e.g., Serbia and Montenegro)
un_countries <- stocks_wheat %>%
  group_by(year, cmaf_country) %>%
  filter(n() > 1) %>%
  # in case of united countries, aggregate X
  mutate(value = sum(value)) %>% 
  distinct(cmaf_country, year, value, .keep_all = T) %>%
  mutate(note = 99) %>%
  ungroup() %>% 
  dplyr::select(year, value, cmaf_country, cmaf_region, note)

un_coun_title <- un_countries %>%
  distinct(year, cmaf_country, .keep_all = F)

# merge with given PSD data
stocks_wheat <- stocks_wheat %>%
  mutate(note = 0) %>%
  rbind(un_countries) %>%
  filter(n() == 1 | note == 99, 
         .by = c(cmaf_country, year),
         year > 1960) %>% dplyr::select(-note)


# detect biggest producer by region (to set trade year)
top_stocks_region <- stocks_wheat %>%
  group_by(cmaf_region, cmaf_country) %>%
  summarise(stocks = sum(value)) %>%
  slice_max(order_by = stocks, n = 1) %>%
  arrange(cmaf_region)

# detect 20 biggest producer countries
top_stocks_country <- stocks_wheat %>%
  group_by(cmaf_country) %>%
  summarise(stocks = sum(value)) %>%
  slice_max(order_by = stocks, n = 20) %>%
  arrange(cmaf_country)

# trade year of relevant entities
trade_stocks_country = trade_year[,-2] %>%
  filter(cmaf_country %in% top_stocks_country$cmaf_country) %>%
  arrange(cmaf_country)

trade_stocks_region = trade_year %>%
  filter(cmaf_country %in% top_stocks_region$cmaf_country) %>%
  arrange(cmaf_region) %>% dplyr::select(-cmaf_country)


# Divide into region/country data
stocks_country <- stocks_wheat %>%
  # no data for specific EU countries - remove them. They can still affect as a region
  filter(cmaf_country %in% trade_stocks_country$cmaf_country & cmaf_region != 'EU') %>%
  dplyr::select(-cmaf_region) %>%
  arrange(cmaf_country) %>% 
  # change 0 to small number (prevent Inf)
  mutate(value = case_when(value == 0 ~ 1,
                           value != 0 ~ value)) %>%
  pivot_wider(names_from = cmaf_country, values_from = value) %>%
  arrange(year) %>%
  # calculate relative annual change
  mutate_at(.vars = -1, rel_change)


stocks_region <- stocks_wheat %>%
  dplyr::select(-cmaf_country) %>%
  # aggregate stocks
  group_by(year, cmaf_region) %>%
  mutate(value = sum(value)) %>% 
  distinct(cmaf_region, year, value, .keep_all = T) %>%
  ungroup() %>%
  arrange(cmaf_region) %>%
  # change 0 to small number (prevent Inf)
  mutate(value = case_when(value == 0 ~ 1,
                           value != 0 ~ value)) %>%
  pivot_wider(names_from = cmaf_region, values_from = value) %>%
  arrange(year) %>%
  # calculate relative annual change
  mutate_at(.vars = -1, rel_change)


# -------------------------- 3.2. Relative Annual change ----------------------------- 
#                          According to relevant trade year                         #
obs_new <- obs_price[,c(1,5)] %>%
  mutate(year = year(date), month = month(date),
         .after = year)

#--------------------- 2.1 Country level ------------------#

stocks_country <- obs_new %>%
  # add monthly dates to dataset
  full_join(y = stocks_country, by = 'year')

# df = only country's columns
df = stocks_country[,5:ncol(stocks_country)]

for (k in 1:nrow(trade_stocks_country)) {
  n_months = as.data.frame(rep(99, trade_stocks_country$start[k]-1))
  colnames(n_months) = colnames(stocks_country[k+4])
  k_stocks = as.matrix(stocks_country[,k+4], ncol = 1) %>% 
    as.data.frame() %>% rename_all(~colnames(n_months))
  k_col = rbind(n_months, k_stocks %>% drop_na())
  
  n_end = as.data.frame(rep(99, nrow(df)-nrow(k_col)))
  colnames(n_end) = colnames(k_col)
  
  k_col = rbind(k_col, n_end)
  
  df[,k] <- k_col
}
colnames(df) = paste0('stock_',colnames(df))
stocks_country <- cbind(stocks_country[,1:4],df)

#--------------------- 2.2 Region level ------------------#

stocks_region <- obs_new %>%
  # add monthly dates to dataset
  full_join(y = stocks_region, by = 'year')

# df = only country's columns
df = stocks_region[,5:ncol(stocks_region)]

for (k in 1:nrow(trade_stocks_region)) {
  n_months = as.data.frame(rep(99, trade_stocks_region$start[k]-1))
  colnames(n_months) = colnames(stocks_region[k+4])
  k_stocks = as.matrix(stocks_region[,k+4], ncol = 1) %>% 
    as.data.frame() %>% rename_all(~colnames(n_months))
  k_col = rbind(n_months, k_stocks %>% drop_na())
  
  n_end = as.data.frame(rep(99, nrow(df)-nrow(k_col)))
  colnames(n_end) = colnames(k_col)
  
  k_col = rbind(k_col, n_end)
  
  df[,k] <- k_col
}
colnames(df) = paste0('stock_',colnames(df))
stocks_region <- cbind(stocks_region[,1:4],df)

#----------------- 3. merge production + price data to create the default dataset ----------------- 

# replace 99 values with NA
change99 <- function(x) ifelse(x > 90, NA, x)

# ----- 3.1 country
n_inputs <- length(unique(default_country$feature_head))

default_country <- obs_new %>%
  # right_join() keeps all observations in y (-[month,feature_head,year])
  right_join(y = default_country, by = c('date','year','month')) %>%
  left_join(y = stocks_country, by = c('date','year','month','obs.'))

## Remove columns with NA for the whole 1st year (heads*12months)
default_country[,6:ncol(default_country)] <- 
  default_country[,6:ncol(default_country)] %>%
  select_if(~sum(.[1:(12*n_inputs)], na.rm = T) > 90)

default_country[,6:ncol(default_country)] <- 
  # replace 99 values with NA
  apply(X = default_country[,6:ncol(default_country)],
        MARGIN = c(1,2),
        FUN = change99)


# ----- 3.2 region
n_inputs <- length(unique(default_region$feature_head))

default_region <- obs_new %>%
  # right_join() keeps all observations in y (-[month,feature_head,year])
  right_join(y = default_region, by = c('date','year','month')) %>%
  left_join(y = stocks_region, by = c('date','year','month','obs.'))

## Remove columns with NA for the whole 1st year (heads*12months)
default_region[,6:ncol(default_region)] <- 
  default_region[,6:ncol(default_region)] %>%
  select_if(~sum(.[1:(12*n_inputs)], na.rm = T) > 90)

default_region[,6:ncol(default_region)] <- 
  # replace 99 values with NA
  apply(X = default_region[,6:ncol(default_region)],
        MARGIN = c(1,2),
        FUN = change99)


big_actors <- list('production_country' = trade_country,
                      'production_region' = trade_region,
                   'stocks_country' = trade_stocks_country,
                   'stocks_region' = trade_stocks_region)

# save as data to use
setwd(wd_data)

save(obs_price, file = "obs_price.RData") # observed prices (to show)
save(default_country, file = 'default_country.RData')
save(default_region, file = 'default_region.RData')
save(big_actors, file = 'big_actors.RData')