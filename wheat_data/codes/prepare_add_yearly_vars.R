# browseURL("https://www.analyticssteps.com/blogs/data-scraping-r-programming-part-1")

################################################################################
# R Code to download default data:                                             #
# 0. dependent var (obs_price): global wheat price, tested and adapted         #
#    Prices are deflated using agriculture price index, basis 2010             #
# 1. Other monthly variables to explain p:  (annual population, just example)  #
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

# retrieve trade year of biggest producers
load('big_actors.RData')

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

# process geographic based codes
setwd(wd_general)
load("geo_codes.RData")

setwd(paste0(wd_data,"/raw_data"))
# get actual year trade
load("wheat_year.RData")

#--------------------- 1. FAO STAT: Download and filter stack data --------------------------- 
url_ <- 'https://fenixservices.fao.org/faostat/static/bulkdownloads/Population_E_All_Data_(Normalized).zip'
download.file(url_, "Population_E_All_Data_(Normalized).zip")
unzip("Population_E_All_Data_(Normalized).zip")
pop_raw <- read_csv("Population_E_All_Data_(Normalized).csv")
colnames(pop_raw) <- gsub(" ", "", colnames(pop_raw))
colnames(pop_raw) <- tolower(colnames(pop_raw))
head(pop_raw)

item_code = 'Population - Est. & Proj.' # 3010
element_code = 511 # Total Population - Both sexes
element = 'Total Population - Both sexes'
max_year = year(today())

# independent variables: price indices (pi)
# filter to have only needed
xvar_pop <- pop_raw %>%
  # areacode `areacode(m49)` area   itemcode item  elementcode element yearcode  year unit  value
  filter(item == item_code & elementcode == element_code & 
           year >= 1960 & year <= max_year) %>% 
  dplyr::select(areacode, area, item, year, value) %>%
  rename_all(.funs = ~c('fao_code','country', 'feature_head','year','value0')) %>%
  #mutate(.vars = c('AC_code','month','feature_head'), .funs = ~as.numeric())
  # convert countries to CMAF standard
  # left_join() keeps all observations in x
  left_join(y = geo_codes[,c(5,7,8)], by = 'fao_code') %>%
  drop_na() %>% group_by(year, cmaf_country) %>%
  distinct(value0, .keep_all = T) %>% 
  # if doubles: if China, take Max, other country, sum()
  mutate(n = n(),
         value = case_when(n > 1 & cmaf_country != 'China' ~ sum(value0), 
                           .default = value0)) %>% 
  # now only China's value changes. Otherwise it removes duplicates 
  slice_max(value0) %>% ungroup()

xvar_pop <- xvar_pop %>%
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


# Divide into regions/country data
stocks_country <- stocks_wheat %>%
  # no data for specific EU countries - remove them. They can still affect as a region
  filter(cmaf_country %in% trade_country$cmaf_country & cmaf_region != 'EU') %>%
  dplyr::select(-cmaf_region) %>%
  arrange(cmaf_country) %>% 
  # change 0 to small number (prevent Inf)
  mutate(value = case_when(value == 0 ~ 1,
                           value != 0 ~ value)) %>%
  pivot_wider(names_from = cmaf_country, values_from = value) %>%
  arrange(year) %>%
  # calculate relative annual change
  mutate_at(.vars = -1, rel_change)


stocks_regions <- stocks_wheat %>%
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


# -------------------------- 2. Relative Annual change ----------------------------- 
#                          According to relevant trade year                         #
obs_price <- obs_price %>%
  mutate(year = year(date), month = month(date),
         .after = year)

#--------------------- 2.1 Country level ------------------#

trade_country <- trade_country %>%
  filter(cmaf_country %in% colnames(stocks_country)) %>%
  arrange(cmaf_country)

stocks_country <- obs_price %>%
  # add monthly dates to dataset
  full_join(y = stocks_country, by = 'year')

# df = only country's columns
df = stocks_country[,5:ncol(stocks_country)]

for (k in 1:nrow(trade_country)) {
  n_months = as.data.frame(rep(99, trade_country$start[k]-1))
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

stocks_regions <- obs_price %>%
  # add monthly dates to dataset
  full_join(y = stocks_regions, by = 'year')

# df = only country's columns
df = stocks_regions[,5:ncol(stocks_regions)]

for (k in 1:nrow(trade_regions)) {
  n_months = as.data.frame(rep(99, trade_regions$start[k]-1))
  colnames(n_months) = colnames(stocks_regions[k+4])
  k_stocks = as.matrix(stocks_regions[,k+4], ncol = 1) %>% 
    as.data.frame() %>% rename_all(~colnames(n_months))
  k_col = rbind(n_months, k_stocks %>% drop_na())
  
  n_end = as.data.frame(rep(99, nrow(df)-nrow(k_col)))
  colnames(n_end) = colnames(k_col)
  
  k_col = rbind(k_col, n_end)
  
  df[,k] <- k_col
}
colnames(df) = paste0('stock_',colnames(df))
stocks_regions <- cbind(stocks_regions[,1:4],df)


# -------------------------- 3. Organise dataset ----------------------------- #
#                          According to optimal lags                           #

xvars_yearly <- list()



xvars_yearly <- do.call(rbind, xvars_yearly)

# remove completely empty rows
xvars_monthly <- xvars_monthly[rowSums(is.na(xvars_monthly)) != ncol(xvars_monthly),]
xvars_monthly <- xvars_monthly %>%
  dplyr::arrange(date)

# save as data to use
setwd(wd_data)
save(xvars_monthly, file = 'xvars_monthly.RData')
