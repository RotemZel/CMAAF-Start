#31-10-2023 wheat price forecast; region, production data

# table of contents:
# 0. general settings
# 1. data import and preparation of summarising environment 
# 5. Summarise & Save

################################################################################
#------------------------------- 0. general settings
################################################################################
setwd("C:/Users/zelingher/OneDrive - IIASA/IIASA/price_forecasting/wheat_prices")
source("master_cmaf_wheat.R")

#### load packages ####\
library(tidyverse)

rm(list=setdiff(ls(), 
                c('wd_main', 'wd_data',
                  'wd_region', 'wd_region_analyse_proc')))

################################################################################
#--------------------- 1. preparation of input ~ output information 
################################################################################
setwd(wd_data)

load('default_region.RData')
load('xvars_monthly.RData')

my_data <- default_region %>%
  # left_join() keeps all observations in x
  left_join(y = xvars_monthly, by = 'date') %>% #drop NA columns, for the analysis stage (II)
  tidyr::drop_na() %>%
  relocate(obs., .after = month)

my_data <- split(my_data, f = my_data$feature_head)

# Now, my_data is a list of I dataframes
for (i in 1:length(my_data))  {
  # Step 1: Select only numeric variables
  my_data[[i]] <- my_data[[i]] %>% 
    dplyr::select(where(is.numeric)) %>%
    # Step 2: Transform the dataframe into a matrix
    as.matrix()
}

setwd(wd_region_analyse_proc)
save(my_data, file = "analyse_data.RData")