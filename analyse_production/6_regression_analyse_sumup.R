#02-01-2023 maize price forecast; regions
# forecast was made using production

################################################################################
#--------------------- 0. load the necessities 
################################################################################

setwd(wd_main)
source("master_maize.R")

library(tidyverse)
#--------------------------------------------------------------------------------
# load data ###
#--------------------------------------------------------------------------------
setwd(wd_regions_production_proc)
load("summary_rf_production.RData")
load("summary_gbm_production.RData")
     gbm_production = gbm_production[-c(4:5)]
load("summary_cart_production.RData")
load("summary_lm_production.RData")


###################################################################################
#----- 1. reduce RMSE: 
###################################################################################

rmsep_all <- do.call("rbind", list(rf_production$rmsep, gbm_production$rmsep,
                        lm_production$rmsep, cart_production$rmsep))

write.csv(rmsep_all, file = "rmsep_all.csv", row.names = F)


records_all <- do.call("rbind", list(rf_production$records, gbm_production$records,
                                   lm_production$records, cart_production$records))

write.csv(records_all, file = "records_all.csv", row.names = F)

################################################################################
#------------- 2. reduce Relative importance, based on average
################################################################################

# Pay attention to differences between models:
### RF (%IncMSE) - accuracy increase if x is in the model
### GBM (relative influence) - squared error decreased if x is in the model
### CART (variable importance) - role of variable in reducing the RSS
### LM (relative influence) - |t| = |𝛽i/SE(𝛽i)|

var_rank <- do.call("rbind",
                    list(lm_production$rank_, cart_production$rank_,
                  rf_production$rank_, gbm_production$rank_))

#--------------------------------------------------------------------------------
# 2.4. arrange relative importance output ###
#--------------------------------------------------------------------------------

var_rank <- var_rank %>%  
  dplyr::group_by(var, forecast_model, month) %>%
  dplyr::summarise(contribution = mean(contribution), .groups = "keep", 
                   var = var) %>% # keep these vars in data
         dplyr::distinct_at(.vars = vars(var), .keep_all = T)

write.csv(var_rank, file = "var_rank.csv", row.names = F)