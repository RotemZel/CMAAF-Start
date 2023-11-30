REM 1. Create Default Data
cd C:/Users/zelingher/OneDrive - IIASA/IIASA/price_forecasting/wheat_prices/wheat_data/codes
start Rscript -e source('prepare_default.R')

REM 2. Analyse data
cd C:/Users/zelingher/OneDrive - IIASA/IIASA/price_forecasting/wheat_prices/wheat_region/analyse_production
start Rscript -e source('1_prepare_analyse.R')

REM call models with for loop (months in 1:12)
for /l %i in (1,1,12) do start Rscript -e months=%i -e source('tbats.R')

REM Call for TBATS
cd C:/Users/zelingher/OneDrive - IIASA/IIASA/price_forecasting/wheat_prices/wheat_ts

REM call tbats with for loop (lags in 1:12)
for /l %i in (1,1,12) do start Rscript -e lags=%i -e source('tbats.R')