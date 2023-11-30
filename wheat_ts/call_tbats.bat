cd C:/Users/zelingher/OneDrive - IIASA/IIASA/price_forecasting/wheat_prices/ts_forecast

# call tbats with for loop (lags in 1:12)
for /l %i in (1,1,12) do start Rscript -e lags=%i -e source('tbats.R')

