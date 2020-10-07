
Orders_Sorted <- treino[order(treino$ds),]
# Sort by date (unless you want to keep the data out of date order for some reason)
Orders_Weekly.ts <- ts(Orders_Sorted$y, frequency = (365.25/7))
# Convert df to time series
Orders_Weekly.hw <- HoltWinters(x=Orders_Weekly.ts, beta = FALSE, gamma = FALSE)
# Run HW
plot(Orders_Weekly.hw) 

previsao_holtwinters <- forecast(object = Orders_Weekly.hw, h = 1)
plot(previsao_holtwinters)

plot.ts(previsao_holtwinters$residuals)            # make a time plot
hist(previsao_holtwinters$residuals)
