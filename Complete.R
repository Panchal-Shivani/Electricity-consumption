## USE FORECAST LIBRARY.
install.packages("forecast")
library(forecast)
library(zoo)
library(ggplot2)


## CREATE DATA FRAME. 
data <- read.csv("Book1.csv")

head(data)
tail(data)


# Check the length of the consumption data
length(data$Consumption)

elec.ts <- ts(data$Consumption, 
                   start = c(2006, 1), end = c(2017, 52), freq = 52)

#DATA PARTITION
nValid <- 208
nTrain <- length(elec.ts) - nValid
train.ts <- window(elec.ts, start = c(2006, 1), end = c(2006, nTrain))
valid.ts <- window(elec.ts, start = c(2006, nTrain + 1), 
                   end = c(2006, nTrain + nValid))

train.ts
valid.ts


#PLOT OF ENTIRE DATASET
# Define start and end years
first.year <- 2006
last.year  <- 2017

# Y‐axis limits covering the full series
yl <- range(elec.ts)

# Plot the full observed series
plot(elec.ts,
     xlab   = "Year",
     ylab   = "Weekly Electricity Consumption",
     main   = "Observed Weekly Electricity Consumption (2006–2017)",
     xlim   = c(first.year, last.year + 1),
     ylim   = yl,
     xaxt   = "n",
     bty    = "l",
     col    = "black",
     lwd    = 2)

# Custom yearly x‐axis
axis(1, at = seq(first.year, last.year + 1, 1),
     labels = seq(first.year, last.year + 1, 1))

# Add a legend
legend("bottomright",
       legend = "Observed",
       col    = "black",
       lty    = 1,
       lwd    = 2,
       bty    = "n")






##Time Series Components
elec.stl <- stl(elec.ts, s.window = "periodic")
autoplot(elec.stl, main = "Electricity Consumption Time Series Components")


# Checking PREDICTABILITY
# Use Acf() function to identify autocorrelation and plot autocorrelation
# for different lags.
autocor <- Acf(elec.ts, lag.max = 12, 
               main = "Autocorrelation for Electricity Consumption")

# Use Arima() function to fit AR(1) model for S&P500 close prices.
# The ARIMA model of order = c(1,0,0) gives an AR(1) model.
close.price.ar1<- Arima(elec.ts, order = c(1,0,0))
summary(close.price.ar1)


# Apply z-test to test the null hypothesis that beta 
# coefficient of AR(1) is equal to 1.
ar1 <- 0.8247
s.e. <- 0.0227
null_mean <- 1
alpha <- 0.05
z.stat <- (ar1-null_mean)/s.e.
z.stat
p.value <- pnorm(z.stat)
p.value
if (p.value<alpha) {
  "Reject null hypothesis"
} else {
  "Accept null hypothesis"
}


#MODEL 1 (Holt Winter's model)

## HOLT-WINTER'S (HW) EXPONENTIAL SMOOTHING WITH PARTITIONED DATA, AUTOMATIC
## ERROR, TREND and SEASONALITY (ZZZ) OPTIONS, AND OPTIMAL PARAMETERS
## ALPHA, BETA, AND GAMMA.

# Create Holt-Winter's (HW) exponential smoothing for partitioned data.
# Use ets() function with model = "ZZZ", i.e., automatic selection of
# error, trend, and seasonality options.
# Use optimal alpha, beta, & gamma to fit HW over the training period.
hw.ZZZ <- ets(train.ts, model = "ZZZ")
hw.ZZZ 

# Use forecast() function to make predictions using this HW model with 
# validation period (nValid). 
# Show predictions in tabular format.
hw.ZZZ.pred <- forecast(hw.ZZZ, h = nValid, level = 0)
hw.ZZZ.pred


#ACCURACY
round(accuracy(hw.ZZZ.pred$mean, valid.ts), 3)


#Holt Winter's model on entire dataset

## FORECAST WITH HOLT-WINTER'S MODEL USING ENTIRE DATA SET INTO
## THE FUTURE 

# Create Holt-Winter's (HW) exponential smoothing for full Amtrak data set. 
# Use ets() function with model = "ZZZ", to identify the best HW option
# and optimal alpha, beta, & gamma to fit HW for the entire data period.
HW.ZZZ.full<- ets(elec.ts, model = "ZZZ")
HW.ZZZ.full

# Use forecast() function to make predictions using this HW model for
# 52 week into the future.
HW.ZZZ.pred.full <- forecast(HW.ZZZ.full, h = 52 , level = 0)
HW.ZZZ.pred.full

#ACCURACY
round(accuracy(HW.ZZZ.pred.full$fitted, elec.ts), 3)

###Applying trailing MA

# Identify and display regression residuals for entire data set.
tot.trend.seas.res <- HW.ZZZ.full$residuals
tot.trend.seas.res

# Use trailing MA to forecast residuals for entire data set.
tot.ma.trail.res <- rollmean(tot.trend.seas.res, k = 4, align = "right")
tot.ma.trail.res

# Create forecast for trailing MA residuals for future 52 periods.
tot.ma.trail.res.pred <- forecast(tot.ma.trail.res, h = 52, level = 0)
tot.ma.trail.res.pred

# Develop 2-level forecast for future 12 periods by combining 
# regression forecast and trailing MA for residuals for future
# 12 periods.
tot.fst.2level <- HW.ZZZ.pred.full$mean + tot.ma.trail.res.pred$mean
tot.fst.2level

# Create a table with regression forecast, trailing MA for residuals,
# and total forecast for future 12 periods.
future52.df <- round(data.frame(HW.ZZZ.pred.full$mean, tot.ma.trail.res.pred$mean, 
                                tot.fst.2level), 3)
names(future52.df) <- c("Holt Winter's.Fst", "MA.Residuals.Fst", "Combined.Fst")
future52.df


# Checking accuracy
# Use accuracy() function to identify common accuracy measures.
# Use round() function to round accuracy measures to three decimal digits.
round(accuracy(HW.ZZZ.pred.full$fitted, valid.ts), 3)
round(accuracy(HW.ZZZ.pred.full$fitted+tot.ma.trail.res, elec.ts), 3)
round(accuracy((naive(elec.ts))$fitted, elec.ts), 3)
round(accuracy((snaive(elec.ts))$fitted, elec.ts), 3)

#Visualization of Holt Winter's Model

#visualization of Holt witer model with trailing MA
#trying visualization
# 1. turn your three forecast vectors into ts objects starting the week after the end of elec.ts
freq <- frequency(elec.ts)
end_year  <- end(elec.ts)[1]
end_week  <- end(elec.ts)[2] + 1
# roll over if past last week
if(end_week > freq) { 
  end_year  <- end_year + 1
  end_week  <- 1
}

hw_ts      <- ts(HW.ZZZ.pred.full$mean,      start=c(end_year, end_week), frequency=freq)
ma_ts      <- ts(tot.ma.trail.res.pred$mean, start=c(end_year, end_week), frequency=freq)
comb_ts    <- ts(tot.fst.2level,             start=c(end_year, end_week), frequency=freq)

# 2. Plot using autoplot + autolayer
autoplot(elec.ts) +
  autolayer(hw_ts,   series="Holt–Winters only") +
  autolayer(ma_ts,   series="MA(residuals) only") +
  autolayer(comb_ts, series="2-level combined") +
  ggtitle("Electricity Consumption & Forecasts") +
  xlab("Year") + ylab("Consumption") +
  scale_colour_manual(
    "Series",
    values=c("black","blue","green","red"),
    breaks=c("Data","Holt–Winters only","MA(residuals) only","2-level combined")
  ) +
  theme_minimal()

# 3. (Optional) If you prefer base R:
ts.plot(
  elec.ts, hw_ts, ma_ts, comb_ts,
  col=c("black","blue","green","red"),
  lty=c(1,2,3,4),
  xlab="Year", ylab="Consumption"
)
legend(
  "topleft",
  legend=c("Actual","Holt–Winters","MA(residuals)","Combined"),
  col=c("black","blue","green","red"),
  lty=1:4,
  bty="n"
)




####Regression model with seasonality

## FIT REGRESSION MODEL WITH SEASONALITY: MODEL 4. 
## FORECAST AND PLOT DATA, AND MEASURE ACCURACY.

# Use tslm() function to create seasonal model.
train.season <- tslm(train.ts ~ season)

# See summary of seasonal model and associated parameters.
summary(train.season)

# If necessary, run the following code to identify seasons.
train.season$data 

# Apply forecast() function to make predictions for ts with 
# seasonality data in validation set.  
train.season.pred <- forecast(train.season, h = nValid, level = 0)
train.season.pred 

#ACCURACY
round(accuracy(train.season.pred$mean, valid.ts), 3)


# Seasonal Regression model on the whole dataset
#Applying it on the whole dataset

# Use tslm() function to create seasonal model.
elec.season <- tslm(elec.ts ~ season)

# See summary of seasonal model and associated parameters.
summary(elec.season)

# Apply forecast() function to make predictions for ts with 
# seasonality data in validation set.  
elec.season.pred <- forecast(elec.season, h = 52, level = 0)
elec.season.pred


####Applying Trailing MA

# Identify and display regression residuals for entire data set.
elec.trend.seas.res <- elec.season$residuals
elec.trend.seas.res

# Use trailing MA to forecast residuals for entire data set.
elec.ma.trail.res <- rollmean(elec.trend.seas.res, k = 4, align = "right")
elec.ma.trail.res

# Create forecast for trailing MA residuals for future 52 periods.
elec.ma.trail.res.pred <- forecast(elec.ma.trail.res, h = 52, level = 0)
elec.ma.trail.res.pred

# Develop 2-level forecast for future 52 periods by combining 
# regression forecast and trailing MA for residuals for future
# 52 periods.
tot.fst.2level.elec <- elec.season.pred$mean + elec.ma.trail.res.pred$mean
tot.fst.2level.elec

# Create a table with regression forecast, trailing MA for residuals,
# and total forecast for future 52 periods.
future12.df.elec <- round(data.frame(elec.season.pred$mean, elec.ma.trail.res.pred$mean, 
                                     tot.fst.2level.elec), 3)
names(future12.df.elec) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
future12.df.elec


# Get aligned residuals
aligned_ma_res <- na.omit(elec.ma.trail.res)

# Align with regression fitted values
combined_fitted <- elec.season.pred$fitted[-c(1:3)] + aligned_ma_res

# Historical residuals + future residual forecasts
historical_ma_res <- c(aligned_ma_res, elec.ma.trail.res.pred$mean)

# Full combined forecast
full_combined <- elec.season.pred$fitted[-c(1:3)] + historical_ma_res



#Chekcing Accuracy
# Use accuracy() function to identify common accuracy measures.
# Use round() function to round accuracy measures to three decimal digits.
round(accuracy(train.season.pred$mean, valid.ts), 3)
round(accuracy(combined_fitted, elec.ts[-c(1:3)]), 3)
round(accuracy((naive(elec.ts))$fitted, elec.ts), 3)
round(accuracy((snaive(elec.ts))$fitted, elec.ts), 3)

#Visualization of Regression model 


#visualization of regression model with seasonality
# 1. Figure out where your forecasts should start:
freq      <- frequency(elec.ts)            # should be 52
end_year  <- end(elec.ts)[1]
end_week  <- end(elec.ts)[2] + 1
if(end_week > freq) {
  end_year <- end_year + 1
  end_week <- 1
}

# 2. Turn each forecast vector into a ts of length 52 starting at that point
reg_ts   <- ts(elec.season.pred$mean,      start=c(end_year, end_week), frequency=freq)
ma_ts    <- ts(elec.ma.trail.res.pred$mean, start=c(end_year, end_week), frequency=freq)
comb_ts  <- ts(tot.fst.2level.elec,         start=c(end_year, end_week), frequency=freq)

# 3. Plot with autoplot + autolayer
autoplot(elec.ts) +
  autolayer(reg_ts,   series="Seasonal Reg only") +
  autolayer(ma_ts,    series="MA(residuals) only") +
  autolayer(comb_ts,  series="2-level combined") +
  ggtitle("Electricity Consumption & Seasonal-Reg + MA-Residual Forecasts") +
  xlab("Year") + ylab("Consumption") +
  scale_colour_manual(
    "Series",
    values=c("black","blue","green","red"),
    breaks=c("Data","Seasonal Reg only","MA(residuals) only","2-level combined")
  ) +
  theme_minimal()

# 4. (Optional) Base‐R version
ts.plot(
  elec.ts, reg_ts, ma_ts, comb_ts,
  col=c("black","blue","green","red"),
  lty=c(1,2,3,4),
  xlab="Year", ylab="Consumption"
)
legend(
  "topleft",
  legend=c("Actual","Seasonal Reg","MA(residuals)","Combined"),
  col=c("black","blue","green","red"),
  lty=1:4,
  bty="n"
)


#####Implementing Arima Model

#Implementing ARIMA model for Partitioned data
# Use auto.arima() function to fit ARIMA model.
# Use summary() to show auto ARIMA model and its parameters.
train.auto.arima <- auto.arima(train.ts)
summary(train.auto.arima)

# Apply forecast() function to make predictions for ts with 
# auto ARIMA model in validation set.  
train.auto.arima.pred <- forecast(train.auto.arima, h = 52, level = 0)
train.auto.arima.pred

# Using Acf() function, create autocorrelation chart of auto ARIMA 
# model residuals.
Acf(train.auto.arima$residuals, lag.max = 12, 
    main = "Autocorrelations of Auto ARIMA Model Residuals")


#checking accuracy
round(accuracy(train.auto.arima.pred$mean, valid.ts), 3)

### ARIMA Model on whole dataset

#Implementing ARIMA on full dataset
arima.seas <- auto.arima(elec.ts)
summary(arima.seas)

# Apply forecast() function to make predictions for ts with 
# seasonal ARIMA model for the future 12 periods. 
arima.seas.pred <- forecast(arima.seas, h = 12, level = 0)
arima.seas.pred

# Use Acf() function to create autocorrelation chart of seasonal ARIMA 
# model residuals.
Acf(arima.seas$residuals, lag.max = 12, 
    main = "Autocorrelations of Seasonal ARIMA (0,1,2)(0,1,1) Model Residuals")

#checking accuracy
round(accuracy(HW.ZZZ.pred.full$fitted, valid.ts), 3)
round(accuracy(HW.ZZZ.pred.full$fitted+tot.ma.trail.res, elec.ts), 3)
round(accuracy(train.season.pred$mean, valid.ts), 3)
round(accuracy(train.auto.arima.pred$mean, valid.ts), 3)
round(accuracy(arima.seas.pred$fitted, elec.ts), 3)
round(accuracy((naive(elec.ts))$fitted, elec.ts), 3)
round(accuracy((snaive(elec.ts))$fitted, elec.ts), 3)


#visualization of ARIMA model forecast
# -- 1) Using forecast’s autoplot on the forecast object:

# This will plot the historical series and the 12‐step ARIMA forecast.
autoplot(arima.seas.pred) +
  ggtitle("Electricity Consumption & ARIMA Forecast") +
  xlab("Year") + ylab("Consumption") +
  theme_minimal()

# -- 2) Or, manually layer actual + forecast as ts objects:

# Figure out where forecasts start:
freq     <- frequency(elec.ts)          # should be 52
eyear    <- end(elec.ts)[1]
eweek    <- end(elec.ts)[2] + 1
if(eweek > freq) {
  eyear <- eyear + 1
  eweek <- 1
}

# Turn the 12‐step forecast into a ts with the right start:
arima_ts <- ts(arima.seas.pred$mean,
               start = c(eyear, eweek),
               frequency = freq)

# Plot with ggplot2 layers:
autoplot(elec.ts) +
  autolayer(arima_ts, series="ARIMA Forecast") +
  ggtitle("Electricity Consumption & ARIMA Forecast") +
  xlab("Year") + ylab("Consumption") +
  scale_colour_manual(
    "Series",
    values=c("Actual"="black","ARIMA Forecast"="blue"),
    breaks=c("Actual","ARIMA Forecast")
  ) +
  theme_minimal()

# -- 3) Base‐R fallback:

ts.plot(
  elec.ts, arima_ts,
  col=c("black","blue"),
  lty=c(1,2),
  xlab="Year", ylab="Consumption"
)
legend(
  "topleft",
  legend=c("Actual","ARIMA Forecast"),
  col=c("black","blue"),
  lty=1:2,
  bty="n"
)