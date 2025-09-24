# PROJECT Lisa Rigo, Paola D'Amore, Jacopo Malaguti
rm(list=ls())

# Load libraries (install if missing)
{
  packages <- c(
    "dplyr", "lubridate", "tsibble", "ggplot2", "forecast",   
    "urca", "tidyr", "readxl", "zoo", "fable", 
    "fabletools", "uroot", "feasts"
  )
  
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg)
    }
    library(pkg, character.only = TRUE)
  }
}


# General data preparation for dataset
{
# Load raw data
raw_data <- read.csv2("TheOutliers_Data.csv", stringsAsFactors=TRUE)
data <- raw_data
str(data) 

# Managing date-time
data$time_series <- as.POSIXct(data$time_series, format = "%d/%m/%y %H:%M", tz = "UTC")
str(data$time_series)
head(data$time_series)
tail(data$time_series)

# data hourly 
data_hourly<- data %>% 
  mutate(YearHour = format(time_series, "%Y-%m-%d %H:00:00")) %>% 
  group_by(YearHour) %>% 
  summarise(traffic_volume = mean(traffic_volume), .groups = "drop")

# Converts YearHour from string ("YYYY-mm-dd HH:MM:SS") to POSIXct object
data_hourly <- data_hourly %>%
  mutate(
    YearHour  = as.POSIXct(YearHour,                    
                           format = "%Y-%m-%d %H:%M:%S",
                           tz     = "UTC"),
    Hour      = factor(format(YearHour, "%H")),        
    Weekday   = wday(YearHour, week_start = 1),          # computes the day of the week (1 = Monday, …, 7 = Sunday)
    isWeekend = factor(ifelse(Weekday >= 6,             # defines “Weekend” if Saturday/Sunday, otherwise “Weekday”
                              "Weekend", "Weekday"))
  ) %>%
  arrange(YearHour)                                    # sorts the data frame in ascending chronological order
}

#EDA
{
#Plot data 
plot(data$time_series,data$traffic_volume) #heavy because of hourly obs

# Create hourly daily time series
ts_hourly<- ts(data_hourly$traffic_volume, start = c(2016, 1), frequency = 24)  
plot(stl(ts_hourly,s.window = "periodic")) 
acf(ts_hourly,lag=60) # seasonal pattern to model, a lot of persistence --> seems a deterministic seasonality

# Create hourly weekly time series 
ts_hourly_WEEK<- ts(data_hourly$traffic_volume, start = c(2016, 1), frequency = 24*7)  
plot(stl(ts_hourly_WEEK,s.window = "periodic")) 
acf(ts_hourly_WEEK,lag=60) # seasonal pattern to model, a lot of persistence--> --> seems a deterministic seasonality
    

#To look for possible trend and seasonality we choose:
################ TO EXPLORE daily weekly pattern over years and month ################
  {
    # year, month, weekday extracted
    data_2 <- data %>% #create data frame with 3 new time columns 
      mutate(
        year       = year(time_series),
        month      = month(time_series, label = TRUE, abbr = FALSE),
        weekday    = wday(time_series, label = TRUE, abbr = FALSE, week_start = 1)
      )
    data_2
    
    # AVERAGE TRAFFIC VOLUME for each weekday by year
    weekday_by_year <- data_2 %>%
      group_by(year, weekday) %>%
      summarise(mean_vol = mean(traffic_volume, na.rm = TRUE), .groups = "drop")
    
    # PLOT 
    ggplot(weekday_by_year, aes(x = weekday, y = mean_vol, color = factor(year), group = year)) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      labs(
        title = "Average Traffic Volume by Weekday — by Year",
        x = "Weekday",
        y = "Average Volume",
        color = "Year"
      ) +
      theme_minimal(base_size = 12)
    
    # AVERAGE TRAFFIC VOLUME for each weekday by month 
    weekday_by_month <- data_2 %>%
      group_by(month, weekday) %>%
      summarise(mean_vol = mean(traffic_volume, na.rm = TRUE), .groups = "drop")
    
    # PLOT
    ggplot(weekday_by_month, aes(x = weekday, y = mean_vol, group = month, color = month)) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      labs(
        title = "Average Traffic Volume by Weekday — by Month",
        x = "Weekday",
        y = "Average Traffic Volume",
        color = "Month"
      ) +
      theme_minimal(base_size = 12)
    
  }
############### TO EXPLORE hourly  pattern over the month and years ##################
  {
    data_3 <- data %>% #create data frame with 3 new time columns 
      mutate(
        year       = year(time_series),
        month      = month(time_series, label = TRUE, abbr = FALSE),
        weekday    = wday(time_series, label = TRUE, abbr = FALSE, week_start = 1),
        hour       = hour(time_series)
      )
    data_3
    
    # AVERAGE TRAFFIC VOLUME by hour of the day by year
    hourly_by_year <- data_3 %>%
      group_by(year, hour) %>%
      summarise(mean_vol = mean(traffic_volume, na.rm = TRUE), .groups = "drop")
    
    # PLOT
    ggplot(hourly_by_year, aes(x = hour, y = mean_vol, color = factor(year), group = year)) +
      geom_line(size = 1.2) +
      geom_point(size = 2.5) +
      labs(title = "Average Traffic Volume by Hour of the Day — by Year",
           x = "Hour of the Day", y = "Average Volume", color = "Year") +
      theme_minimal()
    
    #it reveals a strong seasonality
    
    # AVERAGE TRAFFIC VOLUME by hour of the day by month
    hourly_by_month <- data_3 %>%
      group_by(month, hour) %>%
      summarise(mean_vol = mean(traffic_volume, na.rm = TRUE), .groups = "drop")
    
    # PLOT
    ggplot(hourly_by_month, aes(x = hour, y = mean_vol, color = month, group = month)) +
      geom_line(size = 1.2) +
      geom_point(size = 2.5) +
      labs(title = "Average Traffic Volume by Hour of the Day — by Month",
           x = "Hour of the Day", y = "Average Volume", color = "Month") +
      theme_minimal()
    
    #it reveals a strong seasonality
    
    # AVERAGE TRAFFIC VOLUME by hour of the day by weekday
    hourly_by_weekday <- data_3 %>%
      group_by(weekday, hour) %>%
      summarise(mean_vol = mean(traffic_volume, na.rm = TRUE), .groups = "drop")
  }

############### TO EXPLORE hourly  pattern over the week  ############################
 
 {
    # PLOT WEEKEND VS WEEKDAY
    ggplot(hourly_by_weekday, aes(x = hour, y = mean_vol, color = weekday, group = weekday)) +
      geom_line(size = 1.2) +
      geom_point(size = 2.5) +
      labs(title = "Average Traffic Volume by Hour of the Day — by Day of the Week",
           x = "Hour of the Day", y = "Average Volume", color = "Day of the Week") +
      theme_minimal()
    
    
}


# AVERAGE TRAFFIC VOLUME ON MONDAYS AT 8 AM BY MONTH 
monday_8am <- data %>%
  mutate(
    year         = year(time_series),
    month        = month(time_series, label = TRUE, abbr = TRUE),  
    hour         = hour(time_series),
    weekday_num  = wday(time_series, week_start = 1)                  
  ) %>%
  filter(weekday_num == 1, hour == 8) %>%
  group_by(year, month) %>%
  summarise(mean_vol = mean(traffic_volume, na.rm = TRUE), .groups = "drop")

# PLOT
ggplot(monday_8am, aes(x = month, y = mean_vol, color = factor(year), group = year)) +
  geom_line(size = 1.2) +
  geom_point(size = 2.5) +
  labs(
    title = "Traffic Volume — Mondays at 8 AM by Month",
    x = "Month",
    y = "Average Traffic Volume",
    color = "Year"
  ) +
  theme_minimal(base_size = 12)



# AVERAGE TRAFFIC VOLUME ON SATURDAYS AT 8 AM BY MONTH
saturday_8am <- data %>%
  mutate(
    year        = year(time_series),
    month       = month(time_series, label = TRUE, abbr = TRUE),  
    weekday_num = wday(time_series, week_start = 1),               
    hour_col    = hour(time_series)
  ) %>%
  filter(weekday_num == 6, hour_col == 8) %>%
  group_by(year, month) %>%
  summarise(mean_vol = mean(traffic_volume, na.rm = TRUE), .groups = "drop")

# PLOT
ggplot(saturday_8am, aes(x = month, y = mean_vol, color = factor(year), group = year)) +
  geom_line(size = 1.2) +
  geom_point(size = 2.5) +
  labs(
    title = "Traffic Volume — Saturdays at 8 AM by Month",
    x = "Month",
    y = "Average Traffic Volume",
    color = "Year"
  ) +
  theme_minimal(base_size = 12)

}

#TEST 

{
################################# Run augmented Dickey Fuller test 
  {
    # 1)Simple Dickey Fuller,  No intercept, no trend
    summary(ur.df(ts_hourly, type = "none",lag=3, selectlags = "BIC")) 
   
    # 2.1) General Dickey Fuller  with intercept only
    summary(ur.df(ts_hourly, type = "drift", lags = 3, selectlags = "BIC")) 
   
     #2.2) General Dickey Fuller with intercept and linear trend
    summary(ur.df(ts_hourly, type = "trend", lags = 3, selectlags = "BIC")) 
    
    }
  
################################# Run  OCSB
  {
#In order to run the OCSB for seasonal unit root you need a stochastic trend that we don't have
  ocsb.test(ts_hourly, m = 24)
# As we already suspect, we reject the null hypothesis.
  }
  
################################# Run CH test, for seasonal variation

{
# Perform CH test, for seasonal variation, on monday's 8 AM ts
  
  # Create "date" variable for each year-month
  monday_8am <- monday_8am %>%
    mutate(date = as.Date(paste(year, month, "01", sep = "-"), format = "%Y-%B-%d")) %>%
    arrange(date)
  
  # Monthly time series (frequency 12)
  ts_monday8 <- ts(monday_8am$mean_vol,
                   start = c(min(monday_8am$year), 1),
                   frequency = 12)
  
  # Canova–Hansen test: monthly seasonality
  ch.test(ts_monday8, lag = 12)
  
  
# Perform CH test, for seasonal variation, on saturday's 8 AM ts
  
  # Create "date" variable for each year-month
  saturday_8am <- saturday_8am %>%
    mutate(date = as.Date(paste(year, month, "01", sep = "-"), format = "%Y-%B-%d")) %>%
    arrange(date)
  
  # Monthly time series (frequency 12)
  ts_saturday8 <- ts(saturday_8am$mean_vol,
                     start = c(min(saturday_8am$year), 1),
                     frequency = 12)
  
  # Canova–Hansen test: monthly seasonality
  ch.test(ts_saturday8, lag = 12)      
  

}
}

# FORECAST 72 STEP AHEAD WITH THE CHOOSEN MODELS  
# General data preparation
{
#Setting forecast horizon 
h <- 72
cutoff_time       <- max(data_hourly$YearHour) - hours(h)
train_data_hourly <- data_hourly[data_hourly$YearHour <= cutoff_time, ]
test_data_hourly  <- data_hourly[data_hourly$YearHour  > cutoff_time, ]
}

#1.##########     EXPONENTIAL SMOOTHING      ############# 
{
# Create  tsibble object
    tsbl_hourly <- data_hourly %>%
      as_tsibble(index = YearHour)
    
    #Look at the structure
    glimpse(tsbl_hourly)
  
  ##############################################
  #FORECASTING 72 STEP AHEAD SES,and HOLT-WINTER
  ##############################################
  {
    # training set 
    train_tsbl <- tsbl_hourly %>%
      filter(YearHour <= max(YearHour) - hours(h)) #exclude last 72h
    
    # Test set for future comparison
    test_tsbl <- tsbl_hourly %>%
      filter(YearHour > max(YearHour) - hours(h))
    
    # State-space model of exponential smoothing: SES, Holt, Holt-Winters additive
    model_fit <- train_tsbl %>%
      model(
        SES   = ETS(traffic_volume ~ error("A") + trend("N") + season("N")),
        HWadd = ETS(traffic_volume ~ error("A") + trend("N") + season("A"))
        #alpha is automatically estimated by maximizing the likelihood,implementing an algorithm similar to Nelder-Mead but with constraints.
      )
    
    # Summary  TSIBBLE
    model_fit %>% select(SES) %>% report()
    model_fit %>% select(HWadd) %>% report()
    
    #Fitted value  
    model_fit %>% 
      select(SES) %>%
      augment()
    model_fit %>% 
      select(HWadd) %>%
      augment()
    
    #Forecast value 
    model_fit %>%
      select(SES) %>%
      forecast(h = "72 hours",bootstrap=TRUE)
    model_fit %>%
      select(HWadd) %>%
      forecast(h = "72 hours",bootstrap=TRUE)
    
    ####PLOT 
    #SES
    #Fitted values 
    model_fit %>% 
      select(SES) %>%
      augment() %>%
      ggplot(aes(x = YearHour)) +
      geom_line(aes(y = traffic_volume), color = "black", linetype = "dotted") +
      geom_line(aes(y = .fitted), color = "blue") +
      labs(title = "SES — Fitted vs Actual (Training Set)",
           y = "Traffic Volume") +
      theme_minimal()
    #Forecast
    model_fit %>%
      select(SES) %>%
      forecast(h = "72 hours",bootstrap=TRUE) %>%
      autoplot(test_tsbl) +
      labs(title = "SES — Forecast vs Test Set",
           y = "Traffic Volume", x = "Time") +
      theme_minimal()
    
    #Holt-Winter 
    model_fit %>% 
      select(HWadd) %>%
      augment() %>%
      ggplot(aes(x = YearHour)) +
      geom_line(aes(y = traffic_volume), color = "black", linetype = "dotted") +
      geom_line(aes(y = .fitted), color = "darkgreen") +
      labs(title = "HW Additive — Fitted vs Actual (Training Set)",
           y = "Traffic Volume") +
      theme_minimal()
    model_fit %>%
      select(HWadd) %>%
      forecast(h = "72 hours",bootstrap=TRUE) %>%
      autoplot(test_tsbl) +
      labs(title = "HW Additive — Forecast vs Test Set",
           y = "Traffic Volume", x = "Time") +
      theme_minimal()
    
    #COMPARE MODEL  
    
    # Calculate 72 step ahead forecast for each model
    forecast_all <- model_fit %>%
      forecast(h = "72 hours",bootstrap=TRUE)
   
     # Last week of data + forecast (for better visualization)
    last_week <- tsbl_hourly %>%
      filter(YearHour >= max(YearHour) - days(7))
    
    forecast_all <- model_fit %>% forecast(h = "72 hours") 
    
    autoplot(last_week, traffic_volume) +
      autolayer(forecast_all, level = NULL) +
      labs(title = "Forecast Comparison — SES, HW Additive",
           y = "Traffic Volume", x = "Time", color = "Model") +
      theme_minimal()
    
    #blue line = forecasts
    #black line = actual values 
  
  #The theoretical evaluation (in-sample) measures how well the model fits the data
  #The Holt-Winters Additive model (HWadd) is the best in-sample, 
  #however, this does not guarantee better out-of-sample performance.
  }
}
#2.##########     LM + SARIMA                #############
{
#2a.           ##### LM    ####
{
  
model_hr <- lm(
  traffic_volume ~ Hour * isWeekend -1, #dummy trap
  data = train_data_hourly)

model_hr
summary(model_hr)

#Check residuals
residuals_hr <- residuals(model_hr)
acf(residuals_hr, lag.max = 60) # DISPLAY PERSISTENCE

######### POINT FORECAST 72 STEP AHEAD WITH  LM
######using predict
{
  reg_preds <- predict(model_hr, newdata = test_data_hourly)
  
  # Extract observed value 
  observed <- test_data_hourly$traffic_volume
  
  # Fix the visualization range: last 48h of training + 72h of test
  start_plot <- cutoff_time - hours(48)
  end_plot   <- max(test_data_hourly$YearHour)
  
  # Plot
  plot(test_data_hourly$YearHour, observed, type = "l", lwd = 2, ylim = c(0, 6500),
       xlab = "Time", ylab = "Traffic Volume",
       main = "Point forecasts in the last 72 hours with predict()")
  lines(test_data_hourly$YearHour, reg_preds, col = "red", lwd = 2)
  legend("topright", legend = c("Observed values","Forecast"), col = c("black","red"), lwd = 2)
}
######using algebra
{
  # Extract estimated coefficients
  beta <- coef(model_hr)
  
  # Build the design matrix for the test set
  X_forecast <- model.matrix(traffic_volume ~ Hour * isWeekend -1, data = test_data_hourly) # io mi sono chiesta se sia giusto fare questa cosa, dato che non conosiamo il test set in teoria
  
  #  Compute the forecasts
  reg_preds_2 <- as.vector(X_forecast %*% beta)
  
  # Add the forecasts to the test data frame
  test_data_hourly <- test_data_hourly %>%
    mutate(pred2 = reg_preds_2)
  
  # Fix the visualization range: last 48h of training + 72h of test
  start_plot <- cutoff_time - hours(48)
  end_plot   <- max(test_data_hourly$YearHour)
  
  # Plot
  plot(test_data_hourly$YearHour, test_data_hourly$traffic_volume,
       type = "l", lwd = 2, ylim = c(0, 6500),
       xlab = "Time", ylab = "Traffic Volume",
       main = "Point forecasts in the last 72 hours with algebra")
  lines(test_data_hourly$YearHour,
        test_data_hourly$pred2,
        col = "red", lwd = 2)
  legend("topright",
         legend = c("Observed values","Forecast"),
         col    = c("black",  "red"),
         lwd    = 2)
  
}
########### 95% bootstrapped prediction intervals WITH  LM 
B <- 1000 # Number of bootstrap replications

# Initialize the matrix for the bootstrap series
y_star_hr <- matrix(0, nrow = B, ncol = h)
# Generate bootstrap series by adding resampled residuals
for (j in 1:B) {
  residuals_star <- sample(residuals_hr, size = h, replace = TRUE) # resample h residuals with replacement
  y_star_hr[j, ] <- reg_preds + residuals_star   # add the residuals to the point forecasts
}

# Compute the 2.5% and 97.5% quantiles for each hour
prediction_interval_lb_lr <- apply(y_star_hr, 2, quantile, probs = 0.025)
prediction_interval_ub_lr <- apply(y_star_hr, 2, quantile, probs = 0.975)

# Plot (POSIXct axis)
par(mfrow = c(1,1))

# Fix the visualization range: last 48h of training + 72h of test
start_plot <- cutoff_time - hours(48)
end_plot   <- max(test_data_hourly$YearHour)

# Historical series of observed data
plot(data_hourly$YearHour, data_hourly$traffic_volume, type = "l",
     xlim = c(start_plot, end_plot), ylim = c(-2000, 7500),
     main = "95% Bootstrap Prediction Interval over Last 72h",
     xlab = "Datetime", ylab = "Traffic Volume")

# Point forecast on the test set
lines(test_data_hourly$YearHour, point_forecast_1, col = "red", lwd = 2)

# Uncertainty band
polygon(
  x = c(test_data_hourly$YearHour, rev(test_data_hourly$YearHour)),
  y = c(prediction_interval_lb_lr, rev(prediction_interval_ub_lr)),
  col    = rgb(1, 0, 0, alpha = 0.3),
  border = NA)

# Lines of the lower and upper bounds
lines(test_data_hourly$YearHour, prediction_interval_lb_lr, col = "red", lty = 2)
lines(test_data_hourly$YearHour, prediction_interval_ub_lr, col = "red", lty = 2)

}
  
#2.b           ##### SARIMA ####
{
# residuals and ACF/PACF 
reg_residuals<-residuals(model_hr) 
reg_residuals_ts <- ts(reg_residuals, start = c(2016, 1), frequency = 24)
reg_fitted<-fitted(model_hr)
reg_fitted_ts <- ts(reg_fitted, start = c(2016, 1), frequency = 24)

plot(reg_fitted_ts) 
plot(reg_residuals_ts) 

adf_residuals<- ur.df(reg_residuals_ts, type= "none") 

acf(reg_residuals, lag.max= 100) #no cutoff, spikes at lag 24,48,ecc.. Q=1,2
abline(v = seq(24, 168, by = 24), col = "red", lty = 2) 
pacf(reg_residuals, lag.max= 100) #cutoff at lag 2 p=2, high spike lag 24
abline(v = seq(24, 168, by = 24), col = "blue", lty = 2)
tsdisplay(reg_residuals, lag.max=168, main="Residuals ACF/PACF")


sarima_model23 <- Arima(reg_residuals, 
                        order = c(2, 0, 3), 
                        seasonal = list(order = c(1, 0, 1), period = 24))

sarima_model22 <- Arima(reg_residuals, 
                        order = c(2, 0, 2), 
                        seasonal = list(order = c(1, 0, 1), period = 24)) 

sarima_model24 <- Arima(reg_residuals, 
                        order = c(2, 0, 4), 
                        seasonal = list(order = c(1, 0, 1), period = 24)) 

sarima_model2212 <- Arima(reg_residuals, 
                          order = c(2, 0, 2), 
                          seasonal = list(order = c(1, 0, 2), period = 24)) 


AIC(sarima_model23, sarima_model22, sarima_model24,sarima_model2212)
BIC(sarima_model23, sarima_model22, sarima_model24,sarima_model2212) 

# BIC is more parsimonious 

sarima_res<- residuals(sarima_model22)
acf(sarima_res) 
pacf(sarima_res)

hist(sarima_res) 
checkresiduals(sarima_model22) # residuals are not white noise, but they are overall clean, no substantial autocorrelation

qqnorm(sarima_res)
qqline(sarima_res, col = 2)
# residuals are not normal 
}
  
#2.c           ##### LM + SARIMA    ####
{
#### POINT FORECASTS 72 STEP AHEAD WITH: LR + SARIMA  ####

reg_preds <- predict(model_hr, newdata = test_data_hourly)

sarima_preds <- forecast(sarima_model22, h = 72)$mean 
# Reg + SARIMA
combined_preds_sarima <- reg_preds + sarima_preds


#### VISUALIZATION WITH TSIBBLE ####
# Table with forecasts and timestamps
forecast_tbl <- tibble(
  YearHour = test_data_hourly$YearHour,
  reg_forecast = reg_preds,
  sarima_correction = sarima_preds,
  combined_forecast = combined_preds_sarima,
  actual = test_data_hourly$traffic_volume
)

# Conversion to tsibble
forecast_tsibble <- forecast_tbl %>%
  as_tsibble(index = YearHour)

# Forecasts vs. actual data
ggplot(forecast_tsibble, aes(x = YearHour)) +
  geom_line(aes(y = combined_forecast), color = "blue", size = 1.1) +
  geom_line(aes(y = actual), color = "black", linetype = "dashed") +
  labs(
    title = "Combined Forecast (Regression + SARIMA)",
    x = "Hour", y = "Traffic volume",
    caption = "Blue = predicted | Dashed Black = observed"
  ) +
  theme_minimal()

#### DIFFERENCE BETWEEN REGRESSION AND COMBINED (reg+sarima) ####

# Table with forecasts and observed values
forecast_compare_tbl <- tibble(
  YearHour = test_data_hourly$YearHour,
  actual = test_data_hourly$traffic_volume,
  reg_forecast = reg_preds,
  sarima_correction = sarima_preds,
  combined_forecast = combined_preds_sarima
)

# Difference between regression forecasts and combined forecasts
forecast_compare_tbl <- forecast_compare_tbl %>%
  mutate(
    diff_abs = abs(reg_forecast - combined_forecast), # absolute difference
    diff_pct = 100 * diff_abs / actual  # percentage difference relative to observed values
  )

print(head(forecast_compare_tbl, 20))

# Observed values and forecasts
ggplot(forecast_compare_tbl, aes(x = YearHour)) +
  geom_line(aes(y = actual), color = "black", linetype = "dashed", size = 1, alpha = 0.7) +
  geom_line(aes(y = reg_forecast), color = "blue", size = 1) +
  geom_line(aes(y = combined_forecast), color = "red", size = 1) +
  labs(title = "Regression (blue) vs. Regression+SARIMA (red)",
       y = "Traffic Volume", x = "Hour") +
  theme_minimal()

#### BOOTSTRAP PREDICTION INTERVAL 95% ####

B <- 1000 # Number of bootstrap replications
h <- length(reg_preds)  # test set length in hours, 72

# Matrix to store bootstrap simulations
y_star <- matrix(NA, nrow = B, ncol = h)

set.seed(123) 

for (b in 1:B) {
  # Simulate SARIMA errors of length h (residual simulation)
  sarima_sim <- simulate(sarima_model22, nsim = h)
  
  # Simulation of SARIMA + regression forecasts
  y_star[b, ] <- reg_preds + sarima_sim
}

# LB and UB
boot_lb_SAR <- apply(y_star, 2, quantile, probs = 0.025)
boot_ub_SAR <- apply(y_star, 2, quantile, probs = 0.975)

# Plot with bootstrap prediction intervals

# Extend visualization range: last 48h of training + 72h of test
start_plot <- cutoff_time - hours(48)
end_plot <- max(test_data_hourly$YearHour)

plot(data_hourly$YearHour, data_hourly$traffic_volume, type = "l",
     xlim = c(start_plot, end_plot),
     ylim = c(min(boot_lb_SAR), max(boot_ub_SAR)),
     main = "Bootstrap 95% Prediction Intervals (Regression + SARIMA)",
     xlab = "Datetime", ylab = "Traffic Volume")

# Regression + SARIMA
lines(test_data_hourly$YearHour, reg_preds + sarima_preds, col = "blue", lwd = 2)

# Uncertainty band (bootstrap confidence interval)
polygon(
  x = c(test_data_hourly$YearHour, rev(test_data_hourly$YearHour)),
  y = c(boot_lb_SAR, rev(boot_ub_SAR)),
  col = rgb(0, 0, 1, alpha = 0.2), border = NA
)

# Lower and upper bounds
lines(test_data_hourly$YearHour, boot_lb_SAR, col = "darkblue", lty = 2)
lines(test_data_hourly$YearHour, boot_ub_SAR, col = "darkblue", lty = 2)

legend("bottomleft", legend = c("Observed Values", "Combined Forecast", "Bootstrap PI 95%"),
       col = c("black", "blue", "darkblue"), lty = c(1,1,2), lwd = c(1,2,1), bty = "n")

#### Analysis with an exogenous covariate ####

# POSIXct 
raw_data$time_series <- as.POSIXct(raw_data$time_series, format = "%d/%m/%y %H:%M", tz = "UTC")

# TRAIN / TEST
cutoff_time <- max(raw_data$time_series) - hours(h)
train_raw <- raw_data[raw_data$time_series <= cutoff_time, ]
test_raw  <- raw_data[raw_data$time_series > cutoff_time, ]

# hourly
train_data_hourly <- train_raw %>% mutate(YearHour = floor_date(time_series, unit = "hour")) %>% group_by(YearHour) %>% summarise( traffic_volume = mean(traffic_volume), temp = mean(temp), rain_1h = mean(rain_1h), snow_1h = mean(snow_1h), clouds_all = mean(clouds_all), .groups = "drop" )

# CCF regression residuals (stationary) vs covariates
covariate_names <- c("temp","rain_1h","snow_1h", "clouds_all"  )  
par(mfrow = c(2, 2))  # all in 1
for (cov in covariate_names) {
  ccf(reg_residuals, train_data_hourly[[cov]], lag.max = 24, main = paste("CCF with", cov))
}
par(mfrow = c(1, 1))


# lagged covariate (1 hour)
train_data_hourly <- train_data_hourly %>%
  mutate(temp_lag1 = dplyr::lag(temp, 1)) %>%
  filter(!is.na(temp_lag1))  

test_raw <- test_raw %>%
  mutate(temp_lag1 = dplyr::lag(temp, 1, default = tail(train_data_hourly$temp,1)))

# Regressor matrices
xreg_train <- as.matrix(train_data_hourly$temp_lag1)
xreg_test  <- as.matrix(test_raw$temp_lag1)


# model
sarimacov <- Arima(train_data_hourly$traffic_volume,
                   order = c(2,0,7), 
                   seasonal = list(order = c(1,0,1), period = 24),
                   xreg = xreg_train)
summary(sarimacov)

# RESIDUALS
sarimacov_res <- residuals(sarimacov)
acf(sarimacov_res, main = "ACF of SARIMAX Residuals")
pacf(sarimacov_res, main = "PACF of SARIMAX Residuals")
#not white noise

# check residuals 
hist(sarimacov_res) 
checkresiduals(sarimacov_res)

qqnorm(sarimacov_res)
qqline(sarimacov_res, col = 2)

# The residuals are not normal  


#### POINT FORECASTS 72 STEP AHEAD ####
sarimacov_forecast <- forecast(sarimacov,
                               h = nrow(test_raw),
                               xreg = xreg_test)

### VISUALIZATION ####
### Forecasts vs. observed values
plot(test_raw$time_series, test_raw$traffic_volume, type = "l",
     col = "black", lwd = 2, xlab = "Datetime", ylab = "Traffic Volume",
     main = "Observed vs SARIMAX Forecast")
lines(test_raw$time_series, sarimacov_forecast$mean, col = "red", lwd = 2)
legend("topleft", legend = c("Observed", "Forecast"),
       col = c("black","red"), lty = 1, bty = "n")

#### DIFFERENCE BETWEEN LM, LM + SARIMA, SARIMACOV ####


forecast_compare <- tibble(
  YearHour   = test_raw$time_series,
  Observed   = test_raw$traffic_volume,
  LM         = reg_preds,
  LM_SARIMA  = combined_preds_sarima,
  SARIMACOV    = sarimacov_forecast$mean
)


forecast_long <- forecast_compare %>%
  pivot_longer(cols = c("Observed","LM","LM_SARIMA","SARIMACOV"),
               names_to = "Series",
               values_to = "Traffic")

ggplot(forecast_long, aes(x = YearHour, y = Traffic, color = Series)) +
  geom_line(size = 1.1) +
  scale_color_manual(values = c(
    "Observed" = "black",
    "LM" = "blue",
    "LM_SARIMA" = "red",
    "SARIMACOV" = "green"
  )) +
  labs(
    title = "Forecast Comparison - Last 72 Hours",
    x = "Datetime",
    y = "Traffic Volume",
    color = "Series"
  ) +
  theme_minimal()

#### BOOTSTRAP PREDICTION INTERVAL 95% - SARIMAX ####
B <- 1000
h <- nrow(test_raw)  # forecast horizon (72 hours)

# Matrix to store bootstrap simulations
y_star <- matrix(NA, nrow = B, ncol = h)

set.seed(123)

for (b in 1:B) {
  # simulation from the SARIMACOV model, including future covariates
  sarimacov_sim <- simulate(
    sarimacov,
    nsim = h,
    xreg = xreg_test,     # covariates for the test set

    future = TRUE
  )
  
  y_star[b, ] <- sarimacov_sim
}

# Compute 95% bootstrap intervals
boot_lb <- apply(y_star, 2, quantile, probs = 0.025)
boot_ub <- apply(y_star, 2, quantile, probs = 0.975)

# last 48 hours of training + 72 hours of test
start_plot <- cutoff_time - hours(48)
end_plot <- max(test_raw$time_series)

plot(data_hourly$YearHour, data_hourly$traffic_volume, type = "l",
     xlim = c(start_plot, end_plot),
     ylim = c(min(boot_lb), max(boot_ub)),
     main = "Bootstrap 95% Prediction Intervals (SARIMAX)",
     xlab = "Datetime", ylab = "Traffic Volume")

# SARIMACOV forecast (bootstrap mean or official mean)
lines(test_raw$time_series, sarimacov_forecast$mean, col = "red", lwd = 2)

# bootstrap band
polygon(
  x = c(test_raw$time_series, rev(test_raw$time_series)),
  y = c(boot_lb, rev(boot_ub)),
  col = rgb(1, 0, 0, alpha = 0.2), border = NA
)

# lower and upper bounds
lines(test_raw$time_series, boot_lb, col = "darkred", lty = 2)
lines(test_raw$time_series, boot_ub, col = "darkred", lty = 2)

legend("bottomleft",
       legend = c("Observed Values", "SARIMACOV Forecast", "Bootstrap PI 95%"),
       col = c("black", "red", "darkred"),
       lty = c(1, 1, 2), lwd = c(1, 2, 1), bty = "n")

}
}


#2.PSEUDO OUT-OF SAMPLE ANALYSIS
#General stuff
{
  # Order
  tsbl_hourly <- tsbl_hourly %>% arrange(YearHour)
  head(data$time_series)
  tail(data$time_series)
  # Decision:
  # in-sample: 17544 ore (2y --> 2016-2017) 73%, 
  # Test set: 6552 ore ( 9m -->  2018-01-01 - 2018-09-30) 27%
  
  #Define test sample
  test_start <- ymd_h("2018-01-01 00")
  test_end <- ymd_h("2018-09-30 23")   # 6552 h in the test set 
  test_tsbl_2 <- tsbl_hourly %>%
    filter(YearHour >= test_start, YearHour <= test_end)                                       #test <- window(ts_monthly, start = test_start, end = test_end) 
  train_tsbl_2 <- tsbl_hourly %>%
    filter(YearHour < test_start)
  
  # Rolling forecasts parameters
  h <- 72                             # forecast horizon (72 hours)
  n_obs <- nrow(tsbl_hourly)          # total obs
  window_size <- 17544                # fixed window_size
  start_index <- window_size + 1 
  end_index <- n_obs - h + 1          # you cannot stop the window at the last date 
  n_forecasts <- end_index - start_index + 1  #  how many forecast do I have to do? For each hour of the test set.
}

# POINT FORECAST  
  {
    {
      # Initialize matrix for point forecast 
      point_ses <- point_hwadd <- matrix(NA, n_forecasts, h)
      point_forecast_lm <- point_forecast_LM_SARIMA <- matrix(NA, n_forecasts, h)
      
      # Rolling window forecast loop
      for (i in 1:n_forecasts) {
        message("Step ", i, "/", n_forecasts)  # to be updated on the progress of the loop
        
        # Training window
        train_window <- tsbl_hourly %>%
          slice(i:(i + window_size - 1)) 
        future_window <- tsbl_hourly %>% slice((i + window_size):(i + window_size + h - 1))
        # At each hour, the window moves forward of 1h 
        #-1 to keep the same size 
        
        # Fit ETS
        fit <- train_window %>%
          model(
            SES = ETS(traffic_volume ~ error("A") + trend("N") + season("N")),
            HWadd = ETS(traffic_volume ~ error("A") + trend("N") + season("A")) )
        
        # Fit LM
        xreg_train <- model.matrix(~ Hour * isWeekend, data = train_window)[, -1] # Regressor matrices (drop intercept)
        xreg_future <- model.matrix(~ Hour * isWeekend, data = future_window)[, -1]
        
        # Forecast Linear Regression 
        lm_model <- lm(traffic_volume ~ Hour * isWeekend -1, data = train_window)
        X_future <- model.matrix(lm_model, data = future_window)
        pred_lm <- X_future %*% coef(lm_model)
        
        
        # Fit SARIMA on residuals LM 
        resid_lm <- residuals(lm_model)  # Residuals of the train window for the i-th window 
        
        # Forecast model ETS
        fc <- forecast(fit, h = h)
        fc_ses <- fc %>% filter(.model == "SES") %>% pull(.mean)         # Extract point forecast 
        fc_hwadd <- fc %>% filter(.model == "HWadd") %>% pull(.mean)
        
        
       #Forecast SARIMA
        sarima_fit <- Arima(resid_lm,
                            order    = c(2, 0, 2),
                            seasonal = list(order = c(1, 0, 1), period = 24))
        
        sarima_fc   <- forecast(sarima_fit, h = h)
        resid_corr  <- as.numeric(sarima_fc$mean)   
        
         ## Combination SARIMA + LM
        combined_i <- as.numeric(pred_lm) + resid_corr
     
        # Store point forecast
           point_ses[i, ] <- fc_ses
           point_hwadd[i, ] <- fc_hwadd
           point_forecast_lm[i,] <- pred_lm
           point_forecast_LM_SARIMA[i, ] <- combined_i
           
      }
    }
  }


#MODEL DIAGNOSTIC to understand if we need bootstrap
    #Main diagnostic checks:
    # SES
    {
      # Check for no forecast bias
      # Residuals time series plot
      fit_ses <- train_window %>%
        model(
          SES = ETS(traffic_volume ~ error("A") + trend("N") + season("N"))
        )
      residuals_ses <- augment(fit_ses) %>%
        filter(.model == "SES") %>%
        select(YearHour, .resid)
     
       #Plot Residuals  
      ggplot(residuals_ses, aes(x = YearHour, y = .resid)) +
        geom_line() +
        geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
        labs(title = "Residuals over time", x = "Time", y = "Residuals") +
        theme_minimal()
      # residuals appear centered around 0 -> No forecast bias
      
      #--> NORMALITY --> GAUSSIAN ASSUMPTION OR BOOTSTRAP 
      hist(residuals_ses$.resid, main = "Histogram of residuals", xlab = "Residuals")
      qqnorm(residuals_ses$.resid)
      qqline(residuals_ses$.resid)
      # no normality --> bootstrap
    }
    
# Holt winters
    {
      
      # --> 	No forecast bias
      # Residuals time series plot
      fit_hwadd <- train_window %>%
        model(
          HWadd = ETS(traffic_volume ~ error("A") + trend("N") + season("A"))
        )
      
      residuals_hw <- augment(fit_hwadd) %>%
        filter(.model == "HWadd") %>%
        select(YearHour, .resid)
      # Plot residuals
      ggplot(residuals_hw, aes(x = YearHour, y = .resid)) +
        geom_line() +
        geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
        labs(title = "Residuals over time", x = "Time", y = "Residuals") +
        theme_minimal()
      # residuals appear centered around 0 -> No forecast bias
      
      
      #--> NORMALITY --> GAUSSIAN ASSUMPTION OR BOOTSTRAP 
      hist(residuals_hw$.resid, main = "Histogram of residuals", xlab = "Residuals")
      qqnorm(residuals_hw$.resid)
      qqline(residuals_hw$.resid)
      
    }
# We already saw for the two models that we should also use bootstrap
    
#BOOTSTRAP PREDICTION INTERVAL 
{
    # Parameters
    B <- 1000       
    h <- 72        
    set.seed(123)
    lower_ses_boot <- upper_ses_boot <- matrix(NA, nrow = n_forecasts, ncol = h)
    lower_HWadd_boot <- upper_HWadd_boot <- matrix(NA, n_forecasts, h)
    lower_LM_boot        <- upper_LM_boot        <- matrix(NA_real_, nrow = n_forecasts, ncol = h)
    lower_LM_SARIMA_boot <- upper_LM_SARIMA_boot <- matrix(NA_real_, nrow = n_forecasts, ncol = h)
    
    
    for (i in 1:n_forecasts) {
      message("Step ", i, "/", n_forecasts)
      
      # 1. Training window
      train_window <- tsbl_hourly %>%
        slice(i:(i + window_size - 1))
      future_window <- tsbl_hourly %>% slice((i + window_size):(i + window_size + h - 1))
      
      ################### Fit SES (only to obtain residuals)
      fit_ets <- train_window %>%
        model(
          SES   = ETS(traffic_volume ~ error("A") + trend("N") + season("N")),
          HWadd = ETS(traffic_volume ~ error("A") + trend("N") + season("A"))
        )
      # Extract residuals SES and HW
      resid_tbl <- residuals(fit_ets)   
      resid_ses <- resid_tbl %>% filter(.model == "SES")   %>% pull(.resid)
      resid_hw  <- resid_tbl %>% filter(.model == "HWadd") %>% pull(.resid)
      
      ################## Fit LM
      lm_model <- lm(traffic_volume ~ Hour * isWeekend - 1, data = as.data.frame(train_window))
      X_future <- model.matrix(lm_model, data = as.data.frame(future_window))
      beta_hat <- coef(lm_model)
      pred_lm_i <- as.numeric(X_future %*% beta_hat)  
     
      # Extract residuals
      resid_lm <- residuals(lm_model)
      
      ################## Fit SARIMA 
      #Estimate SARIMA on LM residuals of the window:
      sarima_fit <- Arima(resid_lm,
                          order    = c(2, 0, 2),
                          seasonal = list(order = c(1, 0, 1), period = 24))

# --- Bootstrap SES (B × h)
boot_ses <- matrix(NA, nrow = B, ncol = h)
for (b in 1:B) {
  sampled <- sample(resid_ses, h, replace = TRUE)
  boot_matrix[b, ] <- point_ses[i, ] + sampled  
}
lower_ses_boot[i, ] <- apply(boot_ses, 2, quantile, 0.025)
upper_ses_boot[i, ] <- apply(boot_ses, 2, quantile, 0.975)

      # Bootstrap HW
boot_hw <- matrix(NA_real_, nrow = B, ncol = h)
      for (b in 1:B) {
        sampled <- sample(resid_hw, h, replace = TRUE)
        boot_matrix[b, ] <- point_hwadd[i, ] + sampled  
      }
lower_HWadd_boot[i, ] <- apply(boot_hw, 2, quantile, 0.025)
upper_HWadd_boot[i, ] <- apply(boot_hw, 2, quantile, 0.975)

      
      # Bootstrap LM 
boot_lm <- matrix(NA_real_, nrow = B, ncol = h)
      for (b in 1:B) boot_lm[b, ] <- pred_lm_i + sample(resid_lm, size = h, replace = TRUE)
lower_LM_boot[i, ] <- apply(boot_lm, 2, quantile, 0.025)
upper_LM_boot[i, ] <- apply(boot_lm, 2, quantile, 0.975)

      # Bootstrap LM+SARIMA 
boot_comb <- matrix(NA_real_, nrow = B, ncol = h)
      for (b in 1:B) {
        sarima_sim <- simulate(sarima_fit, nsim = h)
        boot_comb[b, ] <- pred_lm_i + as.numeric(sarima_sim) }
      lower_LM_SARIMA_boot[i, ] <- apply(boot_comb, 2, quantile, probs = 0.025)
      upper_LM_SARIMA_boot[i, ] <- apply(boot_comb, 2, quantile, probs = 0.975)
    
    }
    
}

  
## PLOT SES, HW, LM and SARIMAX with BOOTSTRAP PREDICTION INTERVAL in the rolling window 
{
# Parameters 
j <- 72
n_plot <- 100
end_index <- n_forecasts
start_index <- max(1, end_index - n_plot + 1)

# Observed series in the test window
y_true <- test_tsbl_2$traffic_volume

## SES 
df_ses_boot <- data.frame(
  time     = test_tsbl_2$YearHour[(start_index + j - 1):(end_index + j - 1)],
  forecast = point_ses[start_index:end_index, j],
  lower    = lower_ses_boot[start_index:end_index, j],
  upper    = upper_ses_boot[start_index:end_index, j],
  actual   = y_true[(start_index + j - 1):(end_index + j - 1)]
)
ggplot(df_ses_boot, aes(x = time)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  geom_line(aes(y = actual), color = "black", size = 0.8) +
  geom_line(aes(y = forecast), color = "blue", size = 1) +
  labs(title = paste0("SES – Bootstrap PI | Step-ahead ", j, " | Ultime ", n_plot, " rolling"),
       x = "Time", y = "Traffic Volume",
       caption = "Black = observed, blue = forecast, area = bootstrap 95% PI") +
  theme_minimal()

## Additive HW 
df_hw_boot <- data.frame(
  time     = test_tsbl_2$YearHour[(start_index + j - 1):(end_index + j - 1)],
  forecast = point_hwadd[start_index:end_index, j],
  lower    = lower_HWadd_boot[start_index:end_index, j],
  upper    = upper_HWadd_boot[start_index:end_index, j],
  actual   = y_true[(start_index + j - 1):(end_index + j - 1)]
)
ggplot(df_hw_boot, aes(x = time)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightgreen", alpha = 0.3) +
  geom_line(aes(y = actual), color = "black", size = 0.8) +
  geom_line(aes(y = forecast), color = "darkgreen", size = 1) +
  labs(title = paste0("Additive Holt-Winters – Bootstrap PI | Step-ahead ", j, " | Last ", n_plot, " Rolling"),
       x = "Time", y = "Traffic Volume",
       caption = "Black = observed, green = forecast, area = bootstrap 95% PI") +
  theme_minimal()

##  LM 
df_lm_boot <- data.frame(
  time     = test_tsbl_2$YearHour[(start_index + j - 1):(end_index + j - 1)],
  forecast = point_forecast_lm[start_index:end_index, j],
  lower    = lower_LM_boot[start_index:end_index, j],
  upper    = upper_LM_boot[start_index:end_index, j],
  actual   = y_true[(start_index + j - 1):(end_index + j - 1)]
)
ggplot(df_lm_boot, aes(x = time)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "orange", alpha = 0.3) +
  geom_line(aes(y = actual), color = "black", size = 0.8) +
  geom_line(aes(y = forecast), color = "red3", size = 1) +
  labs(title = paste0("Linear Regression – Bootstrap PI | Step-ahead ", j, " | Last ", n_plot, " Rolling"),
       x = "Time", y = "Traffic Volume",
       caption = "Black = observed, red = forecast, area = bootstrap 95% PI") +
  theme_minimal()

## LM + SARIMA
df_comb_boot <- data.frame(
  time     = test_tsbl_2$YearHour[(start_index + j - 1):(end_index + j - 1)],
  forecast = point_forecast_LM_SARIMA[start_index:end_index, j],
  lower    = lower_LM_SARIMA_boot[start_index:end_index, j],
  upper    = upper_LM_SARIMA_boot[start_index:end_index, j],
  actual   = y_true[(start_index + j - 1):(end_index + j - 1)]
)
ggplot(df_comb_boot, aes(x = time)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "violet", alpha = 0.25) +
  geom_line(aes(y = actual), color = "black", size = 0.8) +
  geom_line(aes(y = forecast), color = "purple4", size = 1) +
  labs(title = paste0("LM + SARIMA (residuals) – Bootstrap PI | Step-ahead ", j, " | Last ", n_plot, " Rolling"),
       x = "Time", y = "Traffic Volume",
       caption = "Black = observed, purple = forecast, area = bootstrap 95% PI") +
  theme_minimal()
}


######### EVALUATION

# POINT FORECAST EVALUATION
  #MSE+DM
{
  # Define a function to compute Mean Squared Error
  mse <- function(x, y) {
    out<- mean((x - y)^2) 
    return(out)
  }
  
  mse_ses <- mse_hwadd <- dm.pv<- numeric(h)  # Preallocate numeric vectors of length h for SES MSE, HWadd MSE, and DM p-values
  mse_linreg <- mse_Sarimax <- dm_pvalues <- numeric(h) # Preallocate MSE vectors for Linear Regression and LM+SARIMA; dm_pvalues

  # Compute per-horizon MSE for each model across rolling windows
  # MSE 
  for (j in 1:h) {
    mse_ses[j] <- mse(point_ses[, j], y_true[j:(n_forecasts + j - 1)]) # MSE at horizon j for SES
    mse_hwadd[j] <- mse(point_hwadd[, j], y_true[j:(n_forecasts + j - 1)])  # MSE at horizon j for HW additive
    mse_linreg[j] <- mse(point_forecast_lm[, j], y_true[j:(n_forecasts + j - 1)]) # MSE at horizon j for Linear Regression
    mse_Sarimax[j] <- mse(point_forecast_LM_SARIMA[, j], y_true[j:(n_forecasts + j - 1)]) # MSE at horizon j for Sarimax
    
   #  Average MSE by model
avg_mse <- c(
  SES        = mean(mse_ses,     na.rm = TRUE), # Average (over j) MSE for SES
  HWadd      = mean(mse_hwadd,   na.rm = TRUE), # Average (over j) MSE for HW additive
  LM         = mean(mse_linreg,  na.rm = TRUE), # Average (over j) MSE for Linear Regression
  LM_SARIMA  = mean(mse_Sarimax, na.rm = TRUE) # Average (over j) MSE for LM+SARIMA
) 

avg_mse <- sort(avg_mse)  # Sort ascending: the best (lowest average MSE) is first

best1 <- names(avg_mse)[1] # Name of the best model by average MSE:
best2 <- names(avg_mse)[2] # Name of the second-best model by average MSE:


print(avg_mse[1:2])  # Print the two best average MSE values


# Compute Diebold–Mariano tests on point-forecast errors
    e.ses  <- y_true[j:(n_forecasts+j-1)] - point_ses[,j] # Errors at horizon j for SES
    e.hw   <- y_true[j:(n_forecasts+j-1)] - point_hwadd[,j] # Errors at horizon j for HW additive
    e_linreg <- y_true[j:(n_forecasts + j - 1)] - point_forecast_lm[, j]  # Errors at horizon j for Linear Regression
    e_Sarimax<-  y_true[j:(n_forecasts + j - 1)] - point_forecast_LM_SARIMA[, j] # Errors at horizon j for LM+SARIMA
    
    res <-  dm.test(e_linreg, e_Sarimax, alternative = "two.sided", h = j, power = 2)
      dm_stat[j] <- as.numeric(res$statistic)
      dm_pval[j] <- res$p.value
    }
    
  }
  # To recap:
  # positive DM statistics means favoring alternative two (e_Sarimax)
  # negative DM statistics means  favoring alternative one (e_linreg) 

  {
  alpha <- 0.05
  
  dm_stat  <- rep(NA_real_, h)
  dm_pval  <- rep(NA_real_, h)
  dm_sig   <- rep(NA, h)                 # TRUE if p < alpha
  pref_DM  <- rep(NA_character_, h)      # preference based on DM (only if significative)
  
  # MSE comparison for horizon j
  mse_lm   <- rep(NA_real_, h)
  mse_smx  <- rep(NA_real_, h)
  pref_MSE <- rep(NA_character_, h)
  
  for (j in 1:h) {
    # Errors at horizon j (rows = rolling windows)
    yj <- y_true[j:(n_forecasts + j - 1)]
    e_lm  <- yj - point_forecast_lm[, j]
    e_smx <- yj - point_forecast_LM_SARIMA[, j]
    
    # DM test on MSE (power = 2), lag = j (multi-step)
    res <- dm.test(e_lm, e_smx, alternative = "two.sided", h = j, power = 2)
                   
    if (!is.null(res)) {
      dm_stat[j] <- as.numeric(res$statistic)
      dm_pval[j] <- res$p.value
      dm_sig[j]  <- dm_pval[j] < alpha
      
      # Mark: DM < 0 favours the 1st (LM), DM > 0 favours the 2nd (LM+SARIMA)
      if (isTRUE(dm_sig[j])) {
        pref_DM[j] <- if (dm_stat[j] < 0) "LM" else if (dm_stat[j] > 0) "LM_SARIMA" else "Tie"
      } else {
        pref_DM[j] <- "No significant diff"
      }
    }
    
    # Best for average MSE at horizon j
    mse_lm[j]  <- mean(e_lm^2,  na.rm = TRUE)
    mse_smx[j] <- mean(e_smx^2, na.rm = TRUE)
    pref_MSE[j] <- if (mse_lm[j] < mse_smx[j]) "LM" else if (mse_lm[j] > mse_smx[j]) "LM_SARIMAX" else "Tie"
  }
  
  # ---- Recap ----
  n_sig     <- sum(dm_sig, na.rm = TRUE)
  share_sig <- n_sig / h
  n_lm_sig  <- sum(dm_sig & dm_stat < 0, na.rm = TRUE)
  n_smx_sig <- sum(dm_sig & dm_stat > 0, na.rm = TRUE)
  n_tie_sig <- sum(dm_sig & dm_stat == 0, na.rm = TRUE)
  
  cat(sprintf("Significant differences: %d / %d (%.1f%%)\n", n_sig, h, 100*share_sig))
  cat(sprintf("Among significant: LM wins %d, LM+SARIMAX wins %d, ties %d\n", n_lm_sig, n_smx_sig, n_tie_sig))
  
  # ---- Detailed table for each horizon ----
  dm_summary <- data.frame(
    StepAhead        = 1:h,
    DM_stat          = dm_stat,
    DM_pvalue        = dm_pval,
    Significant      = dm_sig,
    Preferred_by_DM  = pref_DM,
    MSE_LM           = mse_lm,
    MSE_LM_SARIMAX   = mse_smx,
    Preferred_by_MSE = pref_MSE,
    stringsAsFactors = FALSE
  )
  
  dm_summary 
}
  

# PREDICTION INTERVAL EVALUATION
{
  # Parameters and preallocations
  alpha <- 0.05
  uc_ses_boot <- uc_hw_boot <- uc_linreg <- uc_sarimax <- numeric(h)
  uc_pv_ses_boot <- uc_pv_hw_boot <- uc_pv_linreg <- uc_pv_sarimax <- numeric(h)
  
  winkler_ses_boot <- winkler_hwadd_boot <- winkler_linreg <- winkler_sarimax <- numeric(h)
  dm_winkler_pvalue <- dm_winkler_stat <- rep(NA_real_, h)  # DM on Winkler (LM vs SARIMAX), per horizon
  
  # Coverage function
  uc <- function(l,u,y){
    covered <- (y >= l) & (y <= u)
    mean(covered)
  }
  
  # Winkler loss function
  
  winkler <- function(l, u, y, alpha) {
    # l: vector of lower bounds
    # u: vector of upper bounds
    # y: vector of true values
    # alpha: significance level (e.g., 0.05 for 95% interval)
    n <- length(y)
    score <- numeric(n)
    for (i in 1:n) {
      range <- u[i] - l[i]
      if (y[i] < l[i]) {
        score[i] <- range + (2 / alpha) * (l[i] - y[i])
      } else if (y[i] > u[i]) {
        score[i] <- range + (2 / alpha) * (y[i] - u[i])
      } else {
        score[i] <- range
      }
    }
    return(score)  
  }
  
  for (j in 1:h) {
    # align observations for horizon j
    y_j <- y_true[j:(n_forecasts + j - 1)]
    # Unconditional coverage
    uc_ses_boot[j]  <- uc(lower_ses_boot[, j],       upper_ses_boot[, j],       y_j)
    uc_hw_boot[j]   <- uc(lower_HWadd_boot[, j],     upper_HWadd_boot[, j],     y_j)
    uc_linreg[j]    <- uc(lower_LM_boot[, j],        upper_LM_boot[, j],        y_j)
    uc_sarimax[j]   <- uc(lower_LM_SARIMA_boot[, j], upper_LM_SARIMA_boot[, j], y_j)
    # Coverage tests (target = 1 - alpha)
    uc_pv_ses_boot[j]  <- binom.test(round(uc_ses_boot[j]*n_forecasts),  n_forecasts, p = 1 - alpha)$p.value
    uc_pv_hw_boot[j]   <- binom.test(round(uc_hw_boot[j]*n_forecasts),   n_forecasts, p = 1 - alpha)$p.value
    uc_pv_linreg[j]    <- binom.test(round(uc_linreg[j]*n_forecasts),    n_forecasts, p = 1 - alpha)$p.value
    uc_pv_sarimax[j]   <- binom.test(round(uc_sarimax[j]*n_forecasts),   n_forecasts, p = 1 - alpha)$p.value
    # Winkler loss series (length n_forecasts) for horizon j
    wl_ses   <- winkler(lower_ses_boot[, j],       upper_ses_boot[, j],       y_j, alpha)
    wl_hwadd <- winkler(lower_HWadd_boot[, j],     upper_HWadd_boot[, j],     y_j, alpha)
    wl_lm    <- winkler(lower_LM_boot[, j],        upper_LM_boot[, j],        y_j, alpha)
    wl_smx   <- winkler(lower_LM_SARIMA_boot[, j], upper_LM_SARIMA_boot[, j], y_j, alpha)
    # Mean Winkler per horizon (summary)
    winkler_ses_boot[j]   <- mean(wl_ses)
    winkler_hwadd_boot[j] <- mean(wl_hwadd)
    winkler_linreg[j]     <- mean(wl_lm)
    winkler_sarimax[j]    <- mean(wl_smx)
    # DM test on Winkler loss (LM vs SARIMAX), power=1, lag=j
    res <- dm.test(wl_lm, wl_smx, alternative = "two.sided", h = j, power = 1)
    dm_winkler_pvalue[j] <- res$p.value
    dm_winkler_stat[j]   <- as.numeric(res$statistic)
  }
  
  
  # Summary table
  df_eval <- data.frame(
    StepAhead = 1:h,
    UC_LinReg = uc_linreg,
    UC_SARIMAX = uc_sarimax,
    UC_pvalue_LinReg = uc_pv_linreg,
    UC_pvalue_SARIMAX = uc_pv_sarimax,
    Winkler_LinReg = winkler_linreg,
    Winkler_SARIMAX = winkler_sarimax,
    DM_Winkler_stat = dm_winkler_stat,
    DM_Winkler_pvalue = dm_winkler_pvalue
  )
  
  head(df_eval, 10)

  }
