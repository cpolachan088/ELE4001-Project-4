library(forecast) # Load necessary libraries
library(tidyverse) 
library(Metrics) 
library(lubridate)
library(knitr)
library(tseries) 
library(imputeTS) 

# Set dataset name and get dataset-specific parameters
dataset_name <- "cooking"  

# Set working directory for output
setwd("C:/Users/Christo/Desktop/FYP/R Code/results")
training_set <- paste0("ARIMAX_", dataset_name)
plots_dir <- training_set

# Read and preprocess data
fileName <- paste0("C:/Users/Christo/Desktop/FYP/R Code/DATASETS/", dataset_name,".csv")
pollutionData <- read.csv(fileName, header = TRUE)

# Dataset-specific parameters for each scenario
# These parameters are optimized for each dataset based on their characteristics:
#   - aggregation_minutes: temporal resolution options
#   - forecast_periods: prediction horizons in minutes
#   - train_split: proportion of data used for training
#   - validation_points: number of validation chunks
#   - max_ar/ma/diff: ARIMA model constraints

aggregation_minutes_list <- c(5,15,30,60,120)  
forecast_periods <- c(
  60,
  60*2,  
  60*3,  
  60*6,  
  60*12, 
  60*24,  
  60*36,
  60*48,
  60*72
)

dataset_params <- list(
  "long_term" = list(
    aggregation_minutes = aggregation_minutes_list,
    forecast_periods = forecast_periods,
    train_split = 0.7,
    validation_points = 5,
    max_ar = 3,
    max_ma = 2,
    max_diff = 1
  ),
  "short_term" = list(
    aggregation_minutes = aggregation_minutes_list,
    forecast_periods = forecast_periods,
    train_split = 0.7,
    validation_points = 4,
    max_ar = 2,
    max_ma = 2,
    max_diff = 1
  ),
  "candle" = list(
    aggregation_minutes = c( 1, 5, 15), 
    forecast_periods = c(  60,90,120),  
    train_split = 0.8,
    validation_points = 3,
    max_ar = 1,
    max_ma = 1,
    max_diff = 0
  ),
  "vaping" = list(
    aggregation_minutes = c( 1, 5, 15), 
    forecast_periods = c(30, 45, 60),  
    train_split = 0.7,
    validation_points = 1,
    max_ar = 1,
    max_ma = 1,
    max_diff = 0
  ),
  "cooking" = list(
    aggregation_minutes = c( 1, 5, 15), 
    forecast_periods = c(30, 45, 60),  
    train_split = 0.7,
    validation_points = 2,
    max_ar = 1,
    max_ma = 0,
    max_diff = 0
  )
)

# Define relevant global parameters

current_params <- dataset_params[[dataset_name]]
aggregation_minutes_list <- current_params$aggregation_minutes
forecast_periods <- current_params$forecast_periods
train_split <- current_params$train_split
validation_points <- current_params$validation_points
max_ar <- current_params$max_ar
max_ma <- current_params$max_ma
max_diff <- current_params$max_diff
all_results <- data.frame()
final_results_preprocessed_list <- list()
final_results_unprocessed_list <- list()

# Define AQI Bands for PM2.5

max_pm25 <- max(pollutionData$pm25, na.rm = TRUE)
aqi_bands <- data.frame(
  category = c("Excellent", "Good", "Moderate", "Unhealthy for Sensitive Groups", "Unhealthy", "Hazardous"),
  lower = c(0, 5, 15, 25, 35.5, 55.5),
  upper = c(5, 15, 25, 35.5, 55.5, max(75, ceiling(max_pm25 * 1.1))),
  color = c("#00E400", "#92D050", "#FFFF00", "#FF7E00", "#FF0000", "#8F3F97")
)

# Implementation of ARIMAX model with comprehensive preprocessing
# This function trains and evaluates ARIMAX models for a specified forecast horizon
# Uses expanding window approach for robust validation
# Parameters:
#   forecast_minutes: target prediction horizon in minutes
#   aggregationMinutes: temporal resolution of the data
# Returns: list of performance metrics and final model results

run_arimax_preprocessed <- function(forecast_minutes, aggregationMinutes) {
  forecast_periods <- ceiling(forecast_minutes / aggregationMinutes)
  pm25Length <- length(tsPM25_preprocessed)
  initial_train_size <- floor(pm25Length * train_split)
  step_size <- floor((pm25Length - initial_train_size) / validation_points)
  mae_train_values <- c()
  mae_test_values <- c()
  rmse_train_values <- c()
  rmse_test_values <- c()
  mape_train_values <- c()
  mape_test_values <- c()
  r_squared_values <- c()
  evs_values <- c()
  ljung_box_p_values <- c()
  aic_values <- c()
  bic_values <- c()
  theils_u_values <- c()
  forecast_bias_values <- c()
  xreg_cols <- colnames(modelPredictors_preprocessed)[-1]
  for (i in seq(initial_train_size, pm25Length - forecast_periods, step_size)) {
    train_xreg <- as.matrix(modelPredictors_preprocessed[1:i, xreg_cols])
    train_y <- tsPM25_preprocessed[1:i]
    tryCatch({
      if(forecast_minutes <= 180) {
        model <- auto.arima(train_y,
                            xreg = train_xreg,
                            d = max_diff,
                            max.p = max_ar,
                            max.q = max_ma,
                            approximation = TRUE,
                            stepwise = TRUE,
                            trace = FALSE)
      } else {
        model <- auto.arima(train_y,
                            xreg = train_xreg,
                            d = max_diff,
                            max.p = max_ar,
                            max.q = max_ma,
                            approximation = TRUE,
                            stepwise = TRUE,
                            trace = FALSE)
      }
      train_fitted <- fitted(model)
      train_residuals <- residuals(model)
      test_start <- i + 1
      test_end <- min(i + forecast_periods, length(tsPM25_preprocessed))
      actual_test <- tsPM25_preprocessed[test_start:test_end]
      test_xreg <- as.matrix(modelPredictors_preprocessed[test_start:test_end, xreg_cols])
      forecasted <- forecast(model, xreg = test_xreg)
      forecast_test <- forecasted$mean
      forecast_test <- pmax(0, forecast_test)  
      forecast_test <- pmax(0, forecast_test)  
      naive_forecast <- rep(NA, length(actual_test))
      naive_forecast[1] <- tail(train_y, 1)
      if(length(naive_forecast) > 1) {
        for(j in 2:length(naive_forecast)) {
          naive_forecast[j] <- actual_test[j-1]
        }
      }
      min_length <- min(length(actual_test), length(forecast_test))
      actual_test <- actual_test[1:min_length]
      forecast_test <- forecast_test[1:min_length]
      naive_forecast <- naive_forecast[1:min_length]
      mae_train_val <- mae(train_y, train_fitted)
      mae_test_val <- mae(actual_test, forecast_test)
      rmse_train_val <- rmse(train_y, train_fitted)
      rmse_test_val <- rmse(actual_test, forecast_test)
      safe_mape <- function(actual, predicted) {
        idx <- which(actual > 0)
        if (length(idx) == 0) return(NA)
        mean(abs((actual[idx] - predicted[idx]) / actual[idx]) * 100, na.rm = TRUE)
      }
      mape_train_val <- safe_mape(train_y, train_fitted)
      mape_test_val <- safe_mape(actual_test, forecast_test)
      tss <- sum((actual_test - mean(actual_test))^2)
      rss <- sum((actual_test - forecast_test)^2)
      if(tss > 0) {
        r_squared_val <- 1 - (rss / tss)
        r_squared_val <- min(max(r_squared_val, -1), 1)
      } else {
        r_squared_val <- NA
      }
      var_actual <- var(actual_test, na.rm = TRUE)
      var_residuals <- var(actual_test - forecast_test, na.rm = TRUE)
      if(var_actual > 0) {
        evs_val <- 1 - var_residuals / var_actual
        evs_val <- min(max(evs_val, 0), 1)
      } else {
        evs_val <- NA
      }
      lb_test <- Box.test(train_residuals, lag = min(20, length(train_residuals)/5), 
                          type = "Ljung-Box")
      lb_p_val <- lb_test$p.value
      naive_rmse_val <- rmse(actual_test, naive_forecast)
      if(!is.na(naive_rmse_val) && naive_rmse_val > 0) {
        theils_u_val <- rmse_test_val / naive_rmse_val
      } else {
        theils_u_val <- NA
      }
      mean_actual <- mean(actual_test, na.rm = TRUE)
      if(!is.na(mean_actual) && mean_actual != 0) {
        bias_val <- mean(forecast_test - actual_test, na.rm = TRUE) / mean_actual * 100
      } else {
        bias_val <- NA
      }
      mae_train_values <- c(mae_train_values, mae_train_val)
      mae_test_values <- c(mae_test_values, mae_test_val)
      rmse_train_values <- c(rmse_train_values, rmse_train_val)
      rmse_test_values <- c(rmse_test_values, rmse_test_val)
      mape_train_values <- c(mape_train_values, mape_train_val)
      mape_test_values <- c(mape_test_values, mape_test_val)
      r_squared_values <- c(r_squared_values, r_squared_val)
      evs_values <- c(evs_values, evs_val)
      ljung_box_p_values <- c(ljung_box_p_values, lb_p_val)
      aic_values <- c(aic_values, model$aic)
      bic_values <- c(bic_values, model$bic)
      theils_u_values <- c(theils_u_values, theils_u_val)
      forecast_bias_values <- c(forecast_bias_values, bias_val)
      if (i >= (pm25Length - forecast_periods - step_size)) {
        final_results <- list(
          actual = actual_test,
          forecast = forecast_test,
          lower = if(is.matrix(forecasted$lower)) forecasted$lower[,2] else forecasted$lower,
          upper = if(is.matrix(forecasted$upper)) forecasted$upper[,2] else forecasted$upper,
          model = model,
          residuals = train_residuals
        )
      }
    }, error = function(e) {
      cat("Error in modeling chunk", i, "for", forecast_minutes, "minute forecast:", 
          as.character(e), "\n")
    })
  }
  safe_mean <- function(x) {
    if(length(x) == 0 || all(is.na(x))) return(NA)
    mean(x, na.rm = TRUE)
  }
  return(list(
    mae_train = safe_mean(mae_train_values),
    mae_test = safe_mean(mae_test_values),
    rmse_train = safe_mean(rmse_train_values),
    rmse_test = safe_mean(rmse_test_values),
    mape_train = safe_mean(mape_train_values),
    mape_test = safe_mean(mape_test_values),
    r_squared = safe_mean(r_squared_values),
    evs = safe_mean(evs_values),
    ljung_box_p = safe_mean(ljung_box_p_values),
    aic = safe_mean(aic_values),
    bic = safe_mean(bic_values),
    theils_u = safe_mean(theils_u_values),
    forecast_bias = safe_mean(forecast_bias_values),
    final_results = if(exists("final_results")) final_results else NULL
  ))
}

# Implementation of ARIMAX model with minimal preprocessing
# This simplified version uses basic imputation and avoids feature engineering
# Used as comparison baseline to quantify preprocessing benefits
# Follows identical evaluation approach as the preprocessed version

run_arimax_unprocessed <- function(forecast_minutes, aggregationMinutes) {
  forecast_periods <- ceiling(forecast_minutes / aggregationMinutes)
  pm25Length <- length(tsPM25_unprocessed)
  initial_train_size <- floor(pm25Length * train_split)
  step_size <- floor((pm25Length - initial_train_size) / 5)
  mae_train_values <- c()
  mae_test_values <- c()
  rmse_train_values <- c()
  rmse_test_values <- c()
  mape_train_values <- c()
  mape_test_values <- c()
  r_squared_values <- c()
  evs_values <- c()
  ljung_box_p_values <- c()
  aic_values <- c()
  bic_values <- c()
  theils_u_values <- c()
  forecast_bias_values <- c()
  xreg_cols <- colnames(modelPredictors_unprocessed)[-1]
  for (i in seq(initial_train_size, pm25Length - forecast_periods, step_size)) {
    train_xreg <- as.matrix(modelPredictors_unprocessed[1:i, xreg_cols])
    train_y <- tsPM25_unprocessed[1:i]
    tryCatch({
      model <- auto.arima(train_y,
                          xreg = train_xreg,
                          d = 1,
                          max.p = 2,
                          max.q = 2,
                          approximation = TRUE,
                          stepwise = TRUE,
                          trace = FALSE)
      train_fitted <- fitted(model)
      train_residuals <- residuals(model)
      test_start <- i + 1
      test_end <- min(i + forecast_periods, length(tsPM25_unprocessed))
      actual_test <- tsPM25_unprocessed[test_start:test_end]
      test_xreg <- as.matrix(modelPredictors_unprocessed[test_start:test_end, xreg_cols])
      forecasted <- forecast(model, xreg = test_xreg)
      forecast_test <- forecasted$mean
      forecast_test <- pmax(0, forecast_test)  
      forecasted$lower <- pmax(0, forecasted$lower)
      naive_forecast <- rep(NA, length(actual_test))
      naive_forecast[1] <- tail(train_y, 1)
      if(length(naive_forecast) > 1) {
        for(j in 2:length(naive_forecast)) {
          naive_forecast[j] <- actual_test[j-1]
        }
      }
      min_length <- min(length(actual_test), length(forecast_test))
      actual_test <- actual_test[1:min_length]
      forecast_test <- forecast_test[1:min_length]
      naive_forecast <- naive_forecast[1:min_length]
      mae_train_val <- mae(train_y, train_fitted)
      mae_test_val <- mae(actual_test, forecast_test)
      rmse_train_val <- rmse(train_y, train_fitted)
      rmse_test_val <- rmse(actual_test, forecast_test)
      safe_mape <- function(actual, predicted) {
        idx <- which(actual > 0)
        if (length(idx) == 0) return(NA)
        mean(abs((actual[idx] - predicted[idx]) / actual[idx]) * 100, na.rm = TRUE)
      }
      mape_train_val <- safe_mape(train_y, train_fitted)
      mape_test_val <- safe_mape(actual_test, forecast_test)
      tss <- sum((actual_test - mean(actual_test))^2)
      rss <- sum((actual_test - forecast_test)^2)
      if(tss > 0) {
        r_squared_val <- 1 - (rss / tss)
        r_squared_val <- min(max(r_squared_val, -1), 1)
      } else {
        r_squared_val <- NA
      }
      var_actual <- var(actual_test, na.rm = TRUE)
      var_residuals <- var(actual_test - forecast_test, na.rm = TRUE)
      if(var_actual > 0) {
        evs_val <- 1 - var_residuals / var_actual
        evs_val <- min(max(evs_val, 0), 1)
      } else {
        evs_val <- NA
      }
      lb_test <- Box.test(train_residuals, lag = min(20, length(train_residuals)/5), 
                          type = "Ljung-Box")
      lb_p_val <- lb_test$p.value
      naive_rmse_val <- rmse(actual_test, naive_forecast)
      if(!is.na(naive_rmse_val) && naive_rmse_val > 0) {
        theils_u_val <- rmse_test_val / naive_rmse_val
      } else {
        theils_u_val <- NA
      }
      mean_actual <- mean(actual_test, na.rm = TRUE)
      if(!is.na(mean_actual) && mean_actual != 0) {
        bias_val <- mean(forecast_test - actual_test, na.rm = TRUE) / mean_actual * 100
      } else {
        bias_val <- NA
      }
      mae_train_values <- c(mae_train_values, mae_train_val)
      mae_test_values <- c(mae_test_values, mae_test_val)
      rmse_train_values <- c(rmse_train_values, rmse_train_val)
      rmse_test_values <- c(rmse_test_values, rmse_test_val)
      mape_train_values <- c(mape_train_values, mape_train_val)
      mape_test_values <- c(mape_test_values, mape_test_val)
      r_squared_values <- c(r_squared_values, r_squared_val)
      evs_values <- c(evs_values, evs_val)
      ljung_box_p_values <- c(ljung_box_p_values, lb_p_val)
      aic_values <- c(aic_values, model$aic)
      bic_values <- c(bic_values, model$bic)
      theils_u_values <- c(theils_u_values, theils_u_val)
      forecast_bias_values <- c(forecast_bias_values, bias_val)
      if (i >= (pm25Length - forecast_periods - step_size)) {
        final_results <- list(
          actual = actual_test,
          forecast = forecast_test,
          lower = if(is.matrix(forecasted$lower)) forecasted$lower[,2] else forecasted$lower,
          upper = if(is.matrix(forecasted$upper)) forecasted$upper[,2] else forecasted$upper,
          model = model,
          residuals = train_residuals
        )
      }
    }, error = function(e) {
      cat("Error in modeling chunk", i, "for", forecast_minutes, "minute forecast:", 
          as.character(e), "\n")
    })
  }
  safe_mean <- function(x) {
    if(length(x) == 0 || all(is.na(x))) return(NA)
    mean(x, na.rm = TRUE)
  }
  return(list(
    mae_train = safe_mean(mae_train_values),
    mae_test = safe_mean(mae_test_values),
    rmse_train = safe_mean(rmse_train_values),
    rmse_test = safe_mean(rmse_test_values),
    mape_train = safe_mean(mape_train_values),
    mape_test = safe_mean(mape_test_values),
    r_squared = safe_mean(r_squared_values),
    evs = safe_mean(evs_values),
    ljung_box_p = safe_mean(ljung_box_p_values),
    aic = safe_mean(aic_values),
    bic = safe_mean(bic_values),
    theils_u = safe_mean(theils_u_values),
    forecast_bias = safe_mean(forecast_bias_values),
    final_results = if(exists("final_results")) final_results else NULL
  ))
}

# Main execution loop - iterates through each temporal aggregation level
# For each aggregation, trains models with and without preprocessing
# Evaluates performance across multiple forecast horizons
# Generates visualizations for results comparison

for (aggregationMinutes in aggregation_minutes_list) {
  cat("\n\n========== PROCESSING AGGREGATION PERIOD:", aggregationMinutes, "MINUTES ==========\n\n")
  results_preprocessed <- data.frame(
    AggregationMinutes = aggregationMinutes,
    Preprocessing = "Yes",
    Minutes = forecast_periods,
    Hours = forecast_periods / 60,
    MAE_Train = NA,
    MAE_Test = NA,
    RMSE_Train = NA,
    RMSE_Test = NA,
    MAPE_Train = NA,
    MAPE_Test = NA,
    R_Squared = NA,
    EVS = NA,
    Ljung_Box_p = NA,
    AIC = NA,
    BIC = NA,
    Theils_U = NA,
    Forecast_Bias = NA,
    ExecutionTime = NA
  )
  results_unprocessed <- data.frame(
    AggregationMinutes = aggregationMinutes,
    Preprocessing = "No",
    Minutes = forecast_periods,
    Hours = forecast_periods / 60,
    MAE_Train = NA,
    MAE_Test = NA,
    RMSE_Train = NA,
    RMSE_Test = NA,
    MAPE_Train = NA,
    MAPE_Test = NA,
    R_Squared = NA,
    EVS = NA,
    Ljung_Box_p = NA,
    AIC = NA,
    BIC = NA,
    Theils_U = NA,
    Forecast_Bias = NA,
    ExecutionTime = NA
  )
  
  # Comprehensive preprocessing pipeline:
  # 1. Missing value imputation using Kalman filtering
  # 2. Temporal feature engineering (hour, day of week, cyclical transformations)
  # 3. Time series lag features creation (1-period and 24-period lags)
  # 4. Feature standardization for improved model stability
  
  cat("\n===== Running ARIMAX WITH Preprocessing (Aggregation:", aggregationMinutes, "minutes) =====\n")
  start_time_preprocessed <- Sys.time()
  pollutionData_clean <- pollutionData
  pollutionData_clean[-1] <- lapply(pollutionData_clean[-1], function(x) { 
    x <- as.character(x)  
    x[x == "No data"] <- NA  
    return(x) 
  })
  pollutionData_clean[-1] <- lapply(pollutionData_clean[-1], function(x) { 
    suppressWarnings(as.numeric(x)) 
  })
  for(col in names(pollutionData_clean)[-1]) {
    if(any(is.na(pollutionData_clean[[col]]))) {
      pollutionData_clean[[col]] <- na_kalman(pollutionData_clean[[col]])
    }
  }
  pollutionData_clean$ts <- as.POSIXct(pollutionData_clean$ts, format="%d/%m/%Y %H:%M", tz = "GMT")
  pollutionData_clean$period <- as.numeric(difftime(pollutionData_clean$ts, min(pollutionData_clean$ts), units="mins")) %/% aggregationMinutes
  aggregatedData_preprocessed <- pollutionData_clean %>%
    group_by(period) %>%
    summarise(
      ts = first(ts),
      pm25 = mean(pm25),
      pm10 = mean(pm10),
      co2 = mean(co2),
      temperature = mean(temperature),
      humidity = mean(humidity),
      voc = mean(voc)
    )
  aggregatedData_preprocessed <- aggregatedData_preprocessed %>%
    mutate(
      hour = hour(ts),
      day_of_week = wday(ts),
      is_weekend = ifelse(wday(ts) %in% c(1, 7), 1, 0),
      hour_sin = sin(2 * pi * hour / 24),
      hour_cos = cos(2 * pi * hour / 24)
    )
  if(nrow(aggregatedData_preprocessed) > 24*(60/aggregationMinutes)*7) {
    aggregatedData_preprocessed <- aggregatedData_preprocessed %>%
      mutate(
        dow_sin = sin(2 * pi * day_of_week / 7),
        dow_cos = cos(2 * pi * day_of_week / 7)
      )
  }
  aggregatedData_preprocessed <- aggregatedData_preprocessed %>%
    mutate(
      pm25_lag1 = lag(pm25, 1),
      pm25_lag24 = lag(pm25, 24)
    )
  aggregatedData_preprocessed <- aggregatedData_preprocessed %>%
    mutate(across(contains("lag"), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))
  modelPredictors_preprocessed <- data.frame(
    pm25 = aggregatedData_preprocessed$pm25,
    pm10 = aggregatedData_preprocessed$pm10,
    co2 = aggregatedData_preprocessed$co2,
    temperature = aggregatedData_preprocessed$temperature,
    humidity = aggregatedData_preprocessed$humidity,
    hour_sin = aggregatedData_preprocessed$hour_sin,
    hour_cos = aggregatedData_preprocessed$hour_cos,
    is_weekend = aggregatedData_preprocessed$is_weekend,
    pm25_lag1 = aggregatedData_preprocessed$pm25_lag1,
    pm25_lag24 = aggregatedData_preprocessed$pm25_lag24
  )
  modelPredictors_preprocessed[, -1] <- scale(modelPredictors_preprocessed[, -1])
  tsPM25_preprocessed <- ts(modelPredictors_preprocessed$pm25)
  set.seed(123)
  for(i in 1:nrow(results_preprocessed)) {
    cat("Processing forecast period WITH preprocessing:", results_preprocessed$Hours[i], "hours\n")
    model_results <- run_arimax_preprocessed(results_preprocessed$Minutes[i], aggregationMinutes)
    results_preprocessed$MAE_Train[i] <- model_results$mae_train
    results_preprocessed$MAE_Test[i] <- model_results$mae_test
    results_preprocessed$RMSE_Train[i] <- model_results$rmse_train
    results_preprocessed$RMSE_Test[i] <- model_results$rmse_test
    results_preprocessed$MAPE_Train[i] <- model_results$mape_train
    results_preprocessed$MAPE_Test[i] <- model_results$mape_test
    results_preprocessed$R_Squared[i] <- model_results$r_squared
    results_preprocessed$EVS[i] <- model_results$evs
    results_preprocessed$Ljung_Box_p[i] <- model_results$ljung_box_p
    results_preprocessed$AIC[i] <- model_results$aic
    results_preprocessed$BIC[i] <- model_results$bic
    results_preprocessed$Theils_U[i] <- model_results$theils_u
    results_preprocessed$Forecast_Bias[i] <- model_results$forecast_bias
    final_results_preprocessed_list[[as.character(results_preprocessed$Minutes[i])]] <- model_results$final_results
  }
  end_time_preprocessed <- Sys.time()
  execution_time_preprocessed <- end_time_preprocessed - start_time_preprocessed
  results_preprocessed$ExecutionTime <- as.numeric(execution_time_preprocessed, units="secs")
  cat("\nExecution time WITH preprocessing: ", format(execution_time_preprocessed), "\n")
  cat("\n===== Running ARIMAX WITHOUT Preprocessing (Aggregation:", aggregationMinutes, "minutes) =====\n")
  start_time_unprocessed <- Sys.time()
  pollutionData_raw <- pollutionData
  pollutionData_raw[-1] <- lapply(pollutionData_raw[-1], function(x) { 
    x <- as.character(x)  
    x[x == "No data"] <- NA  
    return(x) 
  })
  pollutionData_raw[-1] <- lapply(pollutionData_raw[-1], function(x) { 
    suppressWarnings(as.numeric(x)) 
  })
  for(col in names(pollutionData_raw)[-1]) {
    if(any(is.na(pollutionData_raw[[col]]))) {
      col_mean <- mean(pollutionData_raw[[col]], na.rm = TRUE)
      pollutionData_raw[[col]][is.na(pollutionData_raw[[col]])] <- col_mean
    }
  }
  pollutionData_raw$ts <- as.POSIXct(pollutionData_raw$ts, format="%d/%m/%Y %H:%M", tz = "GMT")
  pollutionData_raw$period <- as.numeric(difftime(pollutionData_raw$ts, min(pollutionData_raw$ts), units="mins")) %/% aggregationMinutes
  aggregatedData_unprocessed <- pollutionData_raw %>%
    group_by(period) %>%
    summarise(
      ts = first(ts),
      pm25 = mean(pm25),
      pm10 = mean(pm10),
      co2 = mean(co2),
      temperature = mean(temperature),
      humidity = mean(humidity),
      voc = mean(voc)
    )
  modelPredictors_unprocessed <- data.frame(
    pm25 = aggregatedData_unprocessed$pm25,
    pm10 = aggregatedData_unprocessed$pm10,
    co2 = aggregatedData_unprocessed$co2,
    temperature = aggregatedData_unprocessed$temperature,
    humidity = aggregatedData_unprocessed$humidity
  )
  tsPM25_unprocessed <- ts(modelPredictors_unprocessed$pm25)
  for(i in 1:nrow(results_unprocessed)) {
    cat("Processing forecast period WITHOUT preprocessing:", results_unprocessed$Hours[i], "hours\n")
    model_results <- run_arimax_unprocessed(results_unprocessed$Minutes[i], aggregationMinutes)
    results_unprocessed$MAE_Train[i] <- model_results$mae_train
    results_unprocessed$MAE_Test[i] <- model_results$mae_test
    results_unprocessed$RMSE_Train[i] <- model_results$rmse_train
    results_unprocessed$RMSE_Test[i] <- model_results$rmse_test
    results_unprocessed$MAPE_Train[i] <- model_results$mape_train
    results_unprocessed$MAPE_Test[i] <- model_results$mape_test
    results_unprocessed$R_Squared[i] <- model_results$r_squared
    results_unprocessed$EVS[i] <- model_results$evs
    results_unprocessed$Ljung_Box_p[i] <- model_results$ljung_box_p
    results_unprocessed$AIC[i] <- model_results$aic
    results_unprocessed$BIC[i] <- model_results$bic
    results_unprocessed$Theils_U[i] <- model_results$theils_u
    results_unprocessed$Forecast_Bias[i] <- model_results$forecast_bias
    final_results_unprocessed_list[[as.character(results_unprocessed$Minutes[i])]] <- model_results$final_results
  }
  end_time_unprocessed <- Sys.time()
  execution_time_unprocessed <- end_time_unprocessed - start_time_unprocessed
  results_unprocessed$ExecutionTime <- as.numeric(execution_time_unprocessed, units="secs")
  cat("\nExecution time WITHOUT preprocessing: ", format(execution_time_unprocessed), "\n")
  all_results <- rbind(all_results, results_preprocessed, results_unprocessed)
  if (!dir.exists(plots_dir)) {
    dir.create(plots_dir, recursive = TRUE)
  }
  
  # Visualization function for model predictions with confidence intervals
  # Includes AQI band overlays for health context interpretation
  # Color-coded to distinguish between actual values and predictions
  # Provides visual comparison of model performance across different configurations
  
  create_forecast_plot <- function(actual, forecast, lower_ci, upper_ci, time_index, aggregationMinutes, 
                                   forecastMinutes, preprocessing_status, aqi_bands) {
    plot_data <- data.frame(
      Index = 1:length(actual),
      Actual = actual,
      Forecast = forecast,
      Lower = lower_ci,
      Upper = upper_ci
    )
    aqi_dummy <- data.frame(
      x = rep(1, nrow(aqi_bands)),
      y = rep(1, nrow(aqi_bands)),
      category = factor(aqi_bands$category, levels = aqi_bands$category)
    )
    aqi_rectangles <- lapply(1:nrow(aqi_bands), function(i) {
      annotate("rect", 
               xmin = -Inf, xmax = Inf, 
               ymin = aqi_bands$lower[i], ymax = aqi_bands$upper[i],
               fill = aqi_bands$color[i], alpha = 0.3)
    })
    p <- ggplot(plot_data, aes(x = Index)) +
      aqi_rectangles +
      geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "grey80", alpha = 0.3) +
      geom_line(aes(y = Actual, color = "Actual"), linewidth = 0.8) +
      geom_line(aes(y = Forecast, color = "Forecast"), linewidth = 0.8) +
      geom_point(data = aqi_dummy, aes(x = x, y = y, fill = category), 
                 shape = 22, size = 7, alpha = 0) +
      scale_y_continuous(name = expression(PM[2.5]~(mu*g/m^3)), 
                         breaks = c(5, 15, 25, 35.5, 55.5, 75)) +
      scale_color_manual(name = "Data Type", values = c(
        "Actual" = "black", 
        "Forecast" = ifelse(preprocessing_status == "Yes", "blue", "orange"))) +
      scale_fill_manual(name = "AQI Category", values = setNames(aqi_bands$color, aqi_bands$category)) +
      labs(
        title = paste0("ARIMAX ", ifelse(preprocessing_status == "Yes", "WITH", "WITHOUT"), 
                       " Preprocessing: Actual vs Forecast"),
        subtitle = paste0("Data aggregated by ", aggregationMinutes, " minutes, Forecasting ", 
                          forecastMinutes/60, " hours ahead"),
        x = paste0("Time (", aggregationMinutes, "-minute intervals)")
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        legend.box = "vertical",  
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(0.2, "cm"),
        legend.spacing.y = unit(0.3, "cm"),  
        legend.key.size = unit(0.8, "lines"),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 9),
        legend.box.just = "center",  
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray90"),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 10)
      ) +
      guides(
        color = guide_legend(order = 1, 
                             ncol = 2, 
                             title.position = "left", 
                             title.vjust = 0.5),
        fill = guide_legend(order = 2, 
                            ncol = 6,  
                            title.position = "left",
                            title.vjust = 0.5,  
                            label.hjust = 0.5,  
                            override.aes = list(
                              alpha = 1,
                              shape = 22,
                              size = 7,
                              color = "black"
                            ))
      )
    return(p)
  }
  
  # Function to generate comparison visualizations between preprocessed and unprocessed models
  # Creates side-by-side comparisons of predictions and performance metrics
  # Helps visualize the impact of preprocessing on forecast quality
  # Saves plots to directory for inclusion in final report
  
  create_comparison_plots <- function() {
    comparison_hours <- forecast_periods / 60
    for (hour in comparison_hours) {
      for (agg_min in aggregation_minutes_list) {
        cat("Creating comparison plot for", hour, "hour forecast with", agg_min, "minute aggregation\n")
        forecast_min <- hour * 60
        pre_results <- final_results_preprocessed_list[[as.character(forecast_min)]]
        unpre_results <- final_results_unprocessed_list[[as.character(forecast_min)]]
        if (is.null(pre_results) || is.null(unpre_results)) {
          cat("Skipping comparison plot due to missing data in results\n")
          next
        }
        if (is.null(pre_results$actual) || is.null(unpre_results$actual) ||
            length(pre_results$actual) == 0 || length(unpre_results$actual) == 0) {
          cat("Skipping comparison plot due to empty results\n")
          next
        }
        min_length <- min(length(pre_results$actual), length(unpre_results$actual))
        if (min_length <= 0) {
          cat("Skipping comparison plot due to insufficient data points\n")
          next
        }
        compare_data <- data.frame(
          Index = 1:min_length,
          Actual = pre_results$actual[1:min_length],
          Preprocessed = pre_results$forecast[1:min_length],
          Unprocessed = unpre_results$forecast[1:min_length]
        )
        aqi_dummy <- data.frame(
          x = rep(1, nrow(aqi_bands)),
          y = rep(1, nrow(aqi_bands)),
          category = factor(aqi_bands$category, levels = aqi_bands$category)
        )
        aqi_rectangles <- lapply(1:nrow(aqi_bands), function(i) {
          annotate("rect",
                   xmin = -Inf, xmax = Inf,
                   ymin = aqi_bands$lower[i], ymax = aqi_bands$upper[i],
                   fill = aqi_bands$color[i], alpha = 0.3)
        })
        compare_plot <- ggplot(compare_data, aes(x = Index)) +
          aqi_rectangles +
          geom_line(aes(y = Actual, color = "Actual"), linewidth = 0.8) +
          geom_line(aes(y = Preprocessed, color = "With Preprocessing"), linewidth = 0.8) +
          geom_line(aes(y = Unprocessed, color = "Without Preprocessing"), linewidth = 0.8) +
          geom_point(data = aqi_dummy,
                     aes(x = x, y = y, fill = category),
                     shape = 22, size = 7, alpha = 0) +
          scale_y_continuous(name = expression(PM[2.5]~(mu*g/m^3)),
                             breaks = c(5, 15, 25, 35.5, 55.5, 75)) +
          scale_color_manual(name = "Data Type", values = c(
            "Actual" = "black",
            "With Preprocessing" = "blue",
            "Without Preprocessing" = "orange")) +
          scale_fill_manual(name = "AQI Category", values = setNames(aqi_bands$color, aqi_bands$category)) +
          labs(
            title = paste0("ARIMAX Comparison: ", hour, "-Hour Forecast"),
            subtitle = paste0("Data aggregated by ", agg_min, " minutes"),
            x = paste0("Time (", agg_min, "-minute intervals)")
          ) +
          theme_minimal() +
          theme(
            legend.position = "bottom",
            legend.box = "vertical",  
            legend.margin = margin(0, 0, 0, 0),
            legend.spacing.x = unit(0.2, "cm"),
            legend.spacing.y = unit(0.3, "cm"),  
            legend.key.size = unit(0.8, "lines"),
            legend.text = element_text(size = 8),
            legend.title = element_text(size = 9),
            legend.box.just = "center",  
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(color = "gray90"),
            plot.title = element_text(size = 14, face = "bold"),
            plot.subtitle = element_text(size = 10)
          ) +
          guides(
            color = guide_legend(order = 1, 
                                 ncol = 3,  
                                 title.position = "left",
                                 title.vjust = 0.5),  
            fill = guide_legend(order = 2,
                                ncol = 6,  
                                title.position = "left",
                                title.vjust = 0.5,  
                                label.hjust = 0.5,  
                                override.aes = list(
                                  alpha = 1,
                                  shape = 22,
                                  size = 7,
                                  color = "black"
                                ))
          )
        plot_filename <- paste0(plots_dir, "/ARIMAX_comparison_",
                                agg_min, "min_", hour, "h_forecast",
                                "_", 
                                format(Sys.time(), "%Y%m%d_%H%M%S"), 
                                ".png")
        ggsave(plot_filename, compare_plot, width = 10, height = 6, dpi = 300)
        print(compare_plot)
        tryCatch({
          pre_row <- all_results %>% 
            filter(AggregationMinutes == agg_min, 
                   Hours == hour, 
                   Preprocessing == "Yes")
          unpre_row <- all_results %>% 
            filter(AggregationMinutes == agg_min, 
                   Hours == hour, 
                   Preprocessing == "No")
          if(nrow(pre_row) == 0 || nrow(unpre_row) == 0) {
            cat("Skipping metrics plot - missing data for", hour, "hour forecast with", agg_min, "minute aggregation\n")
            next
          }
          expanded_metrics_data <- data.frame(
            Metric = rep(c("MAE", "RMSE", "MAPE (%)", "R²"), 2),
            Dataset = c(rep("Test", 4), rep("Train", 4)),
            Preprocessed = c(
              pre_row$MAE_Test[1],
              pre_row$RMSE_Test[1],
              pre_row$MAPE_Test[1],
              pre_row$R_Squared[1],
              pre_row$MAE_Train[1], 
              pre_row$RMSE_Train[1],
              pre_row$MAPE_Train[1],
              pre_row$R_Squared[1]  
            ),
            Unprocessed = c(
              unpre_row$MAE_Test[1],
              unpre_row$RMSE_Test[1],
              unpre_row$MAPE_Test[1],
              unpre_row$R_Squared[1],
              unpre_row$MAE_Train[1],
              unpre_row$RMSE_Train[1],
              unpre_row$MAPE_Train[1],
              unpre_row$R_Squared[1]  
            )
          )
          expanded_metrics_long <- tidyr::pivot_longer(
            expanded_metrics_data, 
            cols = c(Preprocessed, Unprocessed),
            names_to = "Model",
            values_to = "Value"
          )
          expanded_metrics_long$Model_Dataset <- paste(expanded_metrics_long$Model, 
                                                       expanded_metrics_long$Dataset, 
                                                       sep = "_")
          color_palette <- c(
            "Preprocessed_Test" = "red",
            "Unprocessed_Test" = "green",
            "Preprocessed_Train" = "blue",
            "Unprocessed_Train" = "orange"
          )
          expanded_metrics_plot <- ggplot(
            expanded_metrics_long, 
            aes(x = Metric, y = Value, fill = Model_Dataset)
          ) +
            geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
            geom_text(aes(label = sprintf("%.2f", Value)), 
                      position = position_dodge(width = 0.8), 
                      vjust = -0.5, size = 2.5) +
            scale_fill_manual(
              values = color_palette,
              labels = c("Preprocessed (Test)", "Preprocessed (Train)", 
                         "Unprocessed (Test)", "Unprocessed (Train)")
            ) +
            labs(
              title = paste0("ARIMAX Performance Metrics: ", hour, "-Hour Forecast"),
              subtitle = paste0("Data aggregated by ", agg_min, " minutes (Train & Test)"),
              y = "Value",
              x = ""
            ) +
            theme_minimal() +
            theme(
              legend.position = "bottom",
              legend.title = element_blank(),
              panel.grid.minor = element_blank(),
              plot.title = element_text(size = 14, face = "bold"),
              plot.subtitle = element_text(size = 10)
            )
          expanded_metrics_filename <- paste0(plots_dir, "/ARIMAX_expanded_metrics_", 
                                              agg_min, "min_", hour, "h_forecast", "_", 
                                              format(Sys.time(), "%Y%m%d_%H%M%S"),
                                              ".png")
          ggsave(expanded_metrics_filename, expanded_metrics_plot, width = 12, height = 7, dpi = 300)
          print(expanded_metrics_plot)
          metrics_data <- data.frame(
            Metric = c("MAE", "RMSE", "MAPE (%)", "R²"),
            Preprocessed = c(
              pre_row$MAE_Test[1],
              pre_row$RMSE_Test[1],
              pre_row$MAPE_Test[1],
              pre_row$R_Squared[1]
            ),
            Unprocessed = c(
              unpre_row$MAE_Test[1],
              unpre_row$RMSE_Test[1],
              unpre_row$MAPE_Test[1],
              unpre_row$R_Squared[1]
            )
          )
          metrics_long <- tidyr::pivot_longer(
            metrics_data, 
            cols = c(Preprocessed, Unprocessed),
            names_to = "Model",
            values_to = "Value"
          )
          metrics_plot <- ggplot(metrics_long, aes(x = Metric, y = Value, fill = Model)) +
            geom_bar(stat = "identity", position = "dodge", width = 0.7) +
            geom_text(aes(label = sprintf("%.2f", Value)), 
                      position = position_dodge(width = 0.7), 
                      vjust = -0.5, size = 3) +
            scale_fill_manual(values = c("Preprocessed" = "blue", "Unprocessed" = "orange")) +
            labs(
              title = paste0("ARIMAX Test Performance: ", hour, "-Hour Forecast"),
              subtitle = paste0("Data aggregated by ", agg_min, " minutes (Test data only)"),
              y = "Value",
              x = ""
            ) +
            theme_minimal() +
            theme(
              legend.position = "bottom",
              legend.title = element_blank(),
              panel.grid.minor = element_blank(),
              plot.title = element_text(size = 14, face = "bold"),
              plot.subtitle = element_text(size = 10)
            )
          metrics_filename <- paste0(plots_dir, "/ARIMAX_metrics_test_", 
                                     agg_min, "min_", hour, "h_forecast", "_", 
                                     format(Sys.time(), "%Y%m%d_%H%M%S"),".png")
          ggsave(metrics_filename, metrics_plot, width = 10, height = 6, dpi = 300)
          print(metrics_plot)
        }, error = function(e) {
          cat("Error creating metrics plots:", as.character(e), "\n")
          cat("Skipping metrics plots for", hour, "hour forecast with", agg_min, "minute aggregation\n")
        })
      }
    }
  }
  for (aggregationMinutes in aggregation_minutes_list) {
    cat("\n\n========== CREATING VISUALIZATIONS FOR:", aggregationMinutes, "MINUTES ==========\n\n")
    for (forecast_min in forecast_periods) {
      forecast_hrs <- forecast_min / 60
      cat("Creating visualization for aggregation:", aggregationMinutes, 
          "minutes, forecast:", forecast_hrs, "hours\n")
      pre_key <- as.character(forecast_min)
      pre_results <- final_results_preprocessed_list[[pre_key]]
      if (!is.null(pre_results) && !is.null(pre_results$actual) && length(pre_results$actual) > 0) {
        lower_ci <- if (!is.null(pre_results$lower)) pre_results$lower else rep(0, length(pre_results$actual))
        upper_ci <- if (!is.null(pre_results$upper)) pre_results$upper else rep(0, length(pre_results$actual))
        min_length <- min(length(pre_results$actual), length(pre_results$forecast), 
                          length(lower_ci), length(upper_ci))
        pre_plot <- create_forecast_plot(
          pre_results$actual[1:min_length], 
          pre_results$forecast[1:min_length],
          lower_ci[1:min_length],
          upper_ci[1:min_length],
          1:min_length,
          aggregationMinutes, 
          forecast_min, 
          "Yes", 
          aqi_bands
        )
        plot_filename <- paste0(plots_dir, "/ARIMAX_preprocessed_", 
                                aggregationMinutes, "min_", 
                                forecast_hrs, "h_forecast", "_", 
                                format(Sys.time(), "%Y%m%d_%H%M%S"),".png")
        ggsave(plot_filename, pre_plot, width = 10, height = 6, dpi = 300)
        print(pre_plot)
      } else {
        cat("No valid preprocessed results available for", forecast_hrs, "hour forecast\n")
      }
      unpre_key <- as.character(forecast_min)
      unpre_results <- final_results_unprocessed_list[[unpre_key]]
      if (!is.null(unpre_results) && !is.null(unpre_results$actual) && length(unpre_results$actual) > 0) {
        lower_ci <- if (!is.null(unpre_results$lower)) unpre_results$lower else rep(0, length(unpre_results$actual))
        upper_ci <- if (!is.null(unpre_results$upper)) unpre_results$upper else rep(0, length(unpre_results$actual))
        min_length <- min(length(unpre_results$actual), length(unpre_results$forecast), 
                          length(lower_ci), length(upper_ci))
        unpre_plot <- create_forecast_plot(
          unpre_results$actual[1:min_length], 
          unpre_results$forecast[1:min_length],
          lower_ci[1:min_length],
          upper_ci[1:min_length],
          1:min_length,
          aggregationMinutes, 
          forecast_min, 
          "No", 
          aqi_bands
        )
        plot_filename <- paste0(plots_dir, "/ARIMAX_unprocessed_", 
                                aggregationMinutes, "min_", 
                                forecast_hrs, "h_forecast", "_", 
                                format(Sys.time(), "%Y%m%d_%H%M%S"),".png")
        ggsave(plot_filename, unpre_plot, width = 10, height = 6, dpi = 300)
        print(unpre_plot)
      } else {
        cat("No valid unprocessed results available for", forecast_hrs, "hour forecast\n")
      }
    }
  }
  create_comparison_plots()
} 

# Results aggregation and reporting
# Creates summary statistics across all configurations
# Generates standardized performance tables by aggregation level
# Enables identification of optimal model configurations
# Provides quantitative assessment of preprocessing benefits

results_filename <- paste0(training_set, "_", 
                           format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")

write.csv(all_results, results_filename, row.names = FALSE)

cat("\nAll results saved to:", results_filename, "\n")

summary_table <- all_results %>%
  group_by(AggregationMinutes, Preprocessing) %>%
  summarise(
    Avg_MAE_Test = mean(MAE_Test, na.rm = TRUE),
    Avg_RMSE_Test = mean(RMSE_Test, na.rm = TRUE),
    Avg_R_Squared = mean(R_Squared, na.rm = TRUE),
    Avg_ExecutionTime = mean(ExecutionTime, na.rm = TRUE)
  )

cat("\n===== SUMMARY OF RESULTS =====\n")
print(kable(summary_table, format = "markdown"))