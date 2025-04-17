library(forecast) # Load necessary libraries
library(tidyverse) 
library(Metrics) 
library(lubridate)
library(knitr)
library(tseries)
library(imputeTS)
library(keras)
library(tensorflow)
library(reticulate)

tryCatch({
  # Try to use conda environment if available
  use_condaenv("r-tensorflow", required = FALSE)
  # Set random seed for reproducibility
  tensorflow::set_random_seed(123)
}, error = function(e) {
  cat("Warning: TensorFlow/Keras environment not available. Using default R environment.\n")
  cat("You may need to install TensorFlow and Keras first with:\n")
  cat("install.packages('keras')\n")
  cat("keras::install_keras()\n")
})

# Set dataset name and get dataset-specific parameters
dataset_name <- "cooking"  

# Set working directory for output
setwd("C:/Users/Christo/Desktop/FYP/R Code/results")
training_set <- paste0("LSTM_", dataset_name)
plots_dir <- training_set

# Read and preprocess data
fileName <- paste0("C:/Users/Christo/Desktop/FYP/R Code/DATASETS/", dataset_name,".csv")
pollutionData <- read.csv(fileName, header = TRUE)

# Dataset-specific neural network parameters for each scenario
# Tailored configurations optimized for different datasets:
#   - aggregation_minutes: temporal resolution options
#   - forecast_periods: prediction horizons in minutes
#   - train_split: proportion of data used for training
#   - window_size: sequence length for LSTM input
#   - lstm_units/dense_units: neural network architecture parameters
#   - epochs/batch_size/patience: training hyperparameters optimized per dataset

aggregation_minutes_list <- c(15,30,60,120)  
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
    window_size = 24,
    lstm_units = 64,
    dense_units = 32,
    epochs = 50,
    batch_size = 32,
    patience = 10
  ),
  "short_term" = list(
    aggregation_minutes = aggregation_minutes_list,
    forecast_periods = forecast_periods,
    train_split = 0.7,
    window_size = 12,
    lstm_units = 32,
    dense_units = 16,
    epochs = 30,
    batch_size = 16,
    patience = 5
  ),
  "candle" = list(
    aggregation_minutes = c( 1, 5, 15), 
    forecast_periods = c(  60,90,120),  
    train_split = 0.7,
    window_size = 6,
    lstm_units = 16,
    dense_units = 8,
    epochs = 20,
    batch_size = 8,
    patience = 5
  ),
  "vaping" = list(
    aggregation_minutes = c(1, 5, 15), 
    forecast_periods = c(30, 45, 60),  
    train_split = 0.7, 
    window_size = 2, 
    lstm_units = 8, 
    dense_units = 4, 
    epochs = 15, 
    batch_size = 4, 
    patience = 2 
  ),
  "cooking" = list(
    aggregation_minutes = c(1, 5, 15), 
    forecast_periods = c(30, 45, 60),  
    train_split = 0.7, 
    window_size = 2, 
    lstm_units = 8, 
    dense_units = 4, 
    epochs = 15, 
    batch_size = 4, 
    patience = 2 
  )
)

# Define relevant global parameters

current_params <- dataset_params[[dataset_name]]
aggregation_minutes_list <- current_params$aggregation_minutes
forecast_periods <- current_params$forecast_periods
train_split <- current_params$train_split
window_size <- current_params$window_size
lstm_units <- current_params$lstm_units
dense_units <- current_params$dense_units
epochs <- current_params$epochs
batch_size <- current_params$batch_size
patience <- current_params$patience
all_results <- data.frame()
final_results_preprocessed_list <- list()
final_results_unprocessed_list <- list()
max_pm25 <- max(pollutionData$pm25, na.rm = TRUE)

# Define AQI Bands for PM2.5

aqi_bands <- data.frame(
  category = c("Excellent", "Good", "Moderate", "Unhealthy for Sensitive Groups", "Unhealthy"),
  lower = c(0, 5, 15, 25, 35.5),
  upper = c(5, 15, 25, 35.5, 55.5 ),
  color = c("#00E400", "#92D050", "#FFFF00", "#FF7E00", "#FF0000")
)

# Data preparation function for univariate LSTM models
# Transforms time series data into sliding window format required by LSTM
# Creates input sequences (x) of length window_size and corresponding targets (y)
# Returns arrays formatted for Keras/TensorFlow input

create_lstm_dataset <- function(data, target_col, window_size) {
  x <- NULL
  y <- NULL
  for (i in 1:(length(data) - window_size)) {
    window <- data[i:(i + window_size - 1)]
    target <- data[i + window_size]
    if (is.null(x)) {
      x <- array(window, dim = c(1, window_size, 1))
      y <- array(target, dim = c(1, 1))
    } else {
      x <- abind::abind(x, array(window, dim = c(1, window_size, 1)), along = 1)
      y <- abind::abind(y, array(target, dim = c(1, 1)), along = 1)
    }
  }
  return(list(x = x, y = y))
}

# Data preparation function for multivariate LSTM models
# Transforms multivariate time series into 3D tensors for LSTM input
# Supports variable forecast horizons through forecast_idx parameter
# Handles edge cases and ensures appropriate array dimensions

create_multivariate_lstm_dataset <- function(data, window_size, forecast_idx = 1) {
  n_features <- ncol(data)
  n_samples <- nrow(data) - window_size - forecast_idx + 1
  if (n_samples <= 0) {
    return(NULL)
  }
  x <- array(0, dim = c(n_samples, window_size, n_features))
  y <- array(0, dim = c(n_samples, 1))
  for (i in 1:n_samples) {
    x[i, , ] <- as.matrix(data[i:(i + window_size - 1), ])
    y[i, 1] <- data[i + window_size + forecast_idx - 1, 1]  
  }
  return(list(x = x, y = y))
}

# LSTM neural network architecture construction and training
# Implements a stacked LSTM model with:
#   - Two LSTM layers with dropout for regularization
#   - Dense layers for dimensionality reduction and prediction
#   - Early stopping to prevent overfitting
# Returns trained model and training history for performance analysis

build_lstm_model <- function(x_train, y_train, x_val, y_val, 
                             lstm_units = 50, dense_units = 20, 
                             epochs = 30, batch_size = 16, patience = 5) {
  input_shape <- dim(x_train)[2:3]
  tryCatch({
    model <- keras_model_sequential() %>%
      layer_lstm(units = lstm_units, input_shape = input_shape, return_sequences = TRUE) %>%
      layer_dropout(rate = 0.2) %>%
      layer_lstm(units = lstm_units/2) %>%
      layer_dropout(rate = 0.2) %>%
      layer_dense(units = dense_units, activation = 'relu') %>%
      layer_dense(units = 1)
    model %>% compile(
      loss = 'mean_squared_error',
      optimizer = optimizer_adam(learning_rate = 0.001),
      metrics = c('mean_absolute_error')
    )
    early_stopping <- callback_early_stopping(
      monitor = "val_loss",
      patience = patience,
      restore_best_weights = TRUE
    )
    history <- model %>% fit(
      x_train, y_train,
      epochs = epochs,
      batch_size = batch_size,
      validation_data = list(x_val, y_val),
      callbacks = list(early_stopping),
      verbose = 1
    )
    return(list(model = model, history = history))
  }, error = function(e) {
    cat("Error in LSTM model training:", e$message, "\n")
    return(NULL)
  })
}

# Implementation of LSTM model with comprehensive preprocessing
# Performs advanced feature engineering for neural network input:
#   - Temporal features (hour, day, cyclical transformations)
#   - Multiple lag features at different time scales
#   - Feature scaling for improved gradient-based learning
# Handles train/validation/test splitting and prediction generation
# Returns detailed performance metrics and forecast results

run_lstm_preprocessed <- function(forecast_minutes, data, aggregationMinutes) {
  forecast_periods <- ceiling(forecast_minutes / aggregationMinutes)
  total_rows <- nrow(data)
  if (total_rows < window_size * 3) {
    cat("Dataset too small for LSTM with window size", window_size, "\n")
    return(list(
      mae_train = NA, mae_test = NA, rmse_train = NA, rmse_test = NA,
      mape_train = NA, mape_test = NA, r_squared = NA, evs = NA,
      forecast = NA, actual = NA, model = NA
    ))
  }
  features_df <- data %>%
    mutate(
      hour = hour(ts),
      day_of_week = wday(ts),
      is_weekend = ifelse(wday(ts) %in% c(1, 7), 1, 0),
      hour_sin = sin(2 * pi * hour / 24),
      hour_cos = cos(2 * pi * hour / 24),
      pm25_lag1 = lag(pm25, 1),
      pm25_lag2 = lag(pm25, 2),
      pm25_lag24 = lag(pm25, 24)
    ) %>%
    dplyr::select(-ts) 
  features_df <- features_df %>%
    mutate(across(contains("lag"), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))
  target_column <- "pm25"
  scale_features <- features_df %>% dplyr::select(-all_of(target_column))
  scale_means <- colMeans(scale_features, na.rm = TRUE)
  scale_sds <- apply(scale_features, 2, sd, na.rm = TRUE)
  scaled_features <- as.data.frame(scale(scale_features))
  scaled_data <- cbind(features_df[target_column], scaled_features)
  train_size <- floor(nrow(scaled_data) * train_split)
  train_data <- scaled_data[1:train_size, ]
  test_data <- scaled_data[(train_size+1):nrow(scaled_data), ]
  val_size <- floor(train_size * 0.2)
  actual_train_size <- train_size - val_size
  train_lstm <- train_data[1:actual_train_size, ]
  val_lstm <- train_data[(actual_train_size+1):train_size, ]
  train_dataset <- create_multivariate_lstm_dataset(as.matrix(train_lstm), window_size, forecast_idx = 1)
  val_dataset <- create_multivariate_lstm_dataset(as.matrix(val_lstm), window_size, forecast_idx = 1)
  if (is.null(train_dataset) || is.null(val_dataset)) {
    cat("Error creating LSTM datasets\n")
    return(list(
      mae_train = NA, mae_test = NA, rmse_train = NA, rmse_test = NA,
      mape_train = NA, mape_test = NA, r_squared = NA, evs = NA,
      forecast = NA, actual = NA, model = NA
    ))
  }
  model_result <- build_lstm_model(
    train_dataset$x, train_dataset$y,
    val_dataset$x, val_dataset$y,
    lstm_units = lstm_units,
    dense_units = dense_units,
    epochs = epochs,
    batch_size = batch_size,
    patience = patience
  )
  if (is.null(model_result)) {
    cat("LSTM model training failed\n")
    return(list(
      mae_train = NA, mae_test = NA, rmse_train = NA, rmse_test = NA,
      mape_train = NA, mape_test = NA, r_squared = NA, evs = NA,
      forecast = NA, actual = NA, model = NA
    ))
  }
  lstm_model <- model_result$model
  train_predictions <- lstm_model %>% predict(train_dataset$x)
  look_ahead <- forecast_periods
  test_with_history <- rbind(tail(train_data, window_size), test_data)
  forecasts <- numeric(nrow(test_data))
  for (i in 1:(nrow(test_data))) {
    if (i + look_ahead > nrow(test_data)) break
    current_window <- test_with_history[i:(i+window_size-1), ]
    current_x <- array(as.matrix(current_window), dim = c(1, window_size, ncol(current_window)))
    pred <- lstm_model %>% predict(current_x)
    forecasts[i + look_ahead] <- as.numeric(pred)
  }
  actual_values <- test_data$pm25
  safe_mape <- function(actual, predicted) {
    idx <- which(actual > 0)
    if (length(idx) == 0) return(NA)
    mean(abs((actual[idx] - predicted[idx]) / actual[idx]) * 100, na.rm = TRUE)
  }
  valid_idx <- which(!is.na(forecasts) & !is.na(actual_values))
  if (length(valid_idx) > 0) {
    valid_forecasts <- forecasts[valid_idx]
    valid_actuals <- actual_values[valid_idx]
    mae_test <- mae(valid_actuals, valid_forecasts)
    rmse_test <- rmse(valid_actuals, valid_forecasts)
    mape_test <- safe_mape(valid_actuals, valid_forecasts)
    if (length(valid_idx) > 1) {
      tss <- sum((valid_actuals - mean(valid_actuals))^2)
      rss <- sum((valid_actuals - valid_forecasts)^2)
      r_squared <- 1 - (rss / tss)
      r_squared <- min(max(r_squared, -1), 1)  
    } else {
      r_squared <- NA
    }
    if (length(valid_idx) > 1) {
      var_actual <- var(valid_actuals, na.rm = TRUE)
      var_residuals <- var(valid_actuals - valid_forecasts, na.rm = TRUE)
      evs <- 1 - var_residuals / var_actual
      evs <- min(max(evs, 0), 1)  
    } else {
      evs <- NA
    }
  } else {
    mae_test <- NA
    rmse_test <- NA
    mape_test <- NA
    r_squared <- NA
    evs <- NA
  }
  train_mae <- mae(train_dataset$y, train_predictions)
  train_rmse <- rmse(train_dataset$y, train_predictions)
  train_mape <- safe_mape(train_dataset$y, train_predictions)
  return(list(
    mae_train = train_mae,
    mae_test = mae_test,
    rmse_train = train_rmse,
    rmse_test = rmse_test,
    mape_train = train_mape,
    mape_test = mape_test,
    r_squared = r_squared,
    evs = evs,
    forecast = valid_forecasts,
    actual = valid_actuals,
    model = lstm_model,
    lower = valid_forecasts * 0.9,  
    upper = valid_forecasts * 1.1   
  ))
}

# Implementation of LSTM model with minimal preprocessing
# Serves as baseline comparison using raw time series features
# Uses simplified network architecture with reduced units
# Maintains identical validation approach to preprocessed version
# Enables quantification of feature engineering benefits

run_lstm_unprocessed <- function(forecast_minutes, data, aggregationMinutes) {
  forecast_periods <- ceiling(forecast_minutes / aggregationMinutes)
  total_rows <- nrow(data)
  if (total_rows < window_size * 3) {
    cat("Dataset too small for LSTM with window size", window_size, "\n")
    return(list(
      mae_train = NA, mae_test = NA, rmse_train = NA, rmse_test = NA,
      mape_train = NA, mape_test = NA, r_squared = NA, evs = NA,
      forecast = NA, actual = NA, model = NA
    ))
  }
  features_df <- data %>%
    dplyr::select(-ts) 
  target_column <- "pm25"
  scale_features <- features_df %>% dplyr::select(-all_of(target_column))
  unprocessed_data <- features_df
  train_size <- floor(nrow(unprocessed_data) * train_split)
  train_data <- unprocessed_data[1:train_size, ]
  test_data <- unprocessed_data[(train_size+1):nrow(unprocessed_data), ]
  val_size <- floor(train_size * 0.2)
  actual_train_size <- train_size - val_size
  train_lstm <- train_data[1:actual_train_size, ]
  val_lstm <- train_data[(actual_train_size+1):train_size, ]
  train_dataset <- create_multivariate_lstm_dataset(as.matrix(train_lstm), window_size, forecast_idx = 1)
  val_dataset <- create_multivariate_lstm_dataset(as.matrix(val_lstm), window_size, forecast_idx = 1)
  if (is.null(train_dataset) || is.null(val_dataset)) {
    cat("Error creating LSTM datasets\n")
    return(list(
      mae_train = NA, mae_test = NA, rmse_train = NA, rmse_test = NA,
      mape_train = NA, mape_test = NA, r_squared = NA, evs = NA,
      forecast = NA, actual = NA, model = NA
    ))
  }
  model_result <- build_lstm_model(
    train_dataset$x, train_dataset$y,
    val_dataset$x, val_dataset$y,
    lstm_units = lstm_units/2,  
    dense_units = dense_units/2,
    epochs = epochs,
    batch_size = batch_size,
    patience = patience
  )
  if (is.null(model_result)) {
    cat("LSTM model training failed\n")
    return(list(
      mae_train = NA, mae_test = NA, rmse_train = NA, rmse_test = NA,
      mape_train = NA, mape_test = NA, r_squared = NA, evs = NA,
      forecast = NA, actual = NA, model = NA
    ))
  }
  lstm_model <- model_result$model
  train_predictions <- lstm_model %>% predict(train_dataset$x)
  look_ahead <- forecast_periods
  test_with_history <- rbind(tail(train_data, window_size), test_data)
  forecasts <- numeric(nrow(test_data))
  for (i in 1:(nrow(test_data))) {
    if (i + look_ahead > nrow(test_data)) break
    current_window <- test_with_history[i:(i+window_size-1), ]
    current_x <- array(as.matrix(current_window), dim = c(1, window_size, ncol(current_window)))
    pred <- lstm_model %>% predict(current_x)
    forecasts[i + look_ahead] <- as.numeric(pred)
  }
  actual_values <- test_data$pm25
  safe_mape <- function(actual, predicted) {
    idx <- which(actual > 0)
    if (length(idx) == 0) return(NA)
    mean(abs((actual[idx] - predicted[idx]) / actual[idx]) * 100, na.rm = TRUE)
  }
  valid_idx <- which(!is.na(forecasts) & !is.na(actual_values))
  if (length(valid_idx) > 0) {
    valid_forecasts <- forecasts[valid_idx]
    valid_actuals <- actual_values[valid_idx]
    mae_test <- mae(valid_actuals, valid_forecasts)
    rmse_test <- rmse(valid_actuals, valid_forecasts)
    mape_test <- safe_mape(valid_actuals, valid_forecasts)
    if (length(valid_idx) > 1) {
      tss <- sum((valid_actuals - mean(valid_actuals))^2)
      rss <- sum((valid_actuals - valid_forecasts)^2)
      r_squared <- 1 - (rss / tss)
      r_squared <- min(max(r_squared, -1), 1)  
    } else {
      r_squared <- NA
    }
    if (length(valid_idx) > 1) {
      var_actual <- var(valid_actuals, na.rm = TRUE)
      var_residuals <- var(valid_actuals - valid_forecasts, na.rm = TRUE)
      evs <- 1 - var_residuals / var_actual
      evs <- min(max(evs, 0), 1)  
    } else {
      evs <- NA
    }
  } else {
    mae_test <- NA
    rmse_test <- NA
    mape_test <- NA
    r_squared <- NA
    evs <- NA
  }
  train_mae <- mae(train_dataset$y, train_predictions)
  train_rmse <- rmse(train_dataset$y, train_predictions)
  train_mape <- safe_mape(train_dataset$y, train_predictions)
  return(list(
    mae_train = train_mae,
    mae_test = mae_test,
    rmse_train = train_rmse,
    rmse_test = rmse_test,
    mape_train = train_mape,
    mape_test = mape_test,
    r_squared = r_squared,
    evs = evs,
    forecast = valid_forecasts,
    actual = valid_actuals,
    model = lstm_model,
    lower = valid_forecasts * 0.9,  
    upper = valid_forecasts * 1.1   
  ))
}
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
    ExecutionTime = NA
  )
  
  # Comprehensive preprocessing pipeline for LSTM:
  # 1. Missing value imputation using Kalman filtering
  # 2. Temporal aggregation to reduce noise
  # 3. Feature engineering in the run_lstm_preprocessed function
  # This pipeline ensures optimal data quality for deep learning
  # while preserving temporal patterns essential for sequence modeling
  
  cat("\n===== Running LSTM WITH Preprocessing (Aggregation:", aggregationMinutes, "minutes) =====\n")
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
  for (i in 1:nrow(results_preprocessed)) {
    cat("Processing forecast period WITH preprocessing:", results_preprocessed$Hours[i], "hours\n")
    model_results <- run_lstm_preprocessed(results_preprocessed$Minutes[i], aggregatedData_preprocessed, aggregationMinutes)
    results_preprocessed$MAE_Train[i] <- model_results$mae_train
    results_preprocessed$MAE_Test[i] <- model_results$mae_test
    results_preprocessed$RMSE_Train[i] <- model_results$rmse_train
    results_preprocessed$RMSE_Test[i] <- model_results$rmse_test
    results_preprocessed$MAPE_Train[i] <- model_results$mape_train
    results_preprocessed$MAPE_Test[i] <- model_results$mape_test
    results_preprocessed$R_Squared[i] <- model_results$r_squared
    results_preprocessed$EVS[i] <- model_results$evs
    final_results_preprocessed_list[[as.character(results_preprocessed$Minutes[i])]] <- list(
      actual = model_results$actual,
      forecast = model_results$forecast,
      lower = model_results$lower,
      upper = model_results$upper
    )
  }
  end_time_preprocessed <- Sys.time()
  execution_time_preprocessed <- end_time_preprocessed - start_time_preprocessed
  results_preprocessed$ExecutionTime <- as.numeric(execution_time_preprocessed, units="secs")
  cat("\nExecution time WITH preprocessing: ", format(execution_time_preprocessed), "\n")
  cat("\n===== Running LSTM WITHOUT Preprocessing (Aggregation:", aggregationMinutes, "minutes) =====\n")
  start_time_unprocessed <- Sys.time()
  pollutionData_raw <- pollutionData
  pollutionData_raw[-1] <- lapply(pollutionData_raw[-1], function(x) { 
    x <- as.character(x)  
    x[x == "No data"] <- NA  
    return(x) 
  })
  pollutionData_raw[-1] <- lapply(pollutionData_raw[-1], function(x) { 
    x <- suppressWarnings(as.numeric(x))
    x[is.na(x)] <- mean(x, na.rm = TRUE)
    return(x)
  })
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
  for (i in 1:nrow(results_unprocessed)) {
    cat("Processing forecast period WITHOUT preprocessing:", results_unprocessed$Hours[i], "hours\n")
    model_results <- run_lstm_unprocessed(results_unprocessed$Minutes[i], aggregatedData_unprocessed, aggregationMinutes)
    results_unprocessed$MAE_Train[i] <- model_results$mae_train
    results_unprocessed$MAE_Test[i] <- model_results$mae_test
    results_unprocessed$RMSE_Train[i] <- model_results$rmse_train
    results_unprocessed$RMSE_Test[i] <- model_results$rmse_test
    results_unprocessed$MAPE_Train[i] <- model_results$mape_train
    results_unprocessed$MAPE_Test[i] <- model_results$mape_test
    results_unprocessed$R_Squared[i] <- model_results$r_squared
    results_unprocessed$EVS[i] <- model_results$evs
    final_results_unprocessed_list[[as.character(results_unprocessed$Minutes[i])]] <- list(
      actual = model_results$actual,
      forecast = model_results$forecast,
      lower = model_results$lower,
      upper = model_results$upper
    )
  }
  end_time_unprocessed <- Sys.time()
  execution_time_unprocessed <- end_time_unprocessed - start_time_unprocessed
  results_unprocessed$ExecutionTime <- as.numeric(execution_time_unprocessed, units="secs")
  cat("\nExecution time WITHOUT preprocessing: ", format(execution_time_unprocessed), "\n")
  all_results <- rbind(all_results, results_preprocessed, results_unprocessed)
  if (!dir.exists(plots_dir)) {
    dir.create(plots_dir, recursive = TRUE)
  }
  
  # Visualization framework for LSTM model predictions
  # Creates standardized plots with:
  #   - Confidence intervals using simplified approach (±10%)
  #   - Air Quality Index (AQI) band overlays for health context
  #   - Color-coded actual vs. predicted values
  # Designed for consistent visual comparison across configurations
  
  
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
                         breaks = c(5, 15, 25, 35.5, 55.5,300)) +
      scale_color_manual(name = "Data Type", values = c(
        "Actual" = "black", 
        "Forecast" = ifelse(preprocessing_status == "Yes", "blue", "orange"))) +
      scale_fill_manual(name = "AQI Category", values = setNames(aqi_bands$color, aqi_bands$category)) +
      labs(
        title = paste0("LSTM ", ifelse(preprocessing_status == "Yes", "WITH", "WITHOUT"), 
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
                             breaks = c(5, 15, 25, 35.5, 55.5,300)) +
          scale_color_manual(name = "Data Type", values = c(
            "Actual" = "black",
            "With Preprocessing" = "blue",
            "Without Preprocessing" = "orange")) +
          scale_fill_manual(name = "AQI Category", values = setNames(aqi_bands$color, aqi_bands$category)) +
          labs(
            title = paste0("LSTM Comparison: ", hour, "-Hour Forecast"),
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
        plot_filename <- paste0(plots_dir, "/LSTM_comparison_",
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
              title = paste0("LSTM Performance Metrics: ", hour, "-Hour Forecast"),
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
          expanded_metrics_filename <- paste0(plots_dir, "/LSTM_expanded_metrics_", 
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
              title = paste0("LSTM Test Performance: ", hour, "-Hour Forecast"),
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
          metrics_filename <- paste0(plots_dir, "/LSTM_metrics_test_", 
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
      plot_filename <- paste0(plots_dir, "/LSTM_preprocessed_", 
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
      plot_filename <- paste0(plots_dir, "/LSTM_unprocessed_", 
                              aggregationMinutes, "min_", 
                              forecast_hrs, "h_forecast", "_", 
                              format(Sys.time(), "%Y%m%d_%H%M%S"),".png")
      ggsave(plot_filename, unpre_plot, width = 10, height = 6, dpi = 300)
      print(unpre_plot)
    } else {
      cat("No valid unprocessed results available for", forecast_hrs, "hour forecast\n")
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