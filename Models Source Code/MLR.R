library(forecast) # Load necessary libraries
library(xts) 
library(tidyverse) 
library(Metrics) 
library(knitr)
library(lubridate)
library(caret) 
library(lmtest) 
library(tseries) 

# Set dataset name and get dataset-specific parameters
dataset_name <- "cooking"  

# Read data
fileName <- paste0("C:/Users/Christo/Desktop/FYP/R Code/DATASETS/", dataset_name,".csv")
pollutionData <- read.csv(fileName, header = TRUE)

# Dataset-specific parameters for each scenario
# Tailored configurations based on dataset characteristics:
#   - aggregation_minutes: temporal resolution options
#   - forecast_periods: prediction horizons in minutes
#   - train_split: proportion of data used for training
#   - max_lag: maximum number of lag features to generate
#   - cv_folds: number of cross-validation folds for model training

aggregation_minutes_list <- c(5,15,30,60,120)  # Multiple aggregation periods
forecast_periods <- c(
  60,
  60*2,  # 2 hours 
  60*3,  # 3 hours
  60*6,  # 6 hours
  60*12, # 12 hours
  60*24,  # 24 hours
  60*36,
  60*48,
  60*72
)

# Set working directory for output
setwd("C:/Users/Christo/Desktop/FYP/R Code/results")
training_set <- paste0("MLR_", dataset_name)

plots_dir <- training_set

# Initialize lists to store model results
final_results_preprocessed_list <- list()
final_results_unprocessed_list <- list()

dataset_params <- list(
  "long_term" = list(
    aggregation_minutes = aggregation_minutes_list,
    forecast_periods = forecast_periods,
    train_split = 0.7,
    max_lag = 4,
    cv_folds = 5
  ),
  "short_term" = list(
    aggregation_minutes = aggregation_minutes_list,
    forecast_periods = forecast_periods,
    train_split = 0.7,
    max_lag = 3,
    cv_folds = 4
  ),
  "candle" = list(
    aggregation_minutes = c( 1, 5, 15), 
    forecast_periods = c(  60,90,120),  
    train_split = 0.7,
    max_lag = 1,
    cv_folds = 3
  ),
  "vaping" = list(
    aggregation_minutes = c(1, 5, 15), 
    forecast_periods = c(30, 45, 60),  
    train_split = 0.7,
    max_lag = 1,
    cv_folds = 2
  ),
  "cooking" = list(
    aggregation_minutes = c(1, 5, 15), 
    forecast_periods = c(30, 45, 60),  
    train_split = 0.7,
    max_lag = 0,
    cv_folds = 2
  )
)

# Define relevant global parameters

current_params <- dataset_params[[dataset_name]]
aggregation_minutes_list <- current_params$aggregation_minutes
forecast_periods <- current_params$forecast_periods
train_split <- current_params$train_split
max_lag <- current_params$max_lag
cv_folds <- current_params$cv_folds
all_results <- data.frame()
max_pm25 <- max(pollutionData$pm25, na.rm = TRUE)

# Define AQI Bands for PM2.5

aqi_bands <- data.frame(
  category = c("Excellent", "Good", "Moderate", "Unhealthy for Sensitive Groups", "Unhealthy", "Hazardous"),
  lower = c(0, 5, 15, 25, 35.5, 55.5),
  upper = c(5, 15, 25, 35.5, 55.5, max(75, ceiling(max_pm25 * 1.1))),
  color = c("#00E400", "#92D050", "#FFFF00", "#FF7E00", "#FF0000", "#8F3F97")
)

# Implementation of MLR model with comprehensive preprocessing
# Trains and evaluates linear regression models for forecasting PM2.5
# Key features:
#   - Temporal feature engineering (cyclical transformations)
#   - Adaptive feature creation based on dataset duration
#   - Feature standardization for model stability
#   - Cross-validation for robust parameter estimation
#   - Extensive error metric calculation and visualization


run_mlr_preprocessed <- function(forecastMinutes, data, aggregationMinutes) {
  forecastNumber <- ceiling(forecastMinutes / aggregationMinutes)
  IndependentVariables <- data %>%
    mutate(
      hour = hour(ts),
      sin_hour = sin(2 * pi * hour / 24),
      cos_hour = cos(2 * pi * hour / 24)
    )
  if(max(data$ts) - min(data$ts) >= 3*24*60*60) {
    IndependentVariables <- IndependentVariables %>%
      mutate(
        day_of_week = wday(ts),
        sin_day = sin(2 * pi * day_of_week / 7),
        cos_day = cos(2 * pi * day_of_week / 7)
      )
  }
  if(max(data$ts) - min(data$ts) >= 30*24*60*60) {
    IndependentVariables <- IndependentVariables %>%
      mutate(
        month = month(ts),
        sin_month = sin(2 * pi * month / 12),
        cos_month = cos(2 * pi * month / 12)
      )
  }
  for(i in 1:max_lag) {
    lag_col_name <- paste0("pm25_lag", i)
    IndependentVariables[[lag_col_name]] <- lag(IndependentVariables$pm25, i)
  }
  lag_replacement <- list()
  for(i in 1:max_lag) {
    lag_col_name <- paste0("pm25_lag", i)
    lag_replacement[[lag_col_name]] <- ifelse(
      is.na(IndependentVariables[[lag_col_name]]), 
      mean(IndependentVariables$pm25, na.rm = TRUE), 
      IndependentVariables[[lag_col_name]]
    )
  }
  IndependentVariables <- IndependentVariables %>%
    mutate(!!!lag_replacement)
  IndependentVariables <- IndependentVariables %>% dplyr::select(-ts)
  target <- data.frame(pm2.5_future = lead(IndependentVariables$pm25, forecastNumber))
  modelData <- cbind(IndependentVariables, target)
  modelData$pm2.5_future[is.na(modelData$pm2.5_future)] <- mean(modelData$pm2.5_future, na.rm = TRUE)
  scale_cols <- names(modelData)[!names(modelData) %in% c("pm2.5_future")]
  modelData[scale_cols] <- scale(modelData[scale_cols])
  ctrl <- trainControl(method = "cv", number = cv_folds, verboseIter = FALSE)
  total_rows <- nrow(modelData)
  train_size <- floor(train_split * total_rows)
  train_data <- modelData[1:train_size, ]
  test_data <- modelData[(train_size+1):total_rows, ]
  formula <- as.formula("pm2.5_future ~ .")
  tryCatch({
    model <- train(formula, data = train_data, method = "lm", trControl = ctrl)
    train_pred <- predict(model, newdata = train_data)
    test_pred <- predict(model, newdata = test_data)
    train_pred <- pmax(train_pred, 0)  
    test_pred <- pmax(test_pred, 0)    
    naive_pred <- c(NA, test_data$pm2.5_future[1:(nrow(test_data)-1)])
    metrics <- list(
      mae_train = mae(train_data$pm2.5_future, train_pred),
      mae_test = mae(test_data$pm2.5_future, test_pred),
      rmse_train = rmse(train_data$pm2.5_future, train_pred),
      rmse_test = rmse(test_data$pm2.5_future, test_pred),
      mape_train = mean(abs((train_data$pm2.5_future - train_pred) / 
                              pmax(0.001, abs(train_data$pm2.5_future))), na.rm = TRUE) * 100,
      mape_test = mean(abs((test_data$pm2.5_future - test_pred) / 
                             pmax(0.001, abs(test_data$pm2.5_future))), na.rm = TRUE) * 100,
      r_squared = summary(model$finalModel)$r.squared,
      evs = 1 - var(test_data$pm2.5_future - test_pred, na.rm = TRUE) / var(test_data$pm2.5_future, na.rm = TRUE),
      residuals = test_data$pm2.5_future - test_pred,
      actual = test_data$pm2.5_future,
      forecast = test_pred,
      model = model,
      var_imp = varImp(model)
    )
    lb_test <- tryCatch({
      Box.test(metrics$residuals, lag = min(20, length(metrics$residuals)/5), type = "Ljung-Box")
    }, error = function(e) {
      return(list(p.value = NA))
    })
    metrics$ljung_box_p <- lb_test$p.value
    metrics$aic <- AIC(model$finalModel)
    metrics$bic <- BIC(model$finalModel)
    naive_rmse <- tryCatch({
      rmse(test_data$pm2.5_future[-1], naive_pred[-1])
    }, error = function(e) {
      return(NA)
    })
    if(!is.na(naive_rmse) && naive_rmse > 0) {
      metrics$theils_u <- metrics$rmse_test / naive_rmse
    } else {
      metrics$theils_u <- NA
    }
    metrics$forecast_bias <- mean(metrics$forecast - metrics$actual, na.rm = TRUE) / 
      mean(abs(metrics$actual), na.rm = TRUE) * 100
    metrics$residual_plot <- ggplot(data.frame(
      fitted = test_pred,
      residuals = test_data$pm2.5_future - test_pred
    )) +
      geom_point(aes(x = fitted, y = residuals)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      geom_smooth(aes(x = fitted, y = residuals), method = "loess") +
      labs(title = "Residual Plot", 
           subtitle = paste("Forecast Horizon:", forecastMinutes/60, "hours"),
           x = "Fitted Values", 
           y = "Residuals") +
      theme_minimal()
    return(metrics)
  }, error = function(e) {
    cat("Error in model training:", e$message, "\n")
    return(list(
      mae_train = NA, mae_test = NA, rmse_train = NA, rmse_test = NA,
      mape_train = NA, mape_test = NA, r_squared = NA, evs = NA,
      ljung_box_p = NA, aic = NA, bic = NA, theils_u = NA, forecast_bias = NA,
      actual = NA, forecast = NA, residuals = NA, var_imp = NA,
      residual_plot = NA
    ))
  })
}

# Implementation of MLR model with minimal preprocessing
# Serves as baseline comparison without feature engineering
# Uses raw temporal indicators without transformations
# Maintains identical evaluation framework as preprocessed version
# Enables quantification of preprocessing benefits


run_mlr_unprocessed <- function(forecastMinutes, data, aggregationMinutes) {
  forecastNumber <- ceiling(forecastMinutes / aggregationMinutes)
  IndependentVariables <- data %>%
    mutate(
      hour = hour(ts),
      day_of_week = wday(ts),
      month = month(ts)
    )
  IndependentVariables <- IndependentVariables %>% dplyr::select(-ts)
  target <- data.frame(pm2.5_future = lead(IndependentVariables$pm25, forecastNumber))
  modelData <- cbind(IndependentVariables, target)
  modelData$pm2.5_future[is.na(modelData$pm2.5_future)] <- mean(modelData$pm2.5_future, na.rm = TRUE)
  ctrl <- trainControl(method = "cv", number = 5, verboseIter = FALSE)
  total_rows <- nrow(modelData)
  train_size <- floor(train_split * total_rows)
  train_data <- modelData[1:train_size, ]
  test_data <- modelData[(train_size+1):total_rows, ]
  formula <- as.formula("pm2.5_future ~ .")
  tryCatch({
    model <- train(formula, data = train_data, method = "lm", trControl = ctrl)
    train_pred <- predict(model, newdata = train_data)
    test_pred <- predict(model, newdata = test_data)
    train_pred <- pmax(train_pred, 0)  
    test_pred <- pmax(test_pred, 0)    
    naive_pred <- c(NA, test_data$pm2.5_future[1:(nrow(test_data)-1)])
    metrics <- list(
      mae_train = mae(train_data$pm2.5_future, train_pred),
      mae_test = mae(test_data$pm2.5_future, test_pred),
      rmse_train = rmse(train_data$pm2.5_future, train_pred),
      rmse_test = rmse(test_data$pm2.5_future, test_pred),
      mape_train = mean(abs((train_data$pm2.5_future - train_pred) / 
                              pmax(0.001, abs(train_data$pm2.5_future))), na.rm = TRUE) * 100,
      mape_test = mean(abs((test_data$pm2.5_future - test_pred) / 
                             pmax(0.001, abs(test_data$pm2.5_future))), na.rm = TRUE) * 100,
      r_squared = summary(model$finalModel)$r.squared,
      evs = 1 - var(test_data$pm2.5_future - test_pred, na.rm = TRUE) / var(test_data$pm2.5_future, na.rm = TRUE),
      residuals = test_data$pm2.5_future - test_pred,
      actual = test_data$pm2.5_future,
      forecast = test_pred,
      model = model,
      var_imp = varImp(model)
    )
    lb_test <- tryCatch({
      Box.test(metrics$residuals, lag = min(20, length(metrics$residuals)/5), type = "Ljung-Box")
    }, error = function(e) {
      return(list(p.value = NA))
    })
    metrics$ljung_box_p <- lb_test$p.value
    metrics$aic <- AIC(model$finalModel)
    metrics$bic <- BIC(model$finalModel)
    naive_rmse <- tryCatch({
      rmse(test_data$pm2.5_future[-1], naive_pred[-1])
    }, error = function(e) {
      return(NA)
    })
    if(!is.na(naive_rmse) && naive_rmse > 0) {
      metrics$theils_u <- metrics$rmse_test / naive_rmse
    } else {
      metrics$theils_u <- NA
    }
    metrics$forecast_bias <- mean(metrics$forecast - metrics$actual, na.rm = TRUE) / 
      mean(abs(metrics$actual), na.rm = TRUE) * 100
    metrics$residual_plot <- ggplot(data.frame(
      fitted = test_pred,
      residuals = test_data$pm2.5_future - test_pred
    )) +
      geom_point(aes(x = fitted, y = residuals)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      geom_smooth(aes(x = fitted, y = residuals), method = "loess") +
      labs(title = "Residual Plot", 
           subtitle = paste("Forecast Horizon:", forecastMinutes/60, "hours"),
           x = "Fitted Values", 
           y = "Residuals") +
      theme_minimal()
    return(metrics)
  }, error = function(e) {
    cat("Error in model training:", e$message, "\n")
    return(list(
      mae_train = NA, mae_test = NA, rmse_train = NA, rmse_test = NA,
      mape_train = NA, mape_test = NA, r_squared = NA, evs = NA,
      ljung_box_p = NA, aic = NA, bic = NA, theils_u = NA, forecast_bias = NA,
      actual = NA, forecast = NA, residuals = NA, var_imp = NA,
      residual_plot = NA
    ))
  })
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
  
  # Comprehensive preprocessing pipeline for MLR:
  # 1. Missing value imputation with mean replacement
  # 2. Temporal aggregation to reduce noise
  # 3. Advanced feature engineering in the run_mlr_preprocessed function
  # This approach ensures robust model training while preserving
  # important temporal patterns in the air quality data
  
  cat("\n===== Running MLR WITH Preprocessing (Aggregation:", aggregationMinutes, "minutes) =====\n")
  start_time_preprocessed <- Sys.time()
  pollutionData_clean <- pollutionData
  pollutionData_clean[-1] <- lapply(pollutionData_clean[-1], function(x) { 
    x <- as.character(x)  
    x[x == "No data"] <- NA  
    x <- as.numeric(x)
    return(ifelse(is.na(x), mean(x, na.rm = TRUE), x))  
  })
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
    model_results <- run_mlr_preprocessed(results_preprocessed$Minutes[i], aggregatedData_preprocessed, aggregationMinutes)
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
    final_results_preprocessed_list[[paste0(aggregationMinutes, "_", results_preprocessed$Minutes[i])]] <- model_results
    cat("\nVariable Importance for", results_preprocessed$Hours[i], "hour forecast:\n")
    print(model_results$var_imp)
    print(model_results$residual_plot)
  }
  end_time_preprocessed <- Sys.time()
  execution_time_preprocessed <- end_time_preprocessed - start_time_preprocessed
  results_preprocessed$ExecutionTime <- as.numeric(execution_time_preprocessed, units="secs")
  cat("\nExecution time WITH preprocessing: ", format(execution_time_preprocessed), "\n")
  cat("\n===== Running MLR WITHOUT Preprocessing (Aggregation:", aggregationMinutes, "minutes) =====\n")
  start_time_unprocessed <- Sys.time()
  pollutionData_raw <- pollutionData
  pollutionData_raw[-1] <- lapply(pollutionData_raw[-1], function(x) { 
    x <- as.character(x)  
    x[x == "No data"] <- NA  
    x <- as.numeric(x)
    return(ifelse(is.na(x), mean(x, na.rm = TRUE), x))
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
    model_results <- run_mlr_unprocessed(results_unprocessed$Minutes[i], aggregatedData_unprocessed, aggregationMinutes)
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
    final_results_unprocessed_list[[paste0(aggregationMinutes, "_", results_unprocessed$Minutes[i])]] <- model_results
    cat("\nVariable Importance for", results_unprocessed$Hours[i], "hour forecast:\n")
    print(model_results$var_imp)
    print(model_results$residual_plot)
  }
  end_time_unprocessed <- Sys.time()
  execution_time_unprocessed <- end_time_unprocessed - start_time_unprocessed
  results_unprocessed$ExecutionTime <- as.numeric(execution_time_unprocessed, units="secs")
  cat("\nExecution time WITHOUT preprocessing: ", format(execution_time_unprocessed), "\n")
  all_results <- rbind(all_results, results_preprocessed, results_unprocessed)
  if (!dir.exists(plots_dir)) {
    dir.create(plots_dir, recursive = TRUE)
  }
  
  # Visualization framework for MLR model results
  # Creates standardized comparison plots with:
  #   - Air Quality Index (AQI) band overlays
  #   - Color-coded forecasts and actual values
  #   - Adaptive scaling and labeling
  #   - Consistent formatting for comparison across configurations
  # Provides visual assessment of model performance and forecast utility
  
  create_forecast_plot <- function(actual, forecast, time_index, aggregationMinutes, 
                                   forecastMinutes, preprocessing_status, aqi_bands) {
    plot_data <- data.frame(
      Index = 1:length(actual),
      Actual = actual,
      Forecast = forecast
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
      geom_line(aes(y = Actual, color = "Actual"), linewidth = 0.8) +
      geom_line(aes(y = Forecast, color = "Forecast"), linewidth = 0.8) +
      geom_point(data = aqi_dummy, aes(x = x, y = y, fill = category), 
                 shape = 22, size = 7, alpha = 0) +
      scale_y_continuous(name = expression(PM[2.5]~(mu*g/m^3)), 
                         breaks = c(5, 15, 25, 35.5, 55.5,75)) +
      scale_color_manual(name = "Data Type", values = c(
        "Actual" = "black", 
        "Forecast" = ifelse(preprocessing_status == "Yes", "blue", "orange"))) +
      scale_fill_manual(name = "AQI Category", values = setNames(aqi_bands$color, aqi_bands$category)) +
      labs(
        title = paste0("MLR ", ifelse(preprocessing_status == "Yes", "WITH", "WITHOUT"), 
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
  for (i in 1:nrow(results_preprocessed)) {
    cat("Creating visualization for aggregation:", aggregationMinutes, 
        "minutes, forecast:", results_preprocessed$Hours[i], "hours\n")
    pre_key <- paste0(aggregationMinutes, "_", results_preprocessed$Minutes[i])
    unpre_key <- paste0(aggregationMinutes, "_", results_unprocessed$Minutes[i])
    model_results_pre <- final_results_preprocessed_list[[pre_key]]
    model_results_unpre <- final_results_unprocessed_list[[unpre_key]]
    if (!is.null(model_results_pre) && !is.na(model_results_pre$actual[1]) && !is.na(model_results_pre$forecast[1])) {
      pre_plot <- create_forecast_plot(
        model_results_pre$actual, 
        model_results_pre$forecast, 
        1:length(model_results_pre$actual),
        aggregationMinutes, 
        results_preprocessed$Minutes[i], 
        "Yes", 
        aqi_bands
      )
      plot_filename <- paste0(plots_dir, "/MLR_preprocessed_", 
                              aggregationMinutes, "min_", 
                              results_preprocessed$Hours[i], "h_forecast.png")
      ggsave(plot_filename, pre_plot, width = 10, height = 6, dpi = 250)
      print(pre_plot)
    }
    if (!is.null(model_results_unpre) && !is.na(model_results_unpre$actual[1]) && !is.na(model_results_unpre$forecast[1])) {
      unpre_plot <- create_forecast_plot(
        model_results_unpre$actual, 
        model_results_unpre$forecast, 
        1:length(model_results_unpre$actual),
        aggregationMinutes, 
        results_unprocessed$Minutes[i], 
        "No", 
        aqi_bands
      )
      plot_filename <- paste0(plots_dir, "/MLR_unprocessed_", 
                              aggregationMinutes, "min_", 
                              results_unprocessed$Hours[i], "h_forecast.png")
      ggsave(plot_filename, unpre_plot, width = 10, height = 6, dpi = 300)
      print(unpre_plot)
    }
  }
  
  # Comprehensive comparison visualization generator
  # Creates four types of visualization for each configuration:
  # 1. Side-by-side comparison of forecasts (preprocessed vs. unprocessed)
  # 2. AQI band overlays for health context interpretation
  # 3. Expanded metrics visualization including train/test performance
  # 4. Test-specific metrics for focused evaluation
  # Handles edge cases and ensures consistent output formatting
  
  create_comparison_plots <- function() {
    comparison_hours <- forecast_periods / 60
    for (hour in comparison_hours) {
      for (agg_min in aggregation_minutes_list) {
        cat("Creating comparison plot for", hour, "hour forecast with", agg_min, "minute aggregation\n")
        forecast_min <- hour * 60
        pre_key <- paste0(agg_min, "_", forecast_min)
        unpre_key <- paste0(agg_min, "_", forecast_min)
        model_pre <- final_results_preprocessed_list[[pre_key]]
        model_unpre <- final_results_unprocessed_list[[unpre_key]]
        if (is.null(model_pre) || is.null(model_unpre)) {
          cat("Skipping comparison plot due to missing data in results\n")
          next
        }
        if (is.na(model_pre$actual[1]) || is.na(model_unpre$actual[1])) {
          cat("Skipping comparison plot due to missing prediction data\n")
          next
        }
        compare_data <- data.frame(
          Index = 1:length(model_pre$actual),
          Actual = model_pre$actual,
          Preprocessed = model_pre$forecast,
          Unprocessed = model_unpre$forecast
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
                             breaks = c(5, 15, 25, 35.5, 55.5,75)) +
          scale_color_manual(name = "Data Type", values = c(
            "Actual" = "black",
            "With Preprocessing" = "blue",
            "Without Preprocessing" = "orange")) +
          scale_fill_manual(name = "AQI Category", values = setNames(aqi_bands$color, aqi_bands$category)) +
          labs(
            title = paste0("MLR Comparison: ", hour, "-Hour Forecast"),
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
        plot_filename <- paste0(plots_dir, "/MLR_comparison_",
                                agg_min, "min_", hour, "h_forecast.png")
        ggsave(plot_filename, compare_plot, width = 10, height = 6, dpi = 300)
        print(compare_plot)
        pre_row <- all_results %>% 
          filter(AggregationMinutes == agg_min, 
                 Hours == hour, 
                 Preprocessing == "Yes")
        unpre_row <- all_results %>% 
          filter(AggregationMinutes == agg_min, 
                 Hours == hour, 
                 Preprocessing == "No")
        expanded_metrics_data <- data.frame(
          Metric = rep(c("MAE", "RMSE", "MAPE (%)", "R²"), 2),
          Dataset = c(rep("Test", 4), rep("Train", 4)),
          Preprocessed = c(
            pre_row$MAE_Test,
            pre_row$RMSE_Test,
            pre_row$MAPE_Test,
            pre_row$R_Squared,
            pre_row$MAE_Train, 
            pre_row$RMSE_Train,
            pre_row$MAPE_Train,
            pre_row$R_Squared  
          ),
          Unprocessed = c(
            unpre_row$MAE_Test,
            unpre_row$RMSE_Test,
            unpre_row$MAPE_Test,
            unpre_row$R_Squared,
            unpre_row$MAE_Train,
            unpre_row$RMSE_Train,
            unpre_row$MAPE_Train,
            unpre_row$R_Squared  
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
            title = paste0("Performance Metrics Comparison: ", hour, "-Hour Forecast"),
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
        expanded_metrics_filename <- paste0(plots_dir, "/MLR_expanded_metrics_", 
                                            agg_min, "min_", hour, "h_forecast.png")
        ggsave(expanded_metrics_filename, expanded_metrics_plot, width = 12, height = 7, dpi = 300)
        print(expanded_metrics_plot)
        metrics_data <- data.frame(
          Metric = c("MAE", "RMSE", "MAPE (%)", "R²"),
          Preprocessed = c(
            pre_row$MAE_Test,
            pre_row$RMSE_Test,
            pre_row$MAPE_Test,
            pre_row$R_Squared
          ),
          Unprocessed = c(
            unpre_row$MAE_Test,
            unpre_row$RMSE_Test,
            unpre_row$MAPE_Test,
            unpre_row$R_Squared
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
            title = paste0("Test Performance Metrics: ", hour, "-Hour Forecast"),
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
        metrics_filename <- paste0(plots_dir, "/MLR_metrics_test_", 
                                   agg_min, "min_", hour, "h_forecast.png")
        ggsave(metrics_filename, metrics_plot, width = 10, height = 6, dpi = 300)
        print(metrics_plot)
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