library(tidyverse) # Load necessary libraries
library(forecast)
library(lubridate)
library(knitr)
library(ggplot2)
library(gridExtra)
library(scales)

# Set working directory - change this to your project directory
setwd("C:/Users/Christo/Desktop/FYP/R Code/Change Detection Algorithm")

# Set dataset name 
dataset_name <- "cooking"  

# Read data
fileName <- paste0("C:/Users/Christo/Desktop/FYP/R Code/DATASETS/", dataset_name,".csv")
pollutionData <- read.csv(fileName, header = TRUE)

# Create output directory for the dataset if it doesn't exist
if (!dir.exists(dataset_name)) {
  dir.create(dataset_name)
}

setwd(paste0("C:/Users/Christo/Desktop/FYP/R Code/Change Detection Algorithm/",dataset_name))

# Convert timestamp for analysis
pollutionData$ts <- as.POSIXct(pollutionData$ts, format="%d/%m/%Y %H:%M", tz = "GMT")

dynamic_params <- function(data, dataset_name) {
  # Get basic dataset properties
  total_duration_hours <- as.numeric(difftime(max(data$ts), min(data$ts), units="hours"))
  
  # Calculate sampling frequency - handle possible non-uniform sampling
  time_diffs <- diff(as.numeric(data$ts))
  sampling_frequency_mins <- median(time_diffs) / 60  # Convert seconds to minutes
  
  # Calculate data volatility - first convert any "No data" to NA
  pm25_data <- data$pm25
  if(is.character(pm25_data)) {
    pm25_data[pm25_data == "No data"] <- NA
    pm25_data <- as.numeric(pm25_data)
  }
  
  # Handle NA values for calculation
  pm25_data <- pm25_data[!is.na(pm25_data)]
  data_volatility <- sd(pm25_data, na.rm = TRUE) / mean(pm25_data, na.rm = TRUE)  # Coefficient of variation
  
  # Print dataset characteristics
  cat("\n============= DATASET CHARACTERISTICS =============\n")
  cat("Dataset:", dataset_name, "\n")
  cat("Duration (hours):", total_duration_hours, "\n")
  cat("Sampling frequency (mins):", sampling_frequency_mins, "\n")
  cat("Data volatility (CV):", data_volatility, "\n")
  cat("Min PM2.5:", min(pm25_data, na.rm = TRUE), "\n")
  cat("Max PM2.5:", max(pm25_data, na.rm = TRUE), "\n")
  cat("Mean PM2.5:", mean(pm25_data, na.rm = TRUE), "\n")
  cat("==================================================\n\n")
  
  # Define parameters based on dataset duration
  if (total_duration_hours < 24) {
    params <- list(
      aggregationMinutes = max(1, min(round(sampling_frequency_mins), 5)),
      cusum_k_factor = 0.5,
      cusum_h_factor = 5,
      ewma_lambda = 0.2,
      ewma_L = 3.0,
      window_size = max(4, round(total_duration_hours / 4)),  # 1/4 of the dataset duration
      window_threshold = 3,
      top_cusum = 20,
      top_ewma = 15,
      top_rolling = 10
    )
  } else if (total_duration_hours < 168) {  # Less than 1 week
    params <- list(
      aggregationMinutes = max(5, min(round(sampling_frequency_mins * 2), 15)),
      cusum_k_factor = 0.6,
      cusum_h_factor = 6,
      ewma_lambda = 0.15,
      ewma_L = 3.2,
      window_size = max(6, round(total_duration_hours / 6)),  # 1/6 of the dataset duration
      window_threshold = 3.5,
      top_cusum = 30,
      top_ewma = 25,
      top_rolling = 20
    )
  } else {  )
    params <- list(
      aggregationMinutes = max(15, min(round(sampling_frequency_mins * 3), 60)),
      cusum_k_factor = 0.75,
      cusum_h_factor = 8,
      ewma_lambda = 0.1,
      ewma_L = 3.5,
      window_size = 24,  # 1 day for long datasets
      window_threshold = 4,
      top_cusum = 50,
      top_ewma = 40,
      top_rolling = 30
    )
  }
  
  # Adjust parameters based on volatility
  if (data_volatility > 0.5) {  # High volatility data
    params$cusum_k_factor <- params$cusum_k_factor * 1.2
    params$cusum_h_factor <- params$cusum_h_factor * 1.2
    params$ewma_L <- params$ewma_L * 1.1
    params$window_threshold <- params$window_threshold * 1.1
  } else if (data_volatility < 0.2) {  # Low volatility data
    params$cusum_k_factor <- params$cusum_k_factor * 0.9
    params$cusum_h_factor <- params$cusum_h_factor * 0.9
    params$ewma_L <- params$ewma_L * 0.9
    params$window_threshold <- params$window_threshold * 0.9
  }
  
  # Ensure reasonable bounds for parameters
  params$aggregationMinutes <- min(max(params$aggregationMinutes, 1), 60)
  params$window_size <- max(params$window_size, 2)  # At least 2 observations
  
  cat("================ SELECTED PARAMETERS ===============\n")
  cat("Aggregation minutes:", params$aggregationMinutes, "\n")
  cat("CUSUM K factor:", params$cusum_k_factor, "\n")
  cat("CUSUM H factor:", params$cusum_h_factor, "\n")
  cat("EWMA lambda:", params$ewma_lambda, "\n")
  cat("EWMA L:", params$ewma_L, "\n")
  cat("Window size:", params$window_size, "\n")
  cat("Window threshold:", params$window_threshold, "\n")
  cat("Top CUSUM changes to keep:", params$top_cusum, "\n")
  cat("Top EWMA changes to keep:", params$top_ewma, "\n")
  cat("Top Rolling Window changes to keep:", params$top_rolling, "\n")
  cat("==================================================\n\n")
  
  return(params)
}

# Function to create appropriate time labels based on dataset duration
create_dynamic_time_labels <- function(data) {
  # Calculate dataset duration in hours
  total_duration_hours <- as.numeric(difftime(max(data$ts), min(data$ts), units="hours"))
  
  # Aim for about 8-10 breaks in the axis
  n_breaks <- min(10, max(5, ceiling(length(data$ts) / 30)))
  
  # Create indices for breaks
  date_step <- max(1, round(length(data$ts) / n_breaks))
  date_breaks <- seq(1, length(data$ts), by = date_step)
  
  # Format based on duration
  if (total_duration_hours < 12) {
    # For very short datasets (< 12 hours): show time of day (HH:MM)
    date_labels <- format(data$ts[date_breaks], "%H:%M")
    x_axis_title <- "Time of Day"
  } else if (total_duration_hours < 72) {
    # For medium datasets (12-72 hours): show day and time
    date_labels <- format(data$ts[date_breaks], "%d %b\n%H:%M")
    x_axis_title <- "Date and Time"
  } else if (total_duration_hours < 720) {
    # For datasets spanning days to weeks: show day and month
    date_labels <- format(data$ts[date_breaks], "%d %b")
    x_axis_title <- "Date"
  } else {
    # For long datasets (months): show month and year
    date_labels <- format(data$ts[date_breaks], "%b %Y")
    x_axis_title <- "Month"
  }
  
  return(list(
    breaks = date_breaks,
    labels = date_labels,
    x_axis_title = x_axis_title
  ))
}

# Apply the function to get dynamic parameters
params <- dynamic_params(pollutionData, dataset_name)

# Use the dynamic parameters
aggregationMinutes <- params$aggregationMinutes

# DATA PREPROCESSING
# Convert instances of "No data" to NA
pollutionData[-1] <- lapply(pollutionData[-1], function(x) { 
  x <- as.character(x)  
  x[x == "No data"] <- NA  
  return(x) 
})

# Convert strings to numeric
pollutionData[-1] <- lapply(pollutionData[-1], function(x) { 
  suppressWarnings(as.numeric(x)) 
})

# Replace NAs with column means
columnMean <- sapply(pollutionData[-1], function(x) mean(as.numeric(x), na.rm = TRUE)) 
pollutionData[-1] <- Map(function(x, mean) ifelse(is.na(x), mean, x), pollutionData[-1], columnMean)

# Aggregate data by period
pollutionData$period <- as.numeric(difftime(pollutionData$ts, min(pollutionData$ts), units="mins")) %/% aggregationMinutes

# Aggregate data by period
aggregatedData <- pollutionData %>%
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

# Add temporal context features
aggregatedData <- aggregatedData %>%
  mutate(
    hour = hour(ts),
    day_of_week = wday(ts, label = TRUE),
    is_weekend = day_of_week %in% c("Sat", "Sun"),
    time_of_day = case_when(
      hour %in% 0:5 ~ "Night",
      hour %in% 6:11 ~ "Morning",
      hour %in% 12:17 ~ "Afternoon",
      hour %in% 18:23 ~ "Evening"
    ),
    date = as.Date(ts)
  )

# For short datasets (less than 24 hours)
if(max(as.numeric(difftime(max(aggregatedData$ts), min(aggregatedData$ts), units="hours"))) < 24) {
  # Create a time-based visualization instead of day-of-week
  p4 <- ggplot(aggregatedData, aes(x = hour(ts) + minute(ts)/60, y = pm25)) +
    geom_line() +
    geom_point(aes(color = time_of_day)) +
    scale_x_continuous(breaks = seq(0, 24, by = 1), 
                       labels = function(x) sprintf("%02d:00", as.integer(x))) +
    labs(title = "PM2.5 Levels by Time of Day",
         x = "Time (Hour)",
         y = "PM2.5 (μg/m³)",
         color = "Time of Day") +
    theme_minimal()
}

# 1. CUSUM Change Detection Algorithm

cusum_change_detection <- function(data, target_value, threshold_h, reference_value_k, top_n) {
  # Initialize variables
  n <- length(data)
  cusum_pos <- numeric(n)
  cusum_neg <- numeric(n)
  change_points <- integer(0)
  change_magnitudes <- numeric(0)
  
  # Calculate initial parameters
  cusum_pos[1] <- max(0, data[1] - target_value - reference_value_k)
  cusum_neg[1] <- max(0, target_value - data[1] - reference_value_k)
  
  # Identify change points
  for (i in 2:n) {
    # Calculate positive and negative CUSUMs
    cusum_pos[i] <- max(0, cusum_pos[i-1] + data[i] - target_value - reference_value_k)
    cusum_neg[i] <- max(0, cusum_neg[i-1] + target_value - data[i] - reference_value_k)
    
    # Check if a change point is detected
    if (cusum_pos[i] > threshold_h || cusum_neg[i] > threshold_h) {
      # Calculate magnitude as max of positive or negative CUSUM
      magnitude <- max(cusum_pos[i], cusum_neg[i])
      
      # Add to change points lists
      change_points <- c(change_points, i)
      change_magnitudes <- c(change_magnitudes, magnitude)
      
      # Reset CUSUM after change is detected
      cusum_pos[i] <- 0
      cusum_neg[i] <- 0
    }
  }
  
  # Create results data frame with magnitudes
  if (length(change_points) > 0) {
    results_df <- data.frame(
      index = change_points,
      magnitude = change_magnitudes
    )
    # Sort by magnitude to identify most significant changes
    results_df <- results_df[order(-results_df$magnitude), ]
  } else {
    results_df <- data.frame(index = integer(0), magnitude = numeric(0))
  }
  
  # Return results
  return(list(
    cusum_pos = cusum_pos,
    cusum_neg = cusum_neg,
    change_points_all = results_df,
    # Return only the top N most significant changes
    change_points = if(nrow(results_df) > 0) head(results_df$index, top_n) else integer(0)
  ))
}

# 2. EWMA Change Detection Algorithm

ewma_change_detection <- function(data, lambda = 0.1, L = 3.5, top_n) {
  # Initialize variables
  n <- length(data)
  data_mean <- mean(data, na.rm = TRUE)
  data_sd <- sd(data, na.rm = TRUE)
  ewma <- numeric(n)
  ucl <- numeric(n)  # Upper control limit
  lcl <- numeric(n)  # Lower control limit
  change_points <- integer(0)
  change_magnitudes <- numeric(0)
  
  # Initialize EWMA with the mean
  ewma[1] <- data_mean
  
  # Calculate control limits for first point - wider at the beginning
  factor <- lambda/(2-lambda) * (1-(1-lambda)^(2*0))
  ucl[1] <- data_mean + L * data_sd * sqrt(factor)
  lcl[1] <- data_mean - L * data_sd * sqrt(factor)
  
  # Calculate EWMA and detect changes
  prev_violation <- FALSE  # Flag to avoid consecutive violations
  for (i in 2:n) {
    # Update EWMA
    ewma[i] <- lambda * data[i] + (1-lambda) * ewma[i-1]
    
    # Calculate control limits - properly widening at start
    factor <- min(lambda/(2-lambda) * (1-(1-lambda)^(2*(i-1))), lambda/(2-lambda))
    ucl[i] <- data_mean + L * data_sd * sqrt(factor)
    lcl[i] <- data_mean - L * data_sd * sqrt(factor)
    
    # Check if out of control limits and not immediately after another violation
    if ((ewma[i] > ucl[i] || ewma[i] < lcl[i]) && !prev_violation) {
      # Calculate magnitude as distance from mean
      magnitude <- abs(ewma[i] - data_mean) / data_sd
      
      change_points <- c(change_points, i)
      change_magnitudes <- c(change_magnitudes, magnitude)
      
      prev_violation <- TRUE  # Set flag to avoid consecutive points
    } else {
      # Reset flag if back in control
      if (ewma[i] <= ucl[i] && ewma[i] >= lcl[i]) {
        prev_violation <- FALSE
      }
    }
  }
  
  # Create results data frame with magnitudes
  if (length(change_points) > 0) {
    results_df <- data.frame(
      index = change_points,
      magnitude = change_magnitudes
    )
    # Sort by magnitude
    results_df <- results_df[order(-results_df$magnitude), ]
  } else {
    results_df <- data.frame(index = integer(0), magnitude = numeric(0))
  }
  
  # Return results
  return(list(
    ewma = ewma,
    ucl = ucl,
    lcl = lcl,
    change_points_all = results_df,
    change_points = if(nrow(results_df) > 0) head(results_df$index, top_n) else integer(0)
  ))
}

# 3. Rolling Window Change Detection

rolling_window_change_detection <- function(data, window_size, threshold_sd, top_n) {
  n <- length(data)
  change_points <- integer(0)
  change_magnitudes <- numeric(0)
  
  if (n < 2*window_size) {
    warning("Data too short for the selected window size. Reducing window size.")
    window_size <- max(floor(n/4), 2)  # Adjust window size to 1/4 of data length, min 2
  }
  
  # For each potential change point
  for (i in (window_size+1):(n-window_size)) {
    # Get data before and after the potential change point
    before <- data[(i-window_size):(i-1)]
    after <- data[i:(i+window_size-1)]
    
    # Calculate means and standard deviations
    mean_before <- mean(before, na.rm = TRUE)
    mean_after <- mean(after, na.rm = TRUE)
    sd_before <- sd(before, na.rm = TRUE)
    
    # Use a minimum standard deviation to avoid division by very small values
    sd_before <- max(sd_before, 0.01 * mean(data, na.rm = TRUE))
    
    # Standardized magnitude of change
    magnitude <- abs(mean_after - mean_before) / sd_before
    
    # Detect change if the difference in means exceeds threshold
    if (magnitude > threshold_sd) {
      change_points <- c(change_points, i)
      change_magnitudes <- c(change_magnitudes, magnitude)
      
      # Skip ahead to avoid detecting the same change multiple times
      i <- i + window_size/2
    }
  }
  
  # Create results data frame with magnitudes
  if (length(change_points) > 0) {
    results_df <- data.frame(
      index = change_points,
      magnitude = change_magnitudes
    )
    # Sort by magnitude
    results_df <- results_df[order(-results_df$magnitude), ]
  } else {
    results_df <- data.frame(index = integer(0), magnitude = numeric(0))
  }
  
  # Return results
  return(list(
    change_points_all = results_df,
    # Return only the top N most significant changes
    change_points = if(nrow(results_df) > 0) head(results_df$index, top_n) else integer(0)
  ))
}

# Apply Change Detection to PM2.5 Data

# Extract PM2.5 data
pm25_data <- aggregatedData$pm25
time_points <- 1:length(pm25_data)

# Use dynamic parameters for all algorithms
target_value <- mean(pm25_data)
reference_value_k <- params$cusum_k_factor * sd(pm25_data)
threshold_h <- params$cusum_h_factor * sd(pm25_data)

# Apply CUSUM algorithm
cusum_results <- cusum_change_detection(
  pm25_data, 
  target_value, 
  threshold_h, 
  reference_value_k,
  params$top_cusum
)

# Apply EWMA algorithm
ewma_results <- ewma_change_detection(
  pm25_data, 
  params$ewma_lambda, 
  params$ewma_L,
  params$top_ewma
)

# Apply Rolling Window algorithm
rolling_results <- rolling_window_change_detection(
  pm25_data, 
  params$window_size, 
  params$window_threshold,
  params$top_rolling
)

# Find maximum PM2.5 value in dataset
max_pm25 <- max(pm25_data, na.rm = TRUE)
upper <- c(5, 15, 25, 35.5, 55.5, ceiling (max_pm25 ))

# Define AQI Bands for PM2.5

aqi_bands <- data.frame(
  category = c("Excellent", "Good", "Moderate", "Unhealthy for Sensitive Groups", "Unhealthy", "Hazardous"),
  lower = c(0, 5, 15, 25, 35.5, 55.5),
  upper = c(5, 15, 25, 35.5, 55.5, (max_pm25 * 1.1)),
  color = c("#00E400", "#92D050", "#FFFF00", "#FF7E00", "#FF0000", "#8F3F97")
)

# Create midpoints for labels
aqi_bands$midpoint <- (aqi_bands$lower + aqi_bands$upper) / 2

time_axis <- create_dynamic_time_labels(aggregatedData)
date_breaks <- time_axis$breaks
date_labels <- time_axis$labels
x_axis_title <- time_axis$x_axis_title

y_min <- 0
y_max <- max(pm25_data, na.rm = TRUE)*1.1

aqi_bands$category <- factor(aqi_bands$category, 
                             levels = c("Excellent", "Good", "Moderate", 
                                        "Unhealthy for Sensitive Groups", 
                                        "Unhealthy", "Hazardous"),
                             ordered = TRUE)

max_band_needed <- min(which(aqi_bands$upper >= y_max))
if(is.infinite(max_band_needed)) max_band_needed <- nrow(aqi_bands)

p1 <- ggplot() +
  geom_rect(data = aqi_bands[1:max_band_needed,], 
            aes(xmin = -Inf, xmax = Inf, 
                ymin = lower, ymax = upper, 
                fill = category),
            alpha = 0.3) +
  
  geom_line(data = data.frame(time = time_points, pm25 = pm25_data), 
            aes(x = time, y = pm25), 
            color = "black", size = 0.8) +
  
  geom_vline(data = data.frame(x = cusum_results$change_points, method = "CUSUM"),
             aes(xintercept = x, color = method, linetype = method), 
             size = 0.8, alpha = 0.8) +
  
  geom_vline(data = data.frame(x = ewma_results$change_points, method = "EWMA"),
             aes(xintercept = x, color = method, linetype = method), 
             size = 0.8, alpha = 0.8) +
  
  geom_vline(data = data.frame(x = rolling_results$change_points, method = "Rolling Window"),
             aes(xintercept = x, color = method, linetype = method), 
             size = 0.8, alpha = 0.8) +
  
  scale_fill_manual(values = setNames(aqi_bands$color, aqi_bands$category),
                    breaks = levels(aqi_bands$category)) +  
  scale_color_manual(values = c("CUSUM" = "red", 
                                "EWMA" = "blue", 
                                "Rolling Window" = "darkgreen")) +
  
  scale_linetype_manual(values = c("CUSUM" = "dashed", 
                                   "EWMA" = "dotted", 
                                   "Rolling Window" = "longdash")) +
  
  coord_cartesian(ylim = c(y_min, y_max)) +
  
  scale_x_continuous(
    breaks = date_breaks, 
    labels = date_labels,
    expand = c(0, 0)  
  ) +
  
  scale_y_continuous(
    breaks = upper,
    labels = upper,
    expand = c(0, 0)  
  ) +
  
  labs(title = "PM2.5 with Significant Change Detection",
       subtitle = paste0("Showing top ", params$top_cusum, " CUSUM, ", 
                         params$top_ewma, " EWMA, and ", 
                         params$top_rolling, " Rolling Window most significant changes"),
       x = x_axis_title, 
       y = expression(PM[2.5]~(μg/m^3))) +
  
  theme_minimal() +
  theme(
    plot.margin = margin(0.5, 0.5, 0, 0.5, "cm"),
    
    legend.position = "bottom",
    legend.box = "vertical",
    legend.margin = margin(0, 0, 0, 0),
    legend.spacing.y = unit(0.3, "cm"),
    legend.key.size = unit(1.5, "lines"),     
    legend.text = element_text(size = 12),     
    legend.title = element_text(size = 14, face = "bold"),  
    legend.box.just = "center",
    panel.grid.major = element_line(color = "gray90", size = 0.5),
    panel.grid.minor = element_line(color = "gray95", size = 0.25),
    plot.title = element_text(size = 18, face = "bold"),    
    plot.subtitle = element_text(size = 14),    
    axis.title = element_text(size = 16, face = "bold"),    
    axis.text = element_text(size = 14),        
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14)  
  ) +
  guides(
    color = guide_legend(
      title = "Detection Method",
      override.aes = list(
        linetype = c("dashed", "dotted", "longdash"),
        size = 1.5   
      ),
      order = 1,
      ncol = 3
    ),
    linetype = "none",
    fill = guide_legend(
      title = "AQI Category", 
      nrow = 1, 
      byrow = TRUE,
      override.aes = list(alpha = 0.7),
      order = 2
    )
  )

p2 <- ggplot(data = data.frame(
  time = time_points, 
  pm25 = pm25_data,
  ewma = ewma_results$ewma,
  ucl = ewma_results$ucl,
  lcl = ewma_results$lcl
)) +
  geom_line(aes(x = time, y = pm25), color = "gray50", alpha = 0.3, size = 0.5) +
  geom_line(aes(x = time, y = ewma), color = "blue", size = 1.0) +
  geom_line(aes(x = time, y = ucl), color = "red", linetype = "dashed", size = 0.8) +
  geom_line(aes(x = time, y = lcl), color = "red", linetype = "dashed", size = 0.8) +
  geom_point(data = data.frame(
    time = ewma_results$change_points,
    ewma = ewma_results$ewma[ewma_results$change_points]
  ), 
  aes(x = time, y = ewma), color = "red", size = 2.5) +
  scale_x_continuous(breaks = date_breaks, labels = date_labels) +
  coord_cartesian(ylim = c(y_min, y_max)) +
  labs(title = "EWMA Control Chart for PM2.5",
       subtitle = paste("λ =", params$ewma_lambda, ", L =", params$ewma_L, 
                        "| Points represent top", params$top_ewma, "most significant changes"),
       x = x_axis_title,
       y = expression(PM[2.5]~(μg/m^3))) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(size = 11, face = "bold"),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10),
    panel.grid.major = element_line(color = "gray90", size = 0.5),
    panel.grid.minor = element_line(color = "gray95", size = 0.25)
  )

cusum_max <- max(c(max(cusum_results$cusum_pos, na.rm = TRUE), 
                   max(cusum_results$cusum_neg, na.rm = TRUE)), 
                 na.rm = TRUE)
cusum_max <- cusum_max * 1.1 

p3 <- ggplot() +
  geom_line(data = data.frame(time = time_points, cusum = cusum_results$cusum_pos),
            aes(x = time, y = cusum, color = "Positive CUSUM"), size = 1.0) +
  geom_line(data = data.frame(time = time_points, cusum = cusum_results$cusum_neg),
            aes(x = time, y = cusum, color = "Negative CUSUM"), size = 1.0) +
  geom_hline(yintercept = threshold_h, linetype = "dashed", color = "black", size = 0.8) +
  geom_point(data = data.frame(
    time = cusum_results$change_points,
    y = rep(threshold_h, length(cusum_results$change_points))
  ), 
  aes(x = time, y = y), color = "darkred", size = 2.5) +
  scale_color_manual(values = c("Positive CUSUM" = "blue", "Negative CUSUM" = "red")) +
  scale_x_continuous(breaks = date_breaks, labels = date_labels) +
  coord_cartesian(ylim = c(0, cusum_max)) +
  labs(title = "CUSUM Control Chart for PM2.5",
       subtitle = paste("Target =", round(target_value, 2), 
                        "μg/m³, K =", round(reference_value_k, 2),
                        ", H =", round(threshold_h, 2),
                        "| Points represent top", params$top_cusum, "most significant changes"),
       x = x_axis_title,
       y = "CUSUM Statistic",
       color = "CUSUM Type") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    legend.key.size = unit(1.0, "lines"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(size = 11, face = "bold"),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10),
    panel.grid.major = element_line(color = "gray90", size = 0.5),
    panel.grid.minor = element_line(color = "gray95", size = 0.25)
  ) +
  guides(
    color = guide_legend(
      override.aes = list(size = 1.2),  
      ncol = 2
    )
  )

hour_day_counts <- aggregatedData %>%
  mutate(hour_bin = hour %/% 3 * 3) %>%
  group_by(hour_bin, day_of_week) %>%
  summarise(
    avg_pm25 = mean(pm25, na.rm = TRUE),
    .groups = 'drop'
  )

# Create hour labels
hour_labels <- paste0(
  formatC(seq(0, 21, by = 3), width = 2, flag = "0"), "-", 
  formatC(seq(3, 24, by = 3), width = 2, flag = "0")
)

p4 <- ggplot(hour_day_counts, aes(x = day_of_week, y = factor(hour_bin, 
                                                              levels = seq(21, 0, by = -3)), 
                                  fill = avg_pm25)) +
  geom_tile() +
  scale_fill_gradientn(
    colors = aqi_bands$color,
    values = scales::rescale(upper),
    limits = c(0, max(hour_day_counts$avg_pm25)),
    name = "Average PM2.5 (μg/m³)"
  ) +
  scale_y_discrete(labels = hour_labels) +
  labs(title = "Average PM2.5 by Time of Day and Day of Week",
       subtitle = "Shows patterns in air quality measurements",
       x = "Day of Week", 
       y = "Hour of Day") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),     
    plot.subtitle = element_text(size = 14),          
    axis.title = element_text(size = 16, face = "bold"),     
    axis.text.x = element_text(size = 14, angle = 0),        
    axis.text.y = element_text(size = 14),                   
    legend.title = element_text(size = 14, face = "bold"),   
    legend.text = element_text(size = 12),                   
    legend.key.size = unit(1.5, "lines"),                    
    legend.position = "right",
    panel.grid = element_blank(),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
  )

p5 <- ggplot() +
  geom_histogram(data = head(cusum_results$change_points_all, params$top_cusum),
                 aes(x = magnitude, fill = "CUSUM"), 
                 alpha = 0.7, bins = 15, position = "identity", color = "white", size = 0.2) +
  geom_histogram(data = head(ewma_results$change_points_all, params$top_ewma),
                 aes(x = magnitude, fill = "EWMA"), 
                 alpha = 0.7, bins = 15, position = "identity", color = "white", size = 0.2) +
  geom_histogram(data = head(rolling_results$change_points_all, params$top_rolling),
                 aes(x = magnitude, fill = "Rolling Window"), 
                 alpha = 0.7, bins = 15, position = "identity", color = "white", size = 0.2) +
  scale_fill_manual(values = c("CUSUM" = "red", "EWMA" = "blue", "Rolling Window" = "darkgreen")) +
  labs(title = "Distribution of Change Magnitudes by Detection Method",
       subtitle = "Higher values indicate more significant changes",
       x = "Change Magnitude", 
       y = "Count",
       fill = "Detection Method") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10),
    axis.title = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 9),
    legend.position = "bottom",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    legend.key.size = unit(1.0, "lines"),
    panel.grid.major = element_line(color = "gray90", size = 0.5),
    panel.grid.minor = element_line(color = "gray95", size = 0.25)
  ) +
  guides(
    fill = guide_legend(
      override.aes = list(alpha = 0.9),  # More visible in legend
      ncol = 3
    )
  )

# Function to create appropriate time labels based on dataset duration
create_dynamic_time_labels <- function(data) {
  total_duration_hours <- as.numeric(difftime(max(data$ts), min(data$ts), units="hours"))
  n_breaks <- min(10, max(5, ceiling(length(data$ts) / 30)))
  date_step <- max(1, round(length(data$ts) / n_breaks))
  date_breaks <- seq(1, length(data$ts), by = date_step)
  
  # Format based on duration
  if (total_duration_hours < 12) {
    date_labels <- format(data$ts[date_breaks], "%H:%M")
    x_axis_title <- "Time of Day"
  } else if (total_duration_hours < 72) {
    date_labels <- format(data$ts[date_breaks], "%d %b\n%H:%M")
    x_axis_title <- "Date and Time"
  } else if (total_duration_hours < 720) {
    date_labels <- format(data$ts[date_breaks], "%d %b")
    x_axis_title <- "Date"
  } else {
    date_labels <- format(data$ts[date_breaks], "%b %Y")
    x_axis_title <- "Month"
  }
  
  return(list(
    breaks = date_breaks,
    labels = date_labels,
    x_axis_title = x_axis_title
  ))
}

grid.arrange(p1, p2, p3, p4, p5, 
             layout_matrix = rbind(c(1, 1, 1),
                                   c(2, 2, 3),
                                   c(4, 5, 5)),
             widths = c(1, 1, 1),
             heights = c(1.5, 1, 1))

all_change_points <- sort(unique(c(
  cusum_results$change_points,
  ewma_results$change_points,
  rolling_results$change_points
)))

change_points_table <- data.frame(
  Time_Index = all_change_points,
  Timestamp = aggregatedData$ts[all_change_points],
  Date = as.Date(aggregatedData$ts[all_change_points]),
  Hour = hour(aggregatedData$ts[all_change_points]),
  Day_of_Week = aggregatedData$day_of_week[all_change_points],
  Time_of_Day = aggregatedData$time_of_day[all_change_points],
  Is_Weekend = aggregatedData$is_weekend[all_change_points],
  PM25_Value = pm25_data[all_change_points],
  Detected_By = sapply(all_change_points, function(cp) {
    methods <- c()
    if (cp %in% cusum_results$change_points) methods <- c(methods, "CUSUM")
    if (cp %in% ewma_results$change_points) methods <- c(methods, "EWMA")
    if (cp %in% rolling_results$change_points) methods <- c(methods, "Rolling")
    paste(methods, collapse = ", ")
  }),
  AQI_Band = sapply(pm25_data[all_change_points], function(val) {
    band_idx <- which(aqi_bands$lower <= val & aqi_bands$upper >= val)
    if (length(band_idx) > 0) return(as.character(aqi_bands$category[band_idx]))
    else return("Unknown")
  })
)

if (length(all_change_points) > 0) {
  change_points_table$Previous_Value <- NA
  change_points_table$Change_Magnitude <- NA
  change_points_table$Change_Percentage <- NA
  change_points_table$Direction <- NA
  
  for (i in 1:nrow(change_points_table)) {
    idx <- change_points_table$Time_Index[i]
    if (idx > 1) {
      prev_val <- pm25_data[idx-1]
      change_points_table$Previous_Value[i] <- prev_val
      
      change <- pm25_data[idx] - prev_val
      change_points_table$Change_Magnitude[i] <- change
      
      if (prev_val > 0) {
        change_points_table$Change_Percentage[i] <- change / prev_val * 100
      } else {
        change_points_table$Change_Percentage[i] <- NA
      }
      
      # Determine direction
      change_points_table$Direction[i] <- ifelse(change > 0, "Increase", "Decrease")
    }
  }
}

change_patterns <- change_points_table %>%
  group_by(Time_of_Day, Day_of_Week) %>%
  summarise(
    Count = n(),
    Avg_Magnitude = mean(abs(Change_Magnitude), na.rm = TRUE),
    Pct_Increase = mean(Direction == "Increase", na.rm = TRUE) * 100,
    Avg_PM25 = mean(PM25_Value, na.rm = TRUE),
    .groups = 'drop'
  )

ggsave("pm25_change_detection.png", p1, width = 12, height = 8, dpi = 300)
ggsave("EWMA_control_chart.png", p2, width = 12, height = 8, dpi = 300)
ggsave("CUSUM_control_chart.png", p3, width = 12, height = 8, dpi = 300)
ggsave("heat_map.png", p4, width = 12, height = 8, dpi = 300)
ggsave("change_magnitude.png", p5, width = 12, height = 8, dpi = 300)

combined_plots <- arrangeGrob(p1, p2, p3, p4, p5, 
                              layout_matrix = rbind(c(1, 1, 1),
                                                    c(2, 2, 3),
                                                    c(4, 5, 5)),
                              widths = c(1, 1, 1),
                              heights = c(1.5, 1, 1))

ggsave("combined_change_detection.png", combined_plots, width = 18, height = 12, dpi = 300)

if(nrow(change_points_table) > 0) {
  write.csv(head(change_points_table, 15), "change_points_sample.csv", row.names = FALSE)
  write.csv(change_points_table, "change_points_table.csv", row.names = FALSE)
}

# Save change patterns analysis
write.csv(change_patterns, "change_patterns.csv", row.names = FALSE)

cat("\n=== Sample Change Points ===\n")
if(nrow(change_points_table) > 0) {
  print(head(change_points_table[, c("Timestamp", "PM25_Value", "Detected_By", "AQI_Band", "Direction")], 10))
} else {
  cat("No change points detected.\n")
}

cat("\n=== Change Patterns by Time ===\n")
print(change_patterns)
