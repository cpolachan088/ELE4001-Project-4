library(forecast) # Load necessary libraries
library(xts) 
library(tidyverse) 
library(Metrics) 
library(knitr)
library(lubridate)
library(caret)
library(corrplot)
library(randomForest)
library(gridExtra) 

# Parameters
aggregationMinutes <- 60

# Read and preprocess data
fileName <- "C:/Users/Christo/Desktop/FYP/R Code/DATASETS/gams-dataset.csv"
pollutionData <- read.csv(fileName, header = TRUE)

# Convert instances of "No data" to NA and handle missing values
pollutionData[-1] <- lapply(pollutionData[-1], function(x) { 
  x <- as.numeric(as.character(x))
  return(ifelse(is.na(x), mean(x, na.rm = TRUE), x))
})

# Convert timestamp and aggregate data
#pollutionData$ts <- as.POSIXct(pollutionData$ts, format="%Y-%m-%d %H:%M:%S", tz = "GMT")
pollutionData$ts <- as.POSIXct(pollutionData$ts, format="%d/%m/%Y %H:%M", tz = "GMT")

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

# Create features with seasonal components
modelData <- aggregatedData %>%
  mutate(
    hour = hour(ts),
    day_of_week = wday(ts),
    month = month(ts),
    sin_hour = sin(2 * pi * hour / 24),
    cos_hour = cos(2 * pi * hour / 24),
    sin_month = sin(2 * pi * month / 12),
    cos_month = cos(2 * pi * month / 12)
  ) %>%
  select(-ts, -period)

# Function to fix multicollinearity
fix_multicollinearity <- function(data) {
  # Find and remove highly correlated predictors
  correlation_matrix <- cor(data, use = "complete.obs")
  high_correlations <- findCorrelation(correlation_matrix, cutoff = 0.9)
  
  if (length(high_correlations) > 0) {
    cat("Removing", length(high_correlations), "variables with high correlation:\n")
    cat(names(data)[high_correlations], "\n\n")
    data <- data[, -high_correlations]
  }
  
  return(data)
}

# Create a function to evaluate predictability of each variable with multicollinearity fix
evaluate_variable_predictability <- function(data, target_variable, forecast_horizon = 1) {
  # Create subset without the target to avoid perfect correlation
  predictors_data <- data[, !names(data) %in% target_variable]
  
  # Fix multicollinearity in predictors
  clean_predictors <- fix_multicollinearity(predictors_data)
  
  # Add target back
  lag_data <- cbind(clean_predictors, data[target_variable])
  names(lag_data)[ncol(lag_data)] <- target_variable
  
  # Split data into training and testing
  train_size <- floor(0.8 * nrow(lag_data))
  train_data <- lag_data[1:train_size, ]
  test_data <- lag_data[(train_size+1):nrow(lag_data), ]
  
  # Create formula for all features except the target
  predictors <- setdiff(names(lag_data), target_variable)
  formula <- as.formula(paste(target_variable, "~", paste(predictors, collapse = " + ")))
  
  # Train model with cross-validation
  ctrl <- trainControl(method = "cv", number = 5)
  set.seed(123)  # For reproducibility
  
  # Use more robust method 
  model <- tryCatch({
    train(formula, data = train_data, method = "lm", trControl = ctrl)
  }, error = function(e) {
    # Fallback to simple lm if train fails
    cat("Warning: Cross-validation failed, using simple lm for", target_variable, "\n")
    lm(formula, data = train_data)
  })
  
  # Make predictions
  predictions <- predict(model, newdata = test_data)
  actual <- test_data[[target_variable]]
  
  # Calculate metrics
  rmse_val <- rmse(actual, predictions)
  # Handle division by zero in MAPE calculation
  mape_val <- mean(abs((actual - predictions) / (actual + 1e-10)), na.rm = TRUE) * 100
  
  # Get R-squared
  r_squared <- if(class(model)[1] == "train") {
    summary(model$finalModel)$r.squared
  } else {
    summary(model)$r.squared
  }
  
  # Return results
  return(list(
    variable = target_variable,
    rmse = rmse_val,
    mape = mape_val,
    r_squared = r_squared,
    actual = actual,
    predictions = predictions
  ))
}

# Evaluate each variable as a potential prediction target
target_variables <- c("pm25", "pm10", "co2", "temperature", "humidity", "voc")
results_list <- list()
plot_list <- list()  # For storing individual plots

# Create proper variable labels for plots
variable_labels <- c(
  "pm25" = "PM2.5",
  "pm10" = "PM10",
  "co2" = "CO₂",
  "temperature" = "Temperature",
  "humidity" = "Humidity", 
  "voc" = "VOC"
)

cat("Evaluating predictability of each variable...\n")
for (var in target_variables) {
  cat("Testing", var, "as prediction target...\n")
  results_list[[var]] <- evaluate_variable_predictability(modelData, var)
  
  # Create plot of actual vs predicted for each variable
  plot_data <- data.frame(
    Index = 1:length(results_list[[var]]$actual),
    Actual = results_list[[var]]$actual,
    Predicted = results_list[[var]]$predictions
  )
  
  p <- ggplot(plot_data, aes(x = Index)) +
    geom_line(aes(y = Actual, color = "Actual"), linewidth = 1) +
    geom_line(aes(y = Predicted, color = "Predicted"), linewidth = 1) +
    labs(title = paste("Actual vs Predicted:", variable_labels[var]),
         y = variable_labels[var],
         x = "Time Index") +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.text = element_text(size = 10),
      legend.title = element_blank()
    ) +
    scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red"))
  
  plot_list[[var]] <- p
}

# Combine results into a data frame
results_df <- do.call(rbind, lapply(results_list, function(x) {
  data.frame(
    Variable = x$variable,
    RMSE = x$rmse,
    MAPE = x$mape,
    R_Squared = x$r_squared
  )
}))

# Standardize RMSE to make it comparable across variables with different scales
results_df$RMSE_Standardized <- results_df$RMSE / sapply(target_variables, function(var) {
  sd(modelData[[var]], na.rm = TRUE)
})

# Calculate a composite score (lower is better)
# Normalize each component to 0-1 range for fair weighting
normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

results_df$Normalized_RMSE <- normalize(results_df$RMSE_Standardized)
results_df$Normalized_MAPE <- normalize(results_df$MAPE)
results_df$Normalized_R2 <- 1 - normalize(results_df$R_Squared)  # Invert so lower is better

results_df$Composite_Score <- (results_df$Normalized_RMSE + results_df$Normalized_MAPE + results_df$Normalized_R2) / 3

# Sort by composite score (lower is better)
results_df <- results_df[order(results_df$Composite_Score), ]

# Add nicer variable labels for plots
results_df$VariableLabel <- variable_labels[results_df$Variable]

# Print the results
cat("\n==== Results: Variable Predictability ====\n")
print(kable(results_df[, c("Variable", "RMSE", "RMSE_Standardized", "MAPE", "R_Squared", "Composite_Score")], digits = 3))

# Create improved bar charts with better formatting and more readable axes
# R-Squared plot
p1 <- ggplot(results_df, aes(x = reorder(VariableLabel, -R_Squared), y = R_Squared)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = sprintf("%.3f", R_Squared)), vjust = -0.5, size = 4) +
  labs(title = "R-Squared by Variable",
       subtitle = "Higher is better",
       x = "",
       y = "R-Squared") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 13, face = "bold", color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    panel.grid.major.y = element_line(color = "gray90"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.line = element_line(color = "black", size = 0.5)
  ) +
  ylim(0, max(results_df$R_Squared) * 1.15) # Add some space for labels

# Standardized RMSE plot
p2 <- ggplot(results_df, aes(x = reorder(VariableLabel, RMSE_Standardized), y = RMSE_Standardized)) +
  geom_bar(stat = "identity", fill = "coral") +
  geom_text(aes(label = sprintf("%.3f", RMSE_Standardized)), vjust = -0.5, size = 4) +
  labs(title = "Standardized RMSE by Variable",
       subtitle = "Lower is better",
       x = "",
       y = "Standardized RMSE") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 13, face = "bold", color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    panel.grid.major.y = element_line(color = "gray90"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.line = element_line(color = "black", size = 0.5)
  ) +
  ylim(0, max(results_df$RMSE_Standardized) * 1.15) # Add some space for labels

# MAPE plot
p3 <- ggplot(results_df, aes(x = reorder(VariableLabel, MAPE), y = MAPE)) +
  geom_bar(stat = "identity", fill = "forestgreen") +
  geom_text(aes(label = sprintf("%.3f", MAPE)), vjust = -0.5, size = 4) +
  labs(title = "MAPE by Variable",
       subtitle = "Lower is better",
       x = "",
       y = "MAPE (%)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 13, face = "bold", color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    panel.grid.major.y = element_line(color = "gray90"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.line = element_line(color = "black", size = 0.5)
  ) +
  ylim(0, max(results_df$MAPE) * 1.15) # Add some space for labels

# Composite Score plot
p4 <- ggplot(results_df, aes(x = reorder(VariableLabel, Composite_Score), y = Composite_Score)) +
  geom_bar(stat = "identity", fill = "purple") +
  geom_text(aes(label = sprintf("%.3f", Composite_Score)), vjust = -0.5, size = 4) +
  labs(title = "Composite Score by Variable",
       subtitle = "Lower is better",
       x = "",
       y = "Composite Score") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 13, face = "bold", color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    panel.grid.major.y = element_line(color = "gray90"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.line = element_line(color = "black", size = 0.5)
  ) +
  ylim(0, max(results_df$Composite_Score) * 1.15) # Add some space for labels

# Create a correlation matrix of all variables with improved readability
correlation_matrix <- cor(modelData, use = "complete.obs")

# Arrange all plots in a single window with better layout
dev.new(width = 15, height = 12)
metrics_title <- grid::textGrob("Variable Analysis Metrics", gp = grid::gpar(fontsize = 20, fontface = "bold"))
grid.arrange(
  p1, p2, p3, p4, 
  ncol = 2,
  top = metrics_title
)

# Arrange all the actual vs predicted plots in a single window with better layout
dev.new(width = 15, height = 12)
title_grob <- grid::textGrob("Actual vs Predicted Values by Variable", gp = grid::gpar(fontsize = 20, fontface = "bold"))

# Convert plot_list to a regular list that can be passed to grid.arrange
grid_plots <- list()
for (i in seq_along(plot_list)) {
  grid_plots[[i]] <- plot_list[[names(plot_list)[i]]]
}

# Now arrange with proper layout
grid.arrange(grobs = grid_plots, ncol = 2, top = title_grob)

# Final recommendation
cat("\n==== Final Recommendation ====\n")
cat("Based on predictability metrics, the recommended target variable for predictions is:\n")
cat(results_df$Variable[1], "\n\n")

cat("Variables ranked from best to worst for prediction:\n")
for (i in 1:nrow(results_df)) {
  cat(i, ". ", variable_labels[results_df$Variable[i]], 
      " (R² = ", round(results_df$R_Squared[i], 3),
      ", RMSE = ", round(results_df$RMSE[i], 3),
      ", MAPE = ", round(results_df$MAPE[i], 3), "%)\n", sep = "")
}

# Save correlation matrix to a file with improved formatting
png("correlation_matrix.png", width = 800, height = 800, res = 100)
corrplot(correlation_matrix, method = "circle", type = "upper", 
         tl.col = "black", tl.cex = 1.2, tl.srt = 45, addCoef.col = "black",
         title = "Correlation Matrix of Variables",
         mar = c(0,0,2,0))
dev.off()

# Create a comprehensive final plot for variable recommendation with improved readability
p_final <- ggplot(results_df, aes(x = reorder(VariableLabel, -R_Squared))) +
  geom_bar(aes(y = R_Squared), stat = "identity", fill = "steelblue", alpha = 0.7) +
  geom_line(aes(y = RMSE_Standardized, group = 1), color = "red", size = 1.5) +
  geom_point(aes(y = RMSE_Standardized), color = "red", size = 3) +
  geom_text(aes(y = R_Squared, label = sprintf("%.2f", R_Squared)), 
            vjust = -0.5, color = "blue", size = 4.5, fontface = "bold") +
  geom_text(aes(y = RMSE_Standardized, label = sprintf("%.2f", RMSE_Standardized)), 
            vjust = 1.5, color = "red", size = 4.5, fontface = "bold") +
  scale_y_continuous(
    name = "R-Squared (bars)",
    sec.axis = sec_axis(~., name = "Standardized RMSE (line)")
  ) +
  labs(title = "Variable Selection Analysis",
       subtitle = "Higher R-Squared (bars) is better; Lower RMSE (line) is better",
       x = "") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.title.y.left = element_text(color = "steelblue", size = 14, face = "bold"),
    axis.title.y.right = element_text(color = "red", size = 14, face = "bold"),
    axis.text.x = element_text(size = 13, face = "bold", color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    panel.grid.major.y = element_line(color = "gray90"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.line = element_line(color = "black", size = 0.5)
  )

# Save the final recommendation plot with better resolution
png("variable_recommendation.png", width = 1000, height = 600, res = 100)
print(p_final)
dev.off()

# Display the final recommendation plot
dev.new(width = 12, height = 8)
print(p_final)

cat("\nAnalysis complete. Visualizations displayed and saved.\n")

# Save individual metric plots with improved resolution
ggsave("r_squared_by_variable.png", plot = p1, width = 10, height = 6, dpi = 300)
ggsave("standardized_rmse_by_variable.png", plot = p2, width = 10, height = 6, dpi = 300)
ggsave("mape_by_variable.png", plot = p3, width = 10, height = 6, dpi = 300)
ggsave("composite_score_by_variable.png", plot = p4, width = 10, height = 6, dpi = 300)

# Save the grid of metric plots with better resolution
metrics_title2 <- grid::textGrob("Variable Analysis Metrics", gp = grid::gpar(fontsize = 20, fontface = "bold"))
metric_grid <- grid.arrange(p1, p2, p3, p4, ncol = 2, top = metrics_title2)
ggsave("all_metrics_grid.png", plot = metric_grid, width = 15, height = 12, dpi = 300)

# Save each actual vs predicted plot with better resolution
for (var in target_variables) {
  ggsave(paste0("actual_vs_predicted_", var, ".png"), 
         plot = plot_list[[var]], 
         width = 8, height = 5, dpi = 300)
}

# Save the grid of actual vs predicted plots with better resolution
title_grob2 <- grid::textGrob("Actual vs Predicted Values by Variable", gp = grid::gpar(fontsize = 20, fontface = "bold"))

# Convert plot_list to a regular list that can be passed to grid.arrange
grid_plots2 <- list()
for (i in seq_along(plot_list)) {
  grid_plots2[[i]] <- plot_list[[names(plot_list)[i]]]
}

actual_vs_pred_grid <- grid.arrange(grobs = grid_plots2, ncol = 2, top = title_grob2)
ggsave("all_actual_vs_predicted_grid.png", plot = actual_vs_pred_grid, width = 15, height = 12, dpi = 300)

cat("\nAll plots have been saved as PNG files in the current working directory.\n")
