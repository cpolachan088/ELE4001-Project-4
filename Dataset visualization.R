library(ggplot2) # Load necessary libraries
library(lubridate)
library(dplyr)

dataset_name <- "cooking" 

# Function to visualize PM2.5 from any dataset with labeled background bands and average line on right
visualize_pm25_complete <- function(file_path, date_format = "%d/%m/%Y %H:%M", aggregate_minutes = 1) {
  # Read the data
  data <- read.csv(file_path, header = TRUE)
  
  # Check if the dataset contains PM2.5
  if(!"pm25" %in% colnames(data)) {
    stop("Error: The dataset does not contain a 'pm25' column.")
  }
  
  # Check if the dataset contains a timestamp column
  if(!"ts" %in% colnames(data)) {
    stop("Error: The dataset does not contain a 'ts' column for timestamp.")
  }
  
  # Convert any "No data" or NA in pm25 to NA
  data$pm25 <- as.numeric(as.character(data$pm25))
  
  # Handle missing values
  if(sum(is.na(data$pm25)) > 0) {
    data$pm25[is.na(data$pm25)] <- mean(data$pm25, na.rm = TRUE)
  }
  
  # Try to parse the timestamp
  tryCatch({
    data$ts <- as.POSIXct(data$ts, format = date_format, tz = "GMT")
  }, error = function(e) {
    # If the first format fails, try another common format
    tryCatch({
      data$ts <- as.POSIXct(data$ts, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
    }, error = function(e) {
      stop("Error: Could not parse the timestamp. Please specify the correct date_format parameter.")
    })
  })
  
  # Aggregate data if requested
  if(aggregate_minutes > 0) {
    # Create period column for aggregation
    data$period <- as.numeric(difftime(data$ts, min(data$ts), units="mins")) %/% aggregate_minutes
    
    # Aggregate data
    data <- data %>%
      group_by(period) %>%
      summarise(
        ts = first(ts),
        pm25 = mean(pm25, na.rm = TRUE)
      )
  }
  
  # Define AQI bands based on WHO Guidelines for PM2.5
  aqi_bands <- data.frame(
    category = c("Excellent", "Good", "Moderate","Unhealthy for\nSensitive Groups", "Unhealthy", "Hazardous"),
    lower = c(0, 5, 15, 25, 35.5, 55.5),
    upper = c(5, 15, 25, 35.5, 55.5, max(75, ceiling(max(data$pm25 * 1.1)))),
    color = c("#00E400", "#92D050", "#FFFF00", "#FF7E00", "#FF0000", "#7E3F97")
  )
  
  # Calculate average PM2.5 for reference line
  avg_pm25 <- mean(data$pm25, na.rm = TRUE)
  
  # Calculate the maximum PM2.5 value for the plot
  max_pm25 <- max(data$pm25, na.rm = TRUE) * 1.05
  
  # Create the plot
  p <- ggplot(data, aes(x = ts, y = pm25)) +
    # Add background bands for AQI levels
    mapply(function(low, high, col) {
      geom_rect(aes(xmin = min(data$ts), xmax = max(data$ts), ymin = low, ymax = high),
                fill = col, alpha = 0.2)
    }, aqi_bands$lower, aqi_bands$upper, aqi_bands$color)
  
  # Add the main PM2.5 time series line  
  p <- p + geom_line(color = "blue", size = 1) +
    
    # Add average line
    geom_hline(yintercept = avg_pm25, linetype = "dashed", color = "purple", size = 1) +
    
    # Add average annotation on the right side
    annotate("text", x = max(data$ts), y = avg_pm25, 
             label = paste("Average:", round(avg_pm25, 1), "μg/m³"), 
             hjust = -0.1, vjust = 0.5, color = "purple", fontface = "bold", size = 5)
  
  # Add AQI band labels (right side of the plot)
  for(i in 1:nrow(aqi_bands)) {
    p <- p + annotate("text", x = max(data$ts), 
                      y = (aqi_bands$lower[i] + aqi_bands$upper[i])/2,
                      label = aqi_bands$category[i], 
                      hjust = -0.1, 
                      color = colorspace::darken(aqi_bands$color[i], 0.3), 
                      fontface = "bold",
                      size = 5)
  }
  
  # Add horizontal lines at each threshold for extra clarity
  for(threshold in aqi_bands$lower[-1]) {  # Skip the first (0)
    p <- p + geom_hline(yintercept = threshold, linetype = "dotted", 
                        color = "darkgrey", size = 0.8)
  }
  
  # Add labels and theme
  p <- p + labs(
    title = "PM2.5 Time Series",
    subtitle = paste("Data from", format(min(data$ts), "%d %b %Y %H:%M"), "to", format(max(data$ts), "%d %b %Y %H:%M")),
    x = "Time",
    y = "PM2.5 (μg/m³)"
  ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 22, face = "bold"),
      plot.subtitle = element_text(size = 16),
      axis.title.x = element_text(size = 18, margin = margin(t = 15, r = 0, b = 0, l = 0)),
      axis.title.y = element_text(size = 18, margin = margin(t = 0, r = 15, b = 0, l = 0)),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 16, face = "bold"),
      axis.text.y = element_text(size = 16, face = "bold"),
      panel.grid.major = element_line(color = "gray80", size = 0.5),
      panel.grid.minor = element_line(color = "gray90", size = 0.25),
      plot.margin = margin(r = 140, l = 30, b = 30, t = 30, unit = "pt")  # Extra margin on right for labels
    ) +
    
    # Force specific breaks on y-axis to show band thresholds
    scale_y_continuous(
      breaks = c(0, aqi_bands$lower[-1], round(max_pm25, 0)),
      labels = as.character(c(0, aqi_bands$lower[-1], round(max_pm25, 0)))
    ) +
    
    coord_cartesian(clip = "off")  # Allow annotations to extend beyond plot area
  
  # Save plot to file with higher resolution
  output_file <- paste0(gsub("\\.csv$", "", basename(file_path)), "_pm25_complete_plot.png")
  ggsave(output_file, plot = p, width = 14, height = 8, dpi = 300)
  
  cat("Plot has been saved as:", output_file, "\n")
  
  # Return the plot
  return(p)
}

# Read and preprocess data
fileName <- paste0("C:/Users/Christo/Desktop/FYP/R Code/DATASETS/", dataset_name,".csv")

pm25_plot <- visualize_pm25_complete(fileName)
print(pm25_plot)  # Display the plot
