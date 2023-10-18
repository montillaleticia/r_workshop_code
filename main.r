# Loading necessary libraries
library(ggplot2)

# Fetching data
data <- airquality

# Clean Data
# 
# This function removes rows with missing values from the input data frame.
# 
# Args:
#   data: The input data frame to be cleaned.
# 
# Returns:
#   A cleaned data frame with missing values removed.

clean_data <- function(data) {
  data[complete.cases(data), ]
}

data <- clean_data(data)

# Function to calculate the average Solar Radiation for a given month
# 
# Args:
#   data: The input data frame.
#   month: The month for which to calculate the average.
# 
# Returns:
#   The average solar radiation for the specified month.
calculate_avg_solar <- function(data, month) {
  avg_solar <- mean(data$Solar.R[data$Month == month])
  return(avg_solar)
}

# Calculate Correlation
# 
# This function calculates the correlation between ozone and solar radiation for a specific month.
# 
# Args:
#   data: The input data frame.
#   month: The month for which to calculate the correlation (e.g., 5 for May).
# 
# Returns:
#   The correlation coefficient between ozone and solar radiation for the specified month.
calculate_correlation <- function(data, month) {
  correlation <- cor(data$Ozone[data$Month == month], data$Solar.R[data$Month == month])
  return(correlation)
}

# Generate and Save Plot
# 
# This function generates a scatter plot of ozone vs. solar radiation for a specific month
# and saves it to a file.
# 
# Args:
#   data: The input data frame.
#   month: The month for which to generate the plot (e.g., 5 for May).
#   title: The title of the plot.
#   filename: The name of the output file.
generate_and_save_plot <- function(data, month, title, filename) {
  plot <- ggplot(data[data$Month == month, ], aes(x = Solar.R, y = Ozone)) +
    geom_point(aes(shape = factor(Month))) +
    ggtitle(title)
  ggsave(filename, plot)
}

# Calculate average Solar Radiation and print to console
months <- c(5, 6, 7, 8, 9)

for (month in months) {
  avg_solar <- calculate_avg_solar(data, month)
  cat(paste("Average Solar Radiation for ", month, ": ", avg_solar, "\n"))
}

# Calculate correlation and print to console
for (month in months) {
  correlation <- calculate_correlation(data, month)
  cat(paste("Correlation for ", month, ": ", correlation, "\n"))
}

# Visualization
plot_shapes <- ifelse(correlation_May > 0.5, 19, ifelse(correlation_June > 0.5, 17, 15))

# Save Plots
for (month in months) {
  generate_and_save_plot(data, month, month.name[month], paste("plot_", tolower(month.name[month]), ".png", sep = ""))
}

# Save data
write.csv(data, "cleaned_data.csv", row.names = FALSE)
