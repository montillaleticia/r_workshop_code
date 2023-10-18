# Loading necessary libraries
library(ggplot2)

# Fetching data
data <- airquality

# Data Cleaning - Remove rows with NA
clean_data <- function(data) {
  data[complete.cases(data), ]
}

data <- clean_data(data)

# Function to calculate the average Solar Radiation for a given month
calculate_avg_solar <- function(data, month) {
  avg_solar <- mean(data$Solar.R[data$Month == month])
  return(avg_solar)
}

# Function to calculate the correlation between Ozone and Solar Radiation for a given month
calculate_correlation <- function(data, month) {
  correlation <- cor(data$Ozone[data$Month == month], data$Solar.R[data$Month == month])
  return(correlation)
}

# Function to generate and save a plot
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
