# Load required libraries
library(httr)
library(jsonlite)
library(ggplot2)
library(readr)
library(here)
library(dplyr)
library(tidyr)

# Load credentials
weather_cred <- read_csv(file = here("weather_credentials.csv"))
api_key <- weather_cred$api_key


# Function to get current weather
get_current_weather <- function(city, api_key) {
  base_url <- "http://api.openweathermap.org/data/2.5/weather"
  response <- GET(base_url, query = list(q = city, appid = api_key, units = "metric"))
  
  if (status_code(response) == 200) {
    weather_data <- content(response, as = "parsed")
    return(weather_data)
  } else {
    stop("Error: Unable to fetch current weather data")
  }
}

# Function to compare weather parameters across cities
compare_weather_across_cities <- function(cities, api_key) {
  weather_data <- lapply(cities, function(city) get_current_weather(city, api_key))
  
  df <- data.frame(
    City = sapply(weather_data, function(x) x$name),
    Temperature = sapply(weather_data, function(x) x$main$temp),
    Feels_Like = sapply(weather_data, function(x) x$main$feels_like),
    Min_Temperature = sapply(weather_data, function(x) x$main$temp_min),
    Max_Temperature = sapply(weather_data, function(x) x$main$temp_max),
    Humidity = sapply(weather_data, function(x) x$main$humidity),
    Wind_Speed = sapply(weather_data, function(x) x$wind$speed),
    Cloudiness = sapply(weather_data, function(x) x$clouds$all),
    stringsAsFactors = FALSE
  )
  
  return(df)
}

# Test the function with major European cities
cities <- c("London", "Paris", "Berlin", "Madrid", "Rome", "Vienna", "Bern", "Stockholm")
comparison_data <- compare_weather_across_cities(cities, api_key)
print(comparison_data)

# Reshape the data for plotting
df_long <- comparison_data %>%
  pivot_longer(cols = c("Temperature","Feels_Like", "Min_Temperature", "Max_Temperature", "Humidity", "Wind_Speed", "Cloudiness"),
               names_to = "Parameter", values_to = "Value")


# Sort the data by Humidity
comparison_data_sorted <- comparison_data %>%
  arrange(desc(Temperature))

# Reorder City factor levels based on Humidity
df_long <- df_long %>%
  mutate(City = factor(City, levels = comparison_data_sorted$City))

# Visualize the comparison
comparison_plot <- ggplot(df_long, aes(x = City, y = Value, fill = Parameter)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "City", y = "Value", title = "Weather Comparison Across Major European Cities") +
  scale_fill_manual(name = "Parameter",
                    values = c("Temperature" = "blue", "Feels_Like" = "lightblue",
                               "Min_Temperature" = "skyblue", "Max_Temperature" = "darkblue", 
                               "Humidity" = "green", "Wind_Speed" = "red", "Cloudiness" = "grey")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Additional analysis and insights

summary_stats <- comparison_data %>%
  summarise(across(where(is.numeric), list(mean = mean, sd = sd), na.rm = TRUE))
print(summary_stats)

# Visualize summary statistics
summary_long <- summary_stats %>%
  pivot_longer(cols = everything(), names_to = c("Parameter", ".value"), names_pattern = "(.*)_(.*)")

summary_plot <- ggplot(summary_long, aes(x = Parameter, y = mean, fill = Parameter)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, position = position_dodge(0.9)) +
  labs(x = "Parameter", y = "Mean Value", title = "Summary Statistics of Weather Parameters Across Major European Cities") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save the comparison data to a CSV file for further analysis
write_csv(comparison_data, here("weather_comparison_data.csv"))

# Save the plots
ggsave("Plots/comparison_plot.png", comparison_plot, width = 8, height = 6, dpi = 300)
ggsave("Plots/summary_plot.png", summary_plot, width = 8, height = 6, dpi = 300)
