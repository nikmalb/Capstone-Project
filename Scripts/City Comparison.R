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
    Humidity = sapply(weather_data, function(x) x$main$humidity),
    Wind_Speed = sapply(weather_data, function(x) x$wind$speed),
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
  pivot_longer(cols = c("Temperature", "Humidity", "Wind_Speed"), names_to = "Parameter", values_to = "Value")


# Sort the data by Humidity
comparison_data_sorted <- comparison_data %>%
  arrange(desc(Humidity))

# Reorder City factor levels based on Humidity
df_long <- df_long %>%
  mutate(City = factor(City, levels = comparison_data_sorted$City))

# Visualize the comparison
ggplot(df_long, aes(x = City, y = Value, fill = Parameter)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "City", y = "Value", title = "Weather Comparison Across Major European Cities") +
  scale_fill_manual(name = "Parameter", values = c("Temperature" = "blue", "Humidity" = "green", "Wind_Speed" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

