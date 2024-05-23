# Load required libraries
library(httr)
library(jsonlite)
library(ggplot2)
library(here)

# Load credentials
weather_cred <- read_csv(file = here("weather_credentials.csv"))
api_key <- weather_cred$api_key

# Function to get weather forecast
get_weather_forecast <- function(city, api_key) {
  base_url <- "http://api.openweathermap.org/data/2.5/forecast"
  response <- GET(base_url, query = list(q = city, appid = api_key, units = "metric"))
  
  if (status_code(response) == 200) {
    forecast_data <- content(response, as = "parsed")
    return(forecast_data)
  } else {
    stop("Error: Unable to fetch forecast data")
  }
}

# Function to visualize weather forecast
visualize_weather_forecast <- function(forecast_data, city) {
  temperature <- sapply(forecast_data$list, function(x) x$main$temp)
  date_time <- as.POSIXct(sapply(forecast_data$list, function(x) x$dt_txt), tz = "UTC")
  
  df <- data.frame(DateTime = date_time, Temperature = temperature)
  
  ggplot(df, aes(x = DateTime, y = Temperature)) +
    geom_line() +
    labs(x = "Date Time", y = "Temperature (°C)", title = paste("Weather Forecast for", city)) +
    theme_minimal()
}

# Function to visualize weather forecast for multiple cities
visualize_weather_forecast_multi <- function(forecast_data_list, cities) {
  # combine all forecast data into one dataframe
  df <- data.frame()
  for (i in seq_along(forecast_data_list)) {
    temperature <- sapply(forecast_data_list[[i]]$list, function(x) x$main$temp)
    date_time <- as.POSIXct(sapply(forecast_data_list[[i]]$list, function(x) x$dt_txt), tz = "UTC")
    df <- rbind( df, data.frame(DateTime = date_time, Temperature = temperature, City = cities[i]))
  }
  
  # Plot the combined forecast data
  ggplot(df, aes(x = DateTime, y = Temperature, color = City)) +
    geom_line() +
    labs(x = "Date Time", y = "Temperature (°C)", title = "Weather Forecast") +
    theme_minimal()
}

# Function to display the number of forecast periods
display_num_periods <- function(forecast_data, city) {
  num_periods <- length(forecast_data$list)
  cat("Number of forecast periods for", city, ":", num_periods, "\n")
}

# Function to calculate summary statistics for forecasted temperatures
calculate_summary_stats <- function(forecast_data_list, cities) {
  summary_stats <- list()
  for (i in seq_along(forecast_data_list)) {
    temperature <- sapply(forecast_data_list[[i]]$list, function(x) x$main$temp)
    summary_stats[[i]] <- summary(temperature)
  }
  names(summary_stats) <- cities
  return(summary_stats)
}

# Test the function for major cities
major_cities <- c("London", "Paris", "Berlin", "Madrid", "Rome", "Vienna", "Bern", "Stockholm")
forecast_data_list <- list()
for (city in major_cities) {
  forecast_data <- get_weather_forecast(city, api_key)
  display_num_periods(forecast_data, city)
  forecast_data_list[[length(forecast_data_list) + 1]] <- forecast_data
}

# Visualize the weather forecast for multiple cities
print(visualize_weather_forecast_multi(forecast_data_list, major_cities))

# Calculate summary statistics for forecasted temperatures
summary_stats <- calculate_summary_stats(forecast_data_list, major_cities)
print(summary_stats)
