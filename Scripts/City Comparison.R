# Load required libraries
library(httr)
library(jsonlite)
library(ggplot2)

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
    Wind_Speed = sapply(weather_data, function(x) x$wind$speed)
  )
  
  return(df)
}

# Test the function
cities <- c("London", "New York", "Tokyo", "Sydney")
api_key <- weather_cred$api_key
comparison_data <- compare_weather_across_cities(cities, api_key)
print(comparison_data)
