# Load required libraries
library(httr)
library(jsonlite)
library(ggplot2)

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
visualize_weather_forecast <- function(forecast_data) {
  temperature <- sapply(forecast_data$list, function(x) x$main$temp)
  date_time <- as.POSIXct(sapply(forecast_data$list, function(x) x$dt_txt), tz = "UTC")
  
  df <- data.frame(DateTime = date_time, Temperature = temperature)
  
  ggplot(df, aes(x = DateTime, y = Temperature)) +
    geom_line() +
    labs(x = "Date Time", y = "Temperature (°C)", title = "Weather Forecast") +
    theme_minimal()
}

# Test the function
city <- "Tokyo"
api_key <- weather_cred$api_key
forecast_data <- get_weather_forecast(city, api_key)

# Display the number of forecast periods
num_periods <- length(forecast_data$list)
cat("Number of forecast periods:", num_periods, "\n")
visualize_weather_forecast(forecast_data)
