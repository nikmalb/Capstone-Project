#Prelimitarities
library("httr")
library("jsonlite")
library("readr")
library("here")

rm(list = ls())

# Load credentials
weather_cred <- read_csv(file = here("weather_credentials.csv"))
api_key <- weather_cred$api_key

# Function to get current weather
get_current_weather <- function(city, api_key) {
  base_url <- "http://api.openweathermap.org/data/2.5/weather"
  response <- GET(base_url, query = list(q = city, appid = api_key, units = "metric"))
  
  # Print the status code and response content for debugging
  print(paste("Status Code:", status_code(response)))
  print(content(response, as = "text"))
  
  if (status_code(response) == 200) {
    weather_data <- content(response, as = "text")
    weather_json <- fromJSON(weather_data)
    return(weather_json)
  } else if (status_code(response) == 429) {
    stop("Error: Rate limit exceeded. Please wait and try again later.")
  } else {
    stop("Error: Unable to fetch data. Check the API key and city name.")
  }
}

# Test the function
city <- "Zurich"
current_weather <- get_current_weather(city, api_key)
print(current_weather)


# Function to print weather data
print_weather <- function(weather) {
  cat("Weather in", weather$name, ":\n")
  cat("Temperature:", weather$main$temp, "°C\n")
  cat("Feels like:", weather$main$feels_like, "°C\n")
  cat("Min Temperature:", weather$main$temp_min, "°C\n")
  cat("Max Temperature:", weather$main$temp_max, "°C\n")
  cat("Pressure:", weather$main$pressure, "hPa\n")
  cat("Humidity:", weather$main$humidity, "%\n")
  cat("Visibility:", weather$visibility, "m\n")
  cat("Wind Speed:", weather$wind$speed, "m/s\n")
  cat("Wind Direction:", weather$wind$deg, "°\n")
  cat("Cloudiness:", weather$clouds$all, "%\n")
  cat("Weather Description:", weather$weather$description[1], "\n")
}

# Test the function
print_weather(current_weather)

# Test various cities and store weather data in a list
cities <- c("Zurich", "London", "New York", "Tokyo", "Sydney")
weather_list <- list()
for (city in cities) {
  cat("\n---\n")
  current_weather <- get_current_weather(city, api_key)
  weather_list[[city]] <- current_weather
}

for (city in names(weather_list)) {
  cat("\n---\n")
  print_weather(weather_list[[city]])
}

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

# Function to print weather forecast info
print_forecast_info <- function(forecast_json) {
  cat("Weather forecast for the next 5 days:\n")
  str(forecast_json)  # Debugging line to inspect forecast_json structure
  for (i in seq_along(forecast_json$list)) {
    forecast <- forecast_json$list[[i]]
    cat("Date:", forecast$dt_txt, "\n")
    cat("Temperature:", forecast$main$temp, "°C\n")
    cat("Feels like:", forecast$main$feels_like, "°C\n")
    cat("Description:", forecast$weather[[1]]$description, "\n")
    cat("-----------------------------------\n")
  }
}



# Test the forecast function
city <- "Sydney"
api_key <- weather_cred$api_key

weather_forecast <- get_weather_forecast(city, api_key)

# Function to print readable forecast information
print_forecast_summary <- function(forecast_data) {
  forecast_list <- forecast_data$list
  
  # Create an empty data frame to store the summary
  forecast_summary <- data.frame(
    DateTime = character(),
    Temperature = numeric(),
    FeelsLike = numeric(),
    WeatherDescription = character(),
    WindSpeed = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (forecast in forecast_list) {
    # Extract relevant details
    dt_txt <- forecast$dt_txt
    temp <- forecast$main$temp
    feels_like <- forecast$main$feels_like
    weather_description <- forecast$weather[[1]]$description
    wind_speed <- forecast$wind$speed
    
    # Add the details to the summary data frame
    forecast_summary <- rbind(forecast_summary, data.frame(
      DateTime = dt_txt,
      Temperature = temp,
      FeelsLike = feels_like,
      WeatherDescription = weather_description,
      WindSpeed = wind_speed,
      stringsAsFactors = FALSE
    ))
  }
  
  # Print the summary
  print(forecast_summary)
}

# Use the function to print the weather forecast summary
print_forecast_summary(weather_forecast)



# Extract the list of forecasts
forecasts <- weather_list$list

# Create a function to extract the desired information from each forecast entry
extract_forecast <- function(forecast) {
  data.frame(
    dt = forecast$dt,
    temp = forecast$main$temp,
    feels_like = forecast$main$feels_like,
    temp_min = forecast$main$temp_min,
    temp_max = forecast$main$temp_max,
    pressure = forecast$main$pressure,
    humidity = forecast$main$humidity,
    weather_main = forecast$weather[[1]]$main,
    weather_description = forecast$weather[[1]]$description,
    clouds = forecast$clouds$all,
    wind_speed = forecast$wind$speed,
    wind_deg = forecast$wind$deg,
    wind_gust = forecast$wind$gust,
    visibility = forecast$visibility,
    pop = forecast$pop,
    dt_txt = forecast$dt_txt
  )
}

# Apply the function to each forecast and combine the results into a data frame
forecast_df <- do.call(rbind, lapply(forecasts, extract_forecast))

# Convert the dt column to a readable date format
forecast_df$dt <- as.POSIXct(forecast_df$dt, origin="1970-01-01", tz="UTC")

# Display the data frame
print(forecast_df)
