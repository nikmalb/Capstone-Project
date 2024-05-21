#Prelimitarities
library("httr")
library("jsonlite")
library("readr")
library("here")

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