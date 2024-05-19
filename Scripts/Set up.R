library(tidyverse)
library(httr)
library(rvest)
library(jsonlite)
library(readr)
library(here) 
library(dplyr)
library(ggmap)

# Load credentials
google_cred <- readr::read_csv(file = here::here("credentials.csv"))

# API key
api_key <- google_cred$api_key

# Function to get points of interest in a specific area
get_places <- function(lat, lon, radius = 5000, type = "tourist_attraction", key) {
  base_url <- "https://maps.googleapis.com/maps/api/place/nearbysearch/json"
  url <- paste0(base_url, "?location=", lat, ",", lon, "&radius=", radius, "&type=", type, "&key=", key)
  response <- GET(url)
  if (http_type(response) != "application/json") {
    stop("API response was not JSON")
  }
  content <- content(response, as = "text", encoding = "UTF-8")
  places_data <- fromJSON(content, flatten = TRUE)
  
  # Debugging-Ausgabe
  print(paste("URL:", url))
  print(paste("HTTP Status:", status_code(response)))
  print(paste("Response content:", content))
  
  return(places_data$results)
}

# Create coordinates centre of Lucerne
lat <- 47.0502
lon <- 8.3093

# API requests for Lucerne
places <- get_places(lat, lon, key = api_key)

# Überprüfen, ob die API-Ergebnisse zurückgegeben wurden
if (length(places) == 0) {
  stop("No places found by the API.")
}

# Create data frame
if (length(places) == 0) {
  stop("No places found by the API.")
}

places_df <- do.call(rbind, lapply(places, function(x) {
  str(x)
  class(x)
  print(x$geometry)
  if (!is.null(x$geometry) && !is.null(x$geometry$location)) {
    data.frame(
      name = x$name,
      lat = x$geometry$location$lat,
      lon = x$geometry$location$lng,
      stringsAsFactors = FALSE
    )
  }
})) 

print(places[1:5])
# Check whether places_df is NULL before applying distinct
if (!is.null(places_df)) {
  places_df <- places_df %>% distinct()
}

# Remove NAs and duplicate entries
places_df <- na.omit(places_df)

# Register API key for ggmap
register_google(key = api_key)

# Retrieve map of Switzerland
lucerne_map <- get_map(location = c(46.8, 8.3), zoom = 8)

# Show points of interest on the map
ggmap(lucerne_map) +
  geom_point(data = places_df, aes(x = lon, y = lat), color = "blue", size = 2, alpha = 0.5) +
  ggtitle("Sightseeing in Lucerne")



# Überprüfe die API-Antwort
print(places)  # Drucke den Inhalt von places
http_status <- 200  # Hier sollte der tatsächliche HTTP-Statuscode eingesetzt werden
if (http_status != 200) {
  stop("API request failed with status code ", http_status)
}

# Überprüfe die Datenstruktur der API-Antwort
str(places)  # Überprüfe die Struktur von places
class(places)  # Überprüfe den Typ von places

# Behandlung von Fehlern
if (!is.list(places)) {
  stop("API response is not in the expected format")
}

# Überprüfe, ob places ein Dataframe ist
if (!is.data.frame(places)) {
  stop("Die API-Antwort ist nicht im erwarteten Format (Dataframe)")
}


