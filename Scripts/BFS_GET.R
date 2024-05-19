library(httr)
library(jsonlite)

# URL für die Hauptmetadaten eines Datenwürfels
url <- "https://www.pxweb.bfs.admin.ch/api/v1/de/px-x0102030000_101/px-x-0102030000_101.px"

# Keine Filter mehr nötig
filters <- list(
  response = list(
    format = "json-stat"
  )
)

# Konvertiere den Anfragekörper in JSON
filters_json <- toJSON(filters, auto_unbox = TRUE, pretty = TRUE)

# Konvertiere den Anfragekörper in URL-kodierte Zeichenkette
filters_url_encoded <- URLencode(filters_json)

# An die URL anhängen
full_url <- paste0(url, "?$format=json-stat")

# GET-Anfrage senden
response <- GET(full_url)

# HTTP-Statuscode abrufen
status_code <- status_code(response)

# Antwortinhalt anzeigen
content_text <- content(response, "text", encoding = "UTF-8")
cat(content_text)

# Überprüfen, ob die Anfrage erfolgreich war (Statuscode 200)
if (status_code == 200) {
  # JSON-Daten aus der API-Antwort extrahieren
  data <- fromJSON(content_text, flatten = TRUE)
  print(data)
} else {
  stop(paste("Fehler bei der API-Anfrage. Statuscode:", status_code))
}


rvest::read_html(url)