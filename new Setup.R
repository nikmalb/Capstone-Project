library(httr)
library(jsonlite)

# API-URL für die Lebenserwartung
url <- "https://www.pxweb.bfs.admin.ch/api/v1/en/px-x-0102030000_101/px-x-0102030000_101.px"

# Filter für Geschlecht, Alter und Jahr
filters <- list(
  query = list(
    list(
      code = "Sex",
      selection = list(
        filter = "item",
        values = c("0") # Total Sex
      )
    ),
    list(
      code = "Age",
      selection = list(
        filter = "item",
        values = c("0") # Total Age
      )
    ),
    list(
      code = "Year",
      selection = list(
        filter = "item",
        values = c("2020") # Year 2020
      )
    )
  ),
  response = list(
    format = "json-stat"
  )
)


# Konvertiere den Anfragekörper in JSON
filters_json <- toJSON(filters, auto_unbox = TRUE, pretty = TRUE)
cat(filters_json)

# Konvertiere den Anfragekörper in URL kodierte Zeichenkette
filgers_url_encoded <- URLencode(filters_json)

# POST-Anfrage senden
response <- GET(url, query= list(query = filgers_url_encoded))

# Debugging
print(response)
print(content(response, as = "text", encoding = "UTF-8"))
print(headers(response))
print(status_code(response))

# HTTP-Statuscode der Antwort abrufen
status_code <- status_code(response)

#Antwortinhalt anzeigen
content_text <- content(response, as = "text", encoding = "UTF-8")
cat(content_text)

# Status der Anfrage überprüfen
if (status_code == 200) {
  # Daten erfolgreich abgerufen
  data <- fromJSON(content_text, flatten = TRUE)
  print(data)
} else {
  # Fehlermeldung ausgeben
  stop(paste("Fehler beim Abrufen der Daten. HTTP-Statuscode:", status_code))
}

