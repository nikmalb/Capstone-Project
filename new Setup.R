library(httr)

# API-URL für die Lebenserwartung
url <- "https://www.pxweb.bfs.admin.ch/api/v1/en/px-x-0102030000_101/px-x-0102030000_101.px"

# Filter für Geschlecht, Alter und Jahr
filters <- list(
  query = list(
    list(
      code = "Geschlecht",
      selection = list(
        filter = "item",
        values = c("0") # Total (Geschlecht)
      )
    ),
    list(
      code = "Alter",
      selection = list(
        filter = "item",
        values = c("0") # Total (Alter)
      )
    ),
    list(
      code = "Jahr",
      selection = list(
        filter = "item",
        values = c("2020") # Jahr 2020
      )
    )
  ),
  response = list(
    format = "json-stat"
  )
)

# Vor dem Senden der Anfrage
print(filters)

# POST-Anfrage senden
response <- POST(url, body = filters, encode = "json")
print(content(response))

# HTTP-Statuscode der Antwort abrufen
status_code <- http_status(response)$status_code

# Status der Anfrage überprüfen
if (status_code == 200) {
  # Daten erfolgreich abgerufen
  data <- content(response, as = "text")
  cat(data)
} else {
  # Fehlermeldung ausgeben
  stop(paste("Fehler beim Abrufen der Daten. HTTP-Statuscode:", status_code))
}

library(httr)

# API-URL für die Lebenserwartung
url <- "https://www.pxweb.bfs.admin.ch/api/v1/en/px-x-0102030000_101/px-x-0102030000_101.px"

# Filter für Geschlecht, Alter und Jahr
filters <- list(
  query = list(
    list(
      code = "Geschlecht",
      selection = list(
        filter = "item",
        values = c("0") # Total (Geschlecht)
      )
    ),
    list(
      code = "Alter",
      selection = list(
        filter = "item",
        values = c("0") # Total (Alter)
      )
    ),
    list(
      code = "Jahr",
      selection = list(
        filter = "item",
        values = c("2020") # Jahr 2020
      )
    )
  ),
  response = list(
    format = "json-stat"
  )
)