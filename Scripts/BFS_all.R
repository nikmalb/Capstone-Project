library(httr)

# Define the URL
url <- "https://www.pxweb.bfs.admin.ch/api/v1/en/px-x0102030000_101/px-x-0102030000_101.px"

# Define the POST body
body <- list(
  query = list(
    list(
      code = "Jahr",
      selection = list(
        filter = "all",
        values = list("*")
      )
    )
  ),
  response = list(
    format = "csv"
  )
)

# Convert the body to JSON
json_body <- toJSON(body)

# Make the POST request with JSON payload
response <- POST(url, body = json_body, encode = "json", verbose())

# Check if the request was successful
if (http_status(response)$status_type == 2) {
  # Check if the response is in CSV format
  if (http_type(response) == "text/csv") {
    # Save the CSV response to a file
    content <- content(response, "text")
    writeLines(content, "population_history.csv")
    cat("CSV file saved successfully.")
  } else {
    cat("Error: Unexpected response format.")
  }
} else {
  cat("Error:", http_status(response)$reason)
}

