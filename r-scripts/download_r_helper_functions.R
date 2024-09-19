
# packages we will use in the course
pac <- c("httr", "stringr","moments", "sandwich", "lmtest", "car", "plm", "ivreg", "dynlm", "forecast", "urca")

# install and/or load packages
checkpac <- function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x)
  }
  require(x, character.only = TRUE)
}

# # Load the necessary libraries
# library(httr)
# library(stringr)

# check if packages are install yet
suppressWarnings(sapply(pac, checkpac))

# Specify the URL of the R script
url <- "https://ilias.uni-hohenheim.de/data/UHOH/lm_data/lm_1856939/MA_EconometricMethods_WiSe2324_PracticalClass/r-scripts/r_helper_functions.R"

# Send an HTTP GET request to the URL
response <- GET(url)

# Check if the request was successful (status code 200)
# if (http_type(response) == "application/octet-stream") {
if (http_type(response) == "text/plain") {
  # Extract the R code from the response content
  r_code <- content(response, as = "text", encoding = "UTF-8")
  
  # Define the filename for the saved R script
  filename <- "r_helper_functions.R"
  
  # Save the R code to the current working directory
  writeLines(r_code, con = filename)
  
  cat("R script saved as:", filename, " in the current working directory! \n")
} else {
  cat("Failed to retrieve the R code. Check the URL and your internet connection.\n")
}
