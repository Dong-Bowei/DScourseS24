library(rvest)
library(janitor)
library(lubridate)
library(jsonlite)
library(listviewer)
library(dplyr)

# Q3
wiki <- read_html("https://en.wikipedia.org/wiki/List_of_natural_disasters_in_the_United_States") 

# Select the correct table using a different CSS selector
nature <- wiki %>%
  html_element("#mw-content-text > div.mw-content-ltr.mw-parser-output table.wikitable") %>%
  html_table()

# Print the resulting data frame
nature

write.csv(nature, "nature.csv", row.names = FALSE)

# CSV format obtained

# Q4
endpoint = "https://www.fema.gov/api/open/v2/DisasterDeclarationsSummaries?$filter=disasterNumber%20eq%201491"
fema = fromJSON(endpoint)
str(fema)

str(fema$DisasterDeclarationsSummaries)

head(fema$DisasterDeclarationsSummaries)

# Extract the data frame
fema_df <- fema$DisasterDeclarationsSummaries

# Write to CSV
write.csv(fema_df, "fema_data.csv", row.names = FALSE)

# CSV format obtained