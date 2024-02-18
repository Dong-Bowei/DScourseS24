library(jsonlite)
library(tidyverse)

url <- "https://www.vizgr.org/historical-events/search.php?format=json&begin_date=00000101&end_date=20240209&lang=en"
destination_file <- "downloaded_file.json"

download.file(url, destfile = destination_file, method = "auto")

#getwd()
#setwd("/Users/boweidong/Dropbox/My Mac (Boweis-MacBook-Air.local)/Desktop/DScourseS24")

mylist <- fromJSON("dates.json")
mydf <- bind_rows(mylist$result[-1])
class(mydf$date)

# Assuming n is the number of rows you want to display
n <- 5  # Change this to the desired number of rows

# Display the first n rows of the dataframe
head(mydf, n)







