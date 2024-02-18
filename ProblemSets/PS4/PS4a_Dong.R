library(jsonlite)
library(tidyverse)

getwd()
setwd("/Users/boweidong/Dropbox/My Mac (Boweis-MacBook-Air.local)/Desktop/DScourseS24")

mylist <- fromJSON("dates.json")
mydf <- bind_rows(mylist$result[-1])
class(mydf$date)

# Assuming n is the number of rows you want to display
n <- 5  # Change this to the desired number of rows

# Display the first n rows of the dataframe
head(mydf, n)







