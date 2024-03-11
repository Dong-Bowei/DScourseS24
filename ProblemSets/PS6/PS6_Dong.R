########################################################################################################
# 1990-2021 Natural Incidents Data from FEMA                                                           #
########################################################################################################

setwd("/Users/boweidong/Dropbox/My Mac (Boweis-MacBook-Air.local)/Desktop")

library(Formula)
library(MASS)
library(lmtest)
library(zoo)
library(tidyverse)
library(ipumsr)
library(ggplot2)
library(stargazer)
library(car)
library(ivreg)
library(dplyr)
library(usmap)
library(ggpubr)
library(panelr)
library(gganimate)
library(gifski)
library(gsheet) #Links to the data in a Google Sheet
library(GGally) #Extension of ggplot mapping attributes
library(rgdal) #Provides geospatial attributes
library(magick) #Processes images
library(choroplethr)
library(choroplethrMaps)
library(ggmap)
library(maps)
library(mapdata)
library(ggthemes)
library(sf)
library(mapview)
library(rnaturalearth)
library(rnaturalearthdata)
library(googleway)
library(ggrepel)
library(ggspatial)
# library(libwgeom)
# library(gganimate)
# library(transformr)
library(synthdid)
library(plm)
library(Synth)
# define the url for the appropriate api end point
DisasterDeclarationsSummaries_2 <- read_csv("~/Desktop/DisasterDeclarationsSummaries-2.csv")

Disaster <- DisasterDeclarationsSummaries_2

Disaster <- Disaster %>% filter(declarationTitle != "COVID-19 PANDEMIC")
unique(Disaster$incidentType)

Disaster$Year <- format(Disaster$declarationDate, "%Y")

Disaster$County <- str_c(Disaster$fipsStateCode, sep = "", Disaster$fipsCountyCode)
Disaster$County <- as.numeric(Disaster$County)
Disaster_1 <- Disaster %>% group_by(County, Year) %>% summarise(County,
                                                                Year,
                                                                incidentType)

Disaster_2 <- Disaster_1 %>% filter(Year != 2020 & incidentType != "Biological")

summary_data <- Disaster_2 %>%
  group_by(County, Year, incidentType) %>%
  count(name = "Incident_Count")

summary_data_1 <- summary_data %>% filter(Year > 1989)

#all_combinations <- expand.grid(
#  County = unique(summary_data_1$County),
#  Year = seq(min(summary_data_1$Year), max(summary_data_1$Year), by = 1)
#)

# complete_data <- summary_data_1 %>%
#   complete(County, Year, incidentType, fill = list(incidentType = 0))


# merged_data <- merge(all_combinations, summary_data_1, by = c("County", "Year"), all.x = TRUE)

# Assuming you have a list of incident types named 'incident_types'
incident_types <- c("Fire", "Flood", "Hurricane", "Severe Storm", "Winter Storm", "Tornado",
                    "Snowstorm", "Earthquake", "Mud/Landslide", "Coastal Storm", "Other",
                    "Severe Ice Storm", "Dam/Levee Break", "Tropical Storm", "Tsunami",
                    "Biological", "Typhoon", "Volcanic Eruption", "Freezing",
                    "Toxic Substances", "Chemical", "Terrorist", "Drought",
                    "Human Cause", "Fishing Losses")

# Create a data frame with all possible County-Year-Incident combinations
complete_data <- expand.grid(
  County = unique(summary_data_1$County),
  Year = seq(1990, 2023, by = 1),
  incidentType = incident_types
)

# Combine with my dataset summary_data_1
summary_data_1$Year <- as.numeric(summary_data_1$Year)
Combined_data <- left_join(complete_data, summary_data_1, by = c("County", "Year", "incidentType"))

# Assuming 'your_data' is the name of your data frame and 'column_name' is the column with NAs
Combined_data$Incident_Count[is.na(Combined_data$Incident_Count)] <- 0

# Get rid of certain incidents 
Combined_data.1 <- Combined_data %>%
  filter(!(incidentType %in% c("Fishing Losses", "Other", "Human Cause", "Toxic Substances", "Chemical", "Biological", "Terrorist")))

# Upload the countyfips names dataset
state_and_county_fips_master <- read_csv("~/Desktop/my data sets/state_and_county_fips_master.csv")
fips <- state_and_county_fips_master %>% rename("County" = "fips", "County_name" = "name", "State_name" = "state")
Combined_fips <- dplyr::left_join(fips, Combined_data.1, by = "County")


# Define the list of incident types
incident_types <- c(
  "Fire", "Flood", "Hurricane", "Severe Storm", "Winter Storm",
  "Tornado", "Snowstorm", "Earthquake", "Mud/Landslide",
  "Coastal Storm", "Severe Ice Storm", "Dam/Levee Break",
  "Tropical Storm", "Tsunami", "Typhoon", "Volcanic Eruption",
  "Freezing", "Drought"
)

# Create a data frame with all combinations of County, Year, and IncidentType
all_combinations <- expand.grid(
  County = unique(fips$County),
  Year = seq(1990, 2023, by = 1),
  incidentType = incident_types
)

# Merge with existing data to fill in existing observations
result_data <- dplyr::left_join(all_combinations, fips, by = "County", all.x = TRUE)

# Combine with the existing data
result_data.1 <- dplyr::left_join(result_data, Combined_data.1, by = c("County", "Year", "incidentType"))

colSums(is.na(result_data.1))
result_data.2 <- result_data.1[complete.cases(result_data.1$State_name), ]

colSums(is.na(result_data.2))
result_data.2$Incident_Count[is.na(result_data.2$Incident_Count)] <- 0

result_data.3 <- result_data.2 %>% select(1, 4, 5, 2, 3, 6)

write.csv(result_data.3, "FEMA_data_long.csv")

# Pivot the data to wide format
wide_data <- result_data.3 %>%
  pivot_wider(
    id_cols = c(County, County_name, State_name, Year),
    names_from = incidentType,
    values_from = Incident_Count,
    values_fill = 0  # Fill missing values with 0
  )

write.csv(wide_data, "FEMA_data_wide.csv")

wide_data.1 <- wide_data %>%
  mutate(across(all_of(incident_types), ~ ifelse(. != 0, 1, 0), .names = "indicator_{.col}"))

# file_path <- "/Users/boweidong/Dropbox/My Mac (Boweis-MacBook-Air.local)/Desktop/Migration and Disaster/county0708/co0708us.dat"

# Check if the dataset is strongly balanced
is_strongly_balanced <- wide_data.1 %>%
  group_by(County, Year) %>%
  summarise(n_obs = n()) %>%
  summarise(unique_obs = n_distinct(n_obs)) %>%
  pull(unique_obs)

# Print the result
print(is_strongly_balanced)

# Create the visualization, using year 2020/tornado as example

# Graph.a
#whole country
county_choropleth(internal_combination_2, 
                  title = "Wildfire 2018: United States",
                  legend = "weight") +
  scale_fill_brewer(palette=7) #+ scale_fill_gradient2(breaks = c(0, 0.5, 1, 1.5, 2, 7), labels = c("0", "0.5", "1", "1.5", "2", "7"))

county_choropleth(internal_combination_2, title = "Wildfire 2018: United States",
                  legend = "Counts")

# Graph.b
fire.2018 <- wide_data %>% filter(Year == 2018) %>% select(1, 2, 3, 5)

internal_combination_1 <- select(fire.2018, 1, 4)
internal_combination_2 <- rename(internal_combination_1, 'region' = 'County', 'value' = 'Fire')
#oklahoma as a state
county_choropleth(internal_combination_2, state_zoom = c("oklahoma"), title = "Wildfire 2018: Oklahoma",
                  legend = "Counts") #+ scale_fill_discrete(breaks = c(0.5, 1, 1.5, 2, 2.5), labels = c("0.5", "1", "1.5", "2", "3"))

# Graph.c
wide_Ok <- wide_data.1 %>% filter(State_name == "OK")

wide_Ok.1 <- wide_Ok %>% group_by(Year) %>% summarise(Year,
                                                      Fire = sum(Fire),
                                                      Flood = sum(Flood),
                                                      Hurricane = sum(Hurricane),
                                                      `Severe Storm` = sum(`Severe Storm`),
                                                      # `Winter Storm` = sum(`Winter Storm`),
                                                      Tornado = sum(Tornado),
                                                      Snowstorm = sum(Snowstorm),
                                                      # Earthquake = sum(Earthquake),
                                                      # `Mud/Landslide` = sum(`Mud/Landslide`),
                                                      # `Coastal Storm` = sum(`Coastal Storm`),
                                                      `Severe Ice Storm` = sum(`Severe Ice Storm`)
                                                      # `Dam/Levee Break` = sum(`Dam/Levee Break`),
                                                      # `Tropical Storm` = sum(`Tropical Storm`),
                                                      # Tsunami = sum(Tsunami),
                                                      # Typhoon = sum(Typhoon),
                                                      # `Volcanic Eruption` = sum(`Volcanic Eruption`),
                                                      # Freezing = sum(Freezing),
                                                      # Drought = sum(Drought)
                                                      )
wide_Ok.2 <- unique(wide_Ok.1)




disasters_long <- pivot_longer(wide_Ok.2, cols = -Year, names_to = "Disaster", values_to = "Count")

# Create the dot plot with color differentiation and vertical year labels
ggplot(disasters_long, aes(x = Year)) +
  geom_point(aes(y = Count, color = Disaster), size = 3) +
  labs(
    title = "Oklahoma Natural Disasters by Year",
    x = "Year",
    y = "Incident Count",
    color = "Disaster Type"
  ) +
  scale_color_manual(values = rainbow(18)) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(disasters_long$Year), max(disasters_long$Year), by = 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


