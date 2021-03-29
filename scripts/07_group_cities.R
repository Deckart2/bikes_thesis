#Group Cities:

#Groups Cities by Region and Size:

library(tidyverse)
library(tidylog)

ct <- read.csv("data_in_progress/ct_with_bike.csv", check.names = T)

#Region Classification
midwest <- c("Chicago", "Indianapolis", "Grand Rapids", "Minneapolis",
             "St. Louis", "Kansas City", "Milwaukee", "Detroit", 
             "Rochester", "Buffalo",  "Cincinnati",
             "Columbus", "Cleveland", "Pittsburgh", "St. Paul")

north_east <- c("Washington", "Baltimore", "Boston", "New York",
                "Philadelphia", "Providence", "Virginia Beach")

south <- c("New Orleans", "Oklahoma City", "Memphis", 
           "Jacksonville", "Miami", "Tampa", "Orlando", 
           "Atlanta", "Nashville", "Louisville", "Charlotte",
           "Raleigh", "Richmond")

interior_west <- c("Phoenix", "Dallas", "Houston", "Austin", "Fort Worth",
                "San Antonio", "Las Vegas", "Denver", "Salt Lake City")

west_coast <- c("San Jose", "Sacramento", "San Diego", "San Francisco",
                "Los Angeles", "Portland", "Seattle")

#Size Classification:
#7million < 
extremely_large <- c("New York", "Los Angeles", "Chicago", "Houston", "Dallas", "Ft. Worth")

#3-7 million
large<-c("Washington", "Miami", "Philadelphia", "Atlanta", "Phoenix", "Boston",
         "San Francisco","Detroit", "Seattle", "Minneapolis", "St. Paul", "San Diego", "Tampa")


ct <- ct %>%
  mutate(city_region = case_when(
    city %in% midwest ~ "midwest",
    city %in% north_east ~ "northeast",
    city %in% south ~ "south",
    city %in% interior_west ~ "interior_west",
    city %in% west_coast ~ "west_coast")) %>%
  mutate(city_size = case_when (
    city %in% extremely_large ~ "extremely_large",
    city %in% large ~ "large", 
    TRUE ~ "medium"
  )) %>%
  mutate(bike_presence = case_when(
    total_lane_rate > 0 ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(pop_density = tot_pop_2019/area)

write.csv(ct, "data_in_progress/ct_with_bike_clean.csv")
