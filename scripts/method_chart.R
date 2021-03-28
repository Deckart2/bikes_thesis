#Maps for Method Visualization

library(tmap)
library(tidyverse)
library(tidylog)
library(sf)

city_bounds <- st_read("data_in_progress/city_bounds.geojson")
city_bikes <- st_read("data_in_progress/city_bikes.geojson")
tracts <- st_read("data_in_progress/ct_shp_city1.geojson")
ct_data <- read.csv("data_in_progress/ct_with_bike.csv")

chi_data <- ct_data %>%
  filter(city=="Chicago")

chi_bounds <- city_bounds %>%
  filter(city == "Chicago")
chi_bikes <- city_bikes %>%
  filter(city == "chicago")
chi_tracts <- tracts %>%
  filter(STATEFP10 == "17") %>%
  filter(COUNTYFP10 == "031") %>%
  right_join(chi_data, by = c("GISJOIN" = "gisjoin"))
chi_tracts$area <- st_area(chi_tracts)
chi_tracts$lane_dist_per_area <- chi_tracts$total_lane_dist / chi_tracts$area

legend_title <- "Bike Infrastructure (m/m^2)"

#Plot 1: Bike lanes with background of city:
plot1<- tm_shape(chi_tracts) + tm_fill() + tm_borders(alpha=.25) + 
  tm_shape(chi_bikes) + tm_lines() + tm_compass(position = c("right", "bottom")) +
  tm_scale_bar() + tm_layout(title ="Bike Lanes:", title.size = 2)

#Plot 2: Combined:
plot2<- tm_shape(chi_tracts) + 
  tm_fill(col="lane_dist_per_area", style="jenks", title=legend_title) + 
  tm_borders(alpha = .3) + 
  tm_shape(chi_bikes) + tm_lines(alpha=.8) + tm_compass(position = c("right", "bottom")) +
  tm_scale_bar() + 
  tm_layout(title ="Infrastructure Measure:", title.size = 2)

tmap_arrange(plot1, plot2)

#Change dimensions to 1300 and keep aspect ratio

