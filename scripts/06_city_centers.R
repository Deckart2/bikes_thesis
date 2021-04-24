#Find City Centers:

#Goal: compute distance of tract to CBD:
#Process: 
#Find tracts with highest jobs/area 
#find that tract's centroid
#Compute distance of other centroids to that centroid


library(sf)
library(tidyverse)
library(tidylog)
library(tmap)
library(readxl)
library(janitor)

#Read in Data:
ct_with_bike <- read.csv("data_in_progress/ct_with_bike.csv")
ct_shp_city <- st_read("data_in_progress/ct_shp_city1.geojson")
city_centers <- read_excel("data_raw/holian_city_center_data.xlsx", sheet="Hartley") 

#Find CBD of Top 52 Cities =====
#Create list of all cities I analyze
city_list <- sort(unique(ct_with_bike$city))

#Process City Centers:
#Use the 1982 census locations if they exist and the ArcGis locations otherwise
city_centers <- city_centers %>%
  select("city" = "place", "cbd_retail_lat", "cbd_retail_long", "arcgis_lat", "arcgis_long", pop2k) %>%
  mutate(lat = case_when(
    !is.na(cbd_retail_lat) ~ cbd_retail_lat, 
    is.na(cbd_retail_lat) ~ arcgis_lat)) %>%
  mutate(long = case_when(
    !is.na(cbd_retail_long) ~ cbd_retail_long,
    is.na(cbd_retail_long) ~ arcgis_long
  )) %>%
  mutate(city = replace(city, city == "Saint Louis", "St. Louis")) %>% #respell st. louis
  as.data.frame() %>%
  filter(city %in% city_list)%>%
  arrange(desc(pop2k)) 
  
city_centers <- city_centers[ !duplicated(city_centers$city), ]#Remove duplicate named cities that aren't ones being studied

#Add St. Paul because it wasn't in the data given
#Just googled St. Paul City Hall and put in lat-long from Google Maps
st_paul_row <- data.frame(city="St. Paul",
                          cbd_retail_lat = NA,
                          cbd_retail_long= NA,
                          arcgis_lat = NA,
                          arcgis_long = NA,
                          pop2k = 1,
                          lat = 44.94443574873054,
                          long = -93.09397465986572) 
city_centers <- rbind(city_centers, st_paul_row)
city_centers <- city_centers %>%
  st_as_sf(coords = c("long","lat")) %>%
  select(city) 
st_crs(city_centers) <- 4326
city_centers <- st_transform(city_centers, 2163)
  
#Get data spatial:
ct <- left_join(ct_with_bike, ct_shp_city, by = "gisjoin") %>%
  st_as_sf() %>%
  distinct(.keep_all=TRUE)

#Centroids of spatial data
ct_shp_centroids <- st_centroid(ct) %>%
  select(city, gisjoin)

#Small test:
#ct_shp_centroids_small <- slice_sample(ct_shp_centroids, n = 100) %>%
#  as.data.frame() %>%
#  left_join(city_centers, by = "city") %>%
#  mutate(dist= st_distance(geometry.x, geometry.y, by_element = TRUE))

#Run big:
ct_shp_centroids <- ct_shp_centroids %>%
  as.data.frame() %>%
  left_join(city_centers, by = "city") %>%
  mutate(dist_to_cbd = st_distance(geometry.x, geometry.y, by_element = TRUE)) %>%
  select(gisjoin, dist_to_cbd)

#Add back to main data and convert to km:
ct_with_bike <- left_join(ct_with_bike, ct_shp_centroids) %>%
  select(-c(X, X.1)) %>%
  mutate(dist_to_cbd_km = dist_to_cbd/1000)

#write:
write.csv(ct_with_bike, "data_in_progress/ct_with_bike.csv")




