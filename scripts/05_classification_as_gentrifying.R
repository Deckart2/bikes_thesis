#Adding gentrification label:


library(tidyverse)
library(tidylog)
library(tmap)
library(sf)

ct_data <- read.csv("data_in_progress/ct_with_bike_after_04.csv")



cities <- read.csv("data_in_progress/cities.csv") %>%
  select(-X) %>%
  rename_with(~str_c(., "_city"), change_ed:pct_housing_last_20_10) 
  



#Note: inner join here used to filter out data selected from cities whose bike data I lack
  #access to
ct_data <- inner_join(ct_data, cities, by = "city") 

#Create function to build a new column that classifies a CT as gentrifying:
#Inputs are a df and then the name of columns for: 
# tract's median household income at the start of the time period,
# msa's median household income at the start of the decade, 
# the tract's percent of housing built in last 20 years before start of time period
# the msa's percent of housing built in last 20 years before start of time period
# The change in percent of people 25+ with bachelors in the tract of the time period
# The change in percent of people 25+ with bachelors in the msa over the time period
# The median housing value at the end of the time period
# The inflation adjusted median housing value at the start of the time period 

ct_data <- ct_data %>%
  mutate(gent_status = case_when(
    med_household_inc_2010_inf_adj >= med_household_inc_10_city_infl_adjusted |
    pct_housing_built_1990_09 >= pct_housing_last_20_10_city ~ "advantaged",
    
    med_household_inc_2010 < med_household_inc_10_city &
      pct_housing_built_1990_09 < pct_housing_last_20_10_city &
      change_ed > change_ed_city &
      med_home_val_2019 > med_home_val_2010 ~ "gentrifying",
    
    med_household_inc_2010 < med_household_inc_10_city &
      pct_housing_built_1990_09 < pct_housing_last_20_10_city &
      change_ed <= change_ed_city |
      med_home_val_2019 <= med_home_val_2010 ~ "not_gentrifying"
      ))
ct_data <- ct_data %>%
  mutate(is_gentrifying = case_when(
    gent_status == "gentrifying" ~ 1, 
    TRUE ~ 0
  ))


#Write:
write.csv(ct_data, "data_in_progress/ct_with_bike.csv")



#Vizualizations====
ct_shp_city_gent <- st_read("data_in_progress/ct_shp_city1.geojson")
ct_data <- left_join(ct_data, ct_shp_city_gent, by = c("gisjoin" = "GISJOIN"))






test_viz <- function(city_name){
  df <- ct_data %>%
    filter(city == city_name) %>%
    st_as_sf()
  demo <-  tm_shape(df) + tm_polygons(col = "gent_status")
  demo
}

test_viz_bike <- function(city_name){
  df <- ct_data %>%
    filter(city == city_name) %>%
    st_as_sf()
  demo <-  tm_shape(df) + tm_polygons("total_lane_dist", style = "jenks")
  demo
}

#test_visualization:
chicago_gent <- ct_data %>%
  filter(city == "Chicago") %>%
  left_join(ct_shp_city, by = c("gisjoin" = "GISJOIN")) %>%
  st_as_sf()

phoenix_gent <- ct_data %>%
  filter(city == "Phoenix") %>%
  left_join(ct_shp_city, by = c("gisjoin" = "GISJOIN")) %>%
  st_as_sf()

dc_gent <- ct_data %>%
  

chicago_demo <- tm_shape(chicago_gent) + tm_polygons(col = "gent_status")
phoenix_demo <- tm_shape(phoenix_gent) + tm_polygons(col = "gent_status")


