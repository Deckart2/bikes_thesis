#Maps for Method Visualization

library(tmap)
library(tidyverse)
library(tidylog)
library(sf)
library(RColorBrewer)

city_bounds <- st_read("data_in_progress/city_bounds.geojson")
city_bikes <- st_read("data_in_progress/city_bikes.geojson")
tracts <- st_read("data_raw/census_ipums_ct_shp3/US_tract_2010.shp")
ct_data <- read.csv("data_in_progress/ct_with_bike_clean.csv")

chi_data <- ct_data %>%
  mutate(geoid10 = as.character(geoid10)) %>%
  filter(city=="Chicago")

chi_bikes <- city_bikes %>%
  filter(city == "chicago")

chi_tracts <- chi_data %>%
  left_join(tracts, by = c("gisjoin" = "GISJOIN")) %>%
  st_as_sf() %>%
  st_transform(2163)

legend_title <- "Bike Infrastructure (m/km^2)"

#Color Palette:
pal <- c("#D2DCFF", "#FAD162", "#78AD93")


#Plot 1: Bike lanes with background of city:
chi_bikes_buf <- st_buffer(chi_bikes, 100)

plot1<- tm_shape(chi_tracts) + tm_fill() + tm_borders(alpha=.25) + 
  tm_shape(chi_bikes_buf) + tm_fill(col= "MAP_COLORS") + tm_borders(alpha = 0) + 
  tm_compass(position = c("left", "bottom")) +
  tm_scale_bar(position = c("left", "bottom")) + 
  tm_layout(title ="Bike Lanes Unsegmented", title.size = 2, frame = FALSE)


chi_bikes_split <- st_intersection(chi_bikes, chi_tracts)
chi_bikes_split_buf <- st_buffer(chi_bikes_split, 100)

plot2<- tm_shape(chi_tracts) + tm_fill() + tm_borders(alpha=.25) + 
  tm_shape(chi_bikes_split_buf) + tm_fill(col= "MAP_COLORS") + tm_borders(alpha = 0) + 
  tm_compass(position = c("left", "bottom")) +
  tm_scale_bar(position = c("left", "bottom")) + 
  tm_layout(title ="Bike Lanes Segmented", title.size = 2.4, frame = FALSE)

#Plot 3: Combined:
plot3 <- tm_shape(chi_tracts) + 
  tm_fill(col="total_lane_rate", style="jenks", title=legend_title, palette = "BuGn") + 
  tm_borders(alpha = .3) + 
  tm_shape(chi_bikes) + tm_lines(alpha=.8) + 
  tm_compass(position = c("left", "bottom")) +
  tm_scale_bar(position = c("left", "bottom")) + 
  tm_layout(title ="Infrastructure per Tract", title.size = 2, 
            legend.position = c("right", "bottom"), 
            frame = FALSE) 
combo <- tmap_arrange(plot1, plot2, plot3, ncol=3, asp=.5)
#Export to PDF a 8.5 by 11 and edit out the legend label to make a bigger one



#Change dimensions to 1300 and keep aspect ratio



#Gentrification Chart =======
dc_data <-ct_data %>%
  filter(city=="Washington")


dc_tracts <- dc_data %>%
  left_join(tracts, by = c("gisjoin" = "GISJOIN")) %>%
  st_as_sf() %>%
  st_transform(2163)


dc <- tm_shape(dc_tracts) + tm_polygons(col="gent_status", palette = pal) + 
  tm_compass(size = 1.5) + tm_scale_bar() + 
  tm_layout(title= "Washington DC",
            scale = 1.2,
            title.position = c("right", "top"),  asp = 2,
            frame = FALSE)

nash_data <- ct_data %>%
  filter(city=="Nashville")
nash_tracts <- nash_data %>%
  left_join(tracts, by = c("gisjoin" = "GISJOIN")) %>%
  st_as_sf() %>%
  st_transform(2163)

nashville <- tm_shape(nash_tracts) + 
  tm_polygons(col="gent_status", palette=pal) +
  tm_compass(position = c("center", "bottom")) + tm_scale_bar() + 
  tm_layout(title= "Nashville",
            title.size = 1.8, 
            title.position = c("left", "top"),
            scale = .9,
            legend.position = c("left", "bottom"), asp = 2,
            frame = FALSE)


tmap_arrange(dc, nashville, ncol=2, asp=.6)

#Export as pdf 6.37 x 8.25, convert to jpeg then crop


#Bike Data for Nashville and DC ======
nash_bike <- tm_shape(nash_tracts) + tm_polygons(col="total_lane_rate", bins="jenks", palette = "BuGn", n=5) +
  tm_compass(position = c("center", "bottom")) + tm_scale_bar() + 
  tm_layout(title= "Nashville",
            title.size = 2.5, 
            title.position = c("left", "top"),
            scale = .7,
            legend.position = c("left", "bottom"), asp = 2,
            frame = FALSE)

dc_bike <- tm_shape(dc_tracts) + tm_polygons(col="total_lane_rate", bins="jenks", palette = "BuGn", n=5) + 
  tm_compass(size = 1.5) + tm_scale_bar() + 
  tm_layout(title= "Washington DC",
            scale = 1.2,
            title.position = c("right", "top"),  asp = 2,
            frame = FALSE)

tmap_arrange(dc_bike, nash_bike, ncol=2, asp=.6)
#Export as pdf 6.37 x 8.25, convert to jpeg then crop


#Overall distance/area chart chart========
gent_type <- ct_data %>%
  group_by(gent_status) %>%
  summarize(total_lane_rate = mean(total_lane_rate, na.rm=T), 
            separated_path_rate = mean(separated_path_rate, na.rm=T),
            protected_lane_rate = mean(protected_lane_rate, na.rm=T),
            painted_lane_rate = mean(painted_lane_rate, na.rm=T),
            bvld_rate = mean(bvld_rate, na.rm=T), 
            shared_lane_rate = mean(shared_lane_rate, na.rm=T),
            unknown_rate = mean(unknown_rate, na.rm=T)) %>%
  select(-unknown_rate) %>%
  filter(gent_status != "NA") %>%
  mutate(across(total_lane_rate:shared_lane_rate, round, 1)) %>%
  pivot_longer(names_to = "lane_type", cols = total_lane_rate:shared_lane_rate) %>%
  mutate(name_to_use = case_when(
    lane_type == "total_lane_rate" ~ "Total",
    lane_type == "separated_path_rate" ~ "Separated Trail/Path",
    lane_type == "protected_lane_rate" ~ "Protected Lane",
    lane_type == "painted_lane_rate" ~ "Regular",
    lane_type == "shared_lane_rate" ~ "Shared Lane",
    lane_type == "bvld_rate" ~ "Bike Boulevard"
  )) %>%
  mutate(gent_status = case_when(
    gent_status == "advantaged" ~ "Advantaged",
    gent_status == "gentrifying" ~ "Gentrifying",
    gent_status == "not_gentrifying"~ "Not Gentrifying"
    ))

ggplot(gent_type, aes(fill = gent_status, y=value, x = name_to_use)) +
  geom_bar(position="dodge", stat="identity") + 
  labs(x = "Lane Type", 
       y = "Bike Infrastructure m/km^2", 
       title = "Average Cycling Infrastructure by Type") +
  scale_fill_discrete(name="Gentrification\nStatus") + 
  theme_classic() + 
  scale_fill_manual(values=pal)

#Good output: 6.375, 8.25
  

#Infrastructure by region =====
gent_region <- ct_data %>%
  group_by(gent_status, city_region) %>% 
  summarize(mean_total_lane_rate = mean(total_lane_rate, na.rm=T)) %>%
  filter(gent_status != "NA") %>%
  mutate(name_to_use = case_when(
    city_region == "interior_west" ~ "Interior West",
    city_region == "midwest" ~ "Midwest",
    city_region == "northeast" ~ "Northeast",
    city_region == "south" ~ "South",
    city_region == "west_coast" ~ "West Coast")) %>%
  mutate(gent_status = case_when(
    gent_status == "advantaged" ~ "Advantaged",
    gent_status == "gentrifying" ~ "Gentrifying",
    gent_status == "not_gentrifying"~ "Not Gentrifying"
  ))

ggplot(gent_region, aes(fill = gent_status, y=mean_total_lane_rate, x = name_to_use)) +
  geom_bar(position="dodge", stat="identity") + 
  labs(x = "City Region", 
       y = "Bike Infrastructure m/km^2", 
       title = "Average Cycling Infrastructure by Region") +
  scale_fill_discrete(name="Gentrification\nStatus") + 
  theme_classic() + 
  scale_fill_manual(values=pal)
#Good output: 6.375, 8.25



#Infrastructure by city_size====
gent_size <- ct_data %>%
  group_by(gent_status, city_size) %>% 
  summarize(mean_total_lane_rate = mean(total_lane_rate, na.rm=T)) %>%
  pivot_wider(names_from = gent_status, values_from = mean_total_lane_rate) %>%
  select(-c("NA")) %>%
  mutate(across(advantaged:not_gentrifying, round, 1)) %>%
  pivot_longer(cols= advantaged:not_gentrifying, names_to = "gent_status") %>%
  mutate(name_to_use = case_when(
    city_size == "extremely_large" ~ "Extremely Large",
    city_size == "large" ~ "Large",
    city_size == "medium" ~ "Medium")) %>%
  mutate(gent_status = case_when(
    gent_status == "advantaged" ~ "Advantaged",
    gent_status == "gentrifying" ~ "Gentrifying",
    gent_status == "not_gentrifying"~ "Not Gentrifying"
  ))

ggplot(gent_size, aes(fill = gent_status, y=value, x = name_to_use)) +
  geom_bar(position="dodge", stat="identity") + 
  labs(x = "City Size", 
       y = "Bike Infrastructure m/km^2", 
       title = "Average Cycling Infrastructure by City Size") +
  scale_fill_discrete(name="Gentrification\nStatus") + 
  theme_classic() +
  scale_fill_manual(values=pal)

#Good output: 6.375, 8.25


#Gent Distance====
mean_dist_by_gent <- ct_data %>%
  group_by(gent_status) %>%
  summarize(dist = mean(dist_to_cbd_km)) %>%
  filter(gent_status != "NA") %>%
  mutate(gent_status = case_when(
    gent_status == "advantaged" ~ "Advantaged",
    gent_status == "gentrifying" ~ "Gentrifying",
    gent_status == "not_gentrifying"~ "Not Gentrifying"
  ))

ggplot(mean_dist_by_gent, aes(y=dist, x = gent_status)) +
  geom_bar(position="dodge", stat="identity") + 
  labs(x = "Gentrification Status", 
       y = "Mean Distance to CBD (km)", 
       title = "Average Distance to CBD by Gentrification Status") +
  theme_classic() 

