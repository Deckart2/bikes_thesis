#City bounds from shapefiles:


library(sf)
library(tidyverse)
library(tidylog)
library(tmap)


census_place_files <- list.files('data_raw/city_bounds' , 
                                 recursive = TRUE, 
                                 pattern = "*.shp$|*.geojson$")



#Helper Function:
#All it does is st_read the file given, but it addresses the issue of different
#working directory
read_simp_city_bound <- function(file) {
  new_file <- paste("data_raw/city_bounds/", file, sep="")
  st_read(new_file) %>%
    st_transform(2163) %>%
    select() %>%
    st_union() %>%
    st_as_sf()  %>%
    mutate(city = file) %>%
    st_buffer(dist = 125)
}

#Call function which runs read_simp_city_bound
census_place_list <- lapply(census_place_files, read_simp_city_bound)

#Combines the shapefiles:
city_bounds <- do.call(rbind, census_place_list) 



#Converts city column into actual city name (on folder)
city_bounds$city <- gsub("/.*", "" ,city_bounds$city)

st_write(city_bounds, "data_in_progress/city_bounds.geojson", append = FALSE)


#Test With Chicago/DC/Portland CTs:
ct_shp <- st_read("data_raw/census_ipums_ct_shp3/US_tract_2010.shp") %>%
  st_transform(2163)




ct_il <- ct_shp %>%
  filter(STATEFP10 == "17")

sel_ct_chi <- st_within(ct_il, city_bounds)
sel_ct_chi <- lengths(sel_ct_chi) > 0
ct_chicago <- ct_il[sel_ct_chi, ]
  
tm_shape(ct_chicago) + tm_polygons()

ct_dc <- ct_shp %>%
  filter(STATEFP10 == "11")
sel_ct_dc <- st_within(ct_dc, city_bounds)
sel_ct_dc <- lengths(sel_ct_dc) > 0
ct_dc <- ct_dc[sel_ct_dc, ]
tm_shape(ct_dc) + tm_polygons()

ct_oregon <- ct_shp %>%
  filter(STATEFP10=="41")
sel_ct_port <- st_within(ct_oregon, city_bounds)
sel_ct_port <- lengths(sel_ct_port) > 0
ct_port <- ct_oregon[sel_ct_port, ]

tm_shape(ct_port) + tm_polygons()


