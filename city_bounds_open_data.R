#City bounds from shapefiles:


library(sf)
library(tidyverse)
library(tidylog)
library(tmap)


census_place_files <- list.files('city_bounds' , 
                                 recursive = TRUE, 
                                 pattern = "*.shp$")


#Helper Function:
#All it does is st_read the file given, but it addresses the issue of different
#working directory
read_simp_city_bound <- function(file) {
  new_file <- paste("city_bounds/", file, sep="")
  st_read(new_file) %>%
    st_transform(2163) %>%
    select() %>%
    st_union() %>%
    st_as_sf()  %>%
    mutate(city = file) 
}

#Call function which runs read_simp_city_bound
census_place_list <- lapply(census_place_files, read_simp_city_bound)

#Combines the shapefiles:
city_bounds <- do.call(rbind, census_place_list) 

#Converts city column into actual city name (on folder)
city_bounds$city <- gsub("/.*", "" ,city_bounds$city)



#Test With Illinois CTs:
c_tract_test_IL <- st_read("c_tract_test_IL.geojson")

c_tract_chicago <- c_tract_test_IL[city_bounds, ]
  
tm_shape(c_tract_test_IL) + tm_polygons() + 
  tm_shape(c_tract_chicago) + tm_fill(col = "red") + tm_borders(alpha = .15)
#The visualization shows that this works; need to refine method to pick out
#polygons with only "substantial" overlap







