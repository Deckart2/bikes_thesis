#IPUMS Census Data Manipulation:


#PART 1: Packages and data importation =======
library(tidyverse)
library(tidylog)
library(sf)
library(janitor)
library(rmapshaper)
library(tmap)
library(data.table)
library(units)
library(lwgeom)

#Read data from both years:
ct_2010 <- read.csv("data_raw/census_ipums_ct3/nhgis0015_ds176_20105_2010_tract.csv")
ct_2019 <- read.csv("data_raw/census_ipums_ct3/nhgis0015_ds244_20195_2019_tract.csv")
ct_shp <- st_read("data_raw/census_ipums_ct_shp3/US_tract_2010.shp") %>%
  st_transform(2163)


#PART 2: CLEAN CENSUS DATA =====

ct_2010_main <- ct_2010 %>%
  select(GISJOIN, YEAR, STATE, COUNTY, TRACTA, 
         tot_pop= JLZE001, #total population
         male_18_19 =  JLZE007, 
         male_20= JLZE008,  male_21= JLZE009, #to be pop ages 20-34
         male_22_24= JLZE010, male_25_29= JLZE011, 
         male_30_34= JLZE012,
         female_18_19 =  JLZE031, female_20= JLZE032, 
         female_21= JLZE033, female_22_24= JLZE034, 
         female_25_29= JLZE035, female_30_34= JLZE036,
         bike_to_work = JM0E018, #workers who commute by bike
         tot_workers = JM0E001, #total workers
         pop_25_over =  JN9E001,
         male_bachelors =  JN9E015, male_masters= JN9E016, #bachelors or more by 25
         male_prof= JN9E017, male_phd= JN9E018,
         female_bachelors= JN9E032, female_masters= JN9E033,
         female_prof= JN9E034, female_phd= JN9E035,
         med_household_inc=JOIE001, #median household income
         white=JMJE003, #Race white not hispanic
         black=JMJE004, #black not hispanic
         med_home_val=JTIE001) %>% #median home value
  clean_names() %>%
  mutate(across(tot_pop:med_home_val, as.numeric)) %>%
  mutate(pct_bachelors = ifelse(pop_25_over==0, 0, (male_bachelors + male_masters+
                                                      male_prof + male_phd + female_bachelors + 
                                                      female_masters + female_prof + female_phd)/pop_25_over))  %>%
  mutate(minority = tot_pop - white) %>%
  mutate(pop_18_34 = rowSums(across(male_18_19:female_30_34))) %>%
  select(gisjoin, year, state, county, tracta, tot_pop, pct_bachelors,  
         bike_to_work, tot_workers, med_household_inc, 
         white, black, minority, med_home_val, pop_18_34) %>%
  rename_with(~str_c(., "_2010"), tot_pop:pop_18_34)


#Inflation Adjustment:
#According to: https://data.bls.gov/pdq/SurveyOutputServlet
#in Jan 2010: CPI= 220.086
#in Jan 2019: CPI = 260.122
#Thus inflation between those two times for US is:

inflation<- (260.122-220.086)/220.086 + 1

ct_2010_main <- ct_2010_main %>%
  mutate(med_household_inc_2010_inf_adj = med_household_inc_2010 * inflation)



ct_2019_main <- ct_2019 %>%
  select(GISJOIN, YEAR, STATE, COUNTY, TRACTA, CONCITA, BLKGRPA, 
         tot_pop=ALT0E001, #tot pop
         male_18_19 = ALT0E007,  #pop age 18-34
         male_20=ALT0E008, male_21=ALT0E009,
         male_22_24=ALT0E010, male_25_29=ALT0E011,
         male_30_34=ALT0E012, 
         female_18_19 = ALT0E031, female_20=ALT0E032,
         female_21=ALT0E033, female_22_24=ALT0E034,
         female_25_29=ALT0E035, female_30_34=ALT0E036, 
         white=ALUKE003, #race
         black=ALUKE004, 
         bike_to_work=ALU1E018, #bike to work
         pop_25_over = ALWFE001, #Degrees 25+  
         male_bachelors = ALWFE015, 
         male_masters = ALWFE016,
         male_prof = ALWFE017,
         male_phd = ALWFE018, 
         female_bachelors = ALWFE032, 
         female_masters = ALWFE033,
         female_prof = ALWFE034,
         female_phd = ALWFE035, 
         med_household_inc=ALW1E001, #median household income,\
         tot_workers=ALU1E001, #total workers
         housing_tot = AL0DM001, #housing tot
         housing_09 = AL0DM004, # housing 2000-09
         housing_99 = AL0DM005, # housing 1990-99
         housing_89 = AL0DM006, # housing 1980-89    
         housing_79 = AL0DM007, # housing 1970-79
         housing_69 = AL0DM008, # housing 1960-69
         housing_59 = AL0DM009, # housing 1950-59
         housing_49=  AL0DM010,  # housing 1940-49
         housing_39 = AL0DM011, # housing pre 1940
         med_home_val = AL1HE001 #median home value
         ) %>%
  clean_names() %>%
  mutate(across(tot_pop:med_home_val, as.numeric)) %>%
  mutate(pct_bachelors = ifelse(pop_25_over==0, 0, (male_bachelors + male_masters+
                                                      male_prof + male_phd + female_bachelors + 
                                                      female_masters + female_prof + female_phd)/pop_25_over))  %>%
  mutate(minority = tot_pop - white) %>%
  mutate(pop_18_34 = rowSums(across(male_18_19:female_30_34))) %>%
  mutate(pct_housing_built_1990_09 = (housing_09 + housing_99)/(housing_09 + housing_99 +
                                                                  housing_89 + housing_79 +
                                                                  housing_69 + housing_59 +
                                                                  housing_49+ housing_39)) %>%
  select(gisjoin, year, state, county, tracta, tot_pop, pct_bachelors,
         bike_to_work, tot_workers, med_household_inc, 
         white, black, minority, med_home_val, pop_18_34, 
         pct_housing_built_1990_09) %>%
  rename_with(~str_c(., "_2019"), tot_pop:pop_18_34)



#PART 3: JOIN DATA AND SELECT ONLY FOR CITIES OF INTEREST ======

#Minor Adjustment following guide here: https://www2.census.gov/library/publications/decennial/2010/sf10-1.pdf
ct_2019_main[ct_2019_main$gisjoin=="G0600370137000", "gisjoin"] <- "G0600370930401"

#Join census non-spatial data
ct_2010_2019 <- full_join(ct_2010_main, ct_2019_main, by = "gisjoin") %>%
  select(-c(year.x, county.x, tracta.x, year.y, state.y, county.y, tracta.y)) %>%
  rename(state = state.x) %>%
  mutate(change_ed = pct_bachelors_2019 - pct_bachelors_2010)
  


#Show that no cts in major cities have errors:
#ct_2010_2019_anti_1 <- anti_join(ct_2010_main, ct_2019_main, by = "gisjoin")
#ct_2010_2019_anti_2 <- anti_join(ct_2019_main, ct_2010_main, by = "gisjoin")

#Create a census_tract sf object:
city_bounds <- st_read("data_in_progress/city_bounds.geojson") %>%
  st_transform(2163)

#Updated selection process with 125m buffer and st_within to select polygons in prcoess
#ct_shp_city <- ct_shp
sel_ct <- st_within(ct_shp, city_bounds)
sel_ct <- lengths(sel_ct) > 0
ct_shp_city1 <- ct_shp[sel_ct, ]

#Get city associated with each tract (this likely can be sped up and implemented in above chunk)
ct_shp_city1 <- st_join(st_make_valid(ct_shp_city1), 
                        st_make_valid(city_bounds["city"]), 
                        largest=TRUE) %>%
  select(GEOID10, GISJOIN, city)




#Alt Part 4=========
bike_lanes <- st_read("data_in_progress/city_bikes.geojson") %>%
  st_transform(2163) %>%
  st_cast(to = "LINESTRING")

#Smaller data to test====
#Get Small data (Chicago)
#bike_lanes_chicago <- bike_lanes %>%
#  filter(city =='chicago')
#ct_chicago <- ct_shp_city1 %>%
#  filter(city=="Chicago")

#Run it:
#bike_lanes_chicago_split <- st_intersection(bike_lanes_chicago, ct_chicago)

#Visualize:
#bl_chi_viz <- st_buffer(bike_lanes_chicago_split, 75)
#bl_chi_viz_old <- st_buffer(bike_lanes_chicago, 75)

#lanes_no_segmentation <- tm_shape(ct_chicago) + 
#  tm_fill(alpha=0) + tm_borders(alpha=.3) +
#  tm_shape(bl_chi_viz_old) +   
#  tm_fill(col= "MAP_COLORS") + tm_borders(alpha = 0) 

#lanes_segmentation <- tm_shape(ct_chicago) + 
#  tm_fill(alpha=0) + tm_borders(alpha=.3) +
#  tm_shape(bl_chi_viz) + 
#  tm_fill(col= "MAP_COLORS") + tm_borders(alpha = 0) 
#tmap_arrange(lanes_no_segmentation, lanes_segmentation)

#Larger data====

#Split data by census tract, add segment length, convert to centroids
bike_lanes_split <-  st_intersection(bike_lanes, ct_shp_city1)
bike_lanes_split$segment_length <- st_length(bike_lanes_split)
bike_lanes_split <- st_centroid(bike_lanes_split)
bike_lanes_split <- drop_units(bike_lanes_split)

bike_lanes_split1 <- bike_lanes_split %>%
  group_by(GISJOIN, type, city.1) %>%
  summarize(sum_segment_length = sum(segment_length)) %>%
  as.data.frame() %>%
  select(-c(geometry, city.1)) %>%
  pivot_wider(names_from = type, values_from = sum_segment_length, values_fill = 0) %>%
  #rename(unknown = "NA") %>%
  mutate(total_lane_dist = rowSums(across(painted_lane:unknown))) %>%
  clean_names() 

ct_w_bikes <- left_join(ct_shp_city1, bike_lanes_split1, by = c("GISJOIN" = "gisjoin")) %>%
  replace_na(list("painted_lane" = 0, "shared_lane" = 0, 
                  "separated_path" = 0, "bvld" = 0, 
                  "protected_lane"=0, "unknown"=0)) %>%
  clean_names() %>%
  left_join(ct_2010_2019)


#Add Spatial Area Component
ct_w_bikes$area <- st_area(ct_w_bikes)
ct_w_bikes <- ct_w_bikes %>%
  mutate(painted_lane_rate = painted_lane/area) %>%
  mutate(shared_lane_rate = shared_lane/area) %>%
  mutate(separated_path_rate = separated_path/area) %>%
  mutate(bvld_rate = bvld/area) %>%
  mutate(unknown_rate = unknown/area) %>%
  mutate(protected_lane_rate = protected_lane/area) %>%
  mutate(total_lane_rate = total_lane_dist/area) %>%
  replace_na(list(total_lane_rate = 0))
  select(-c(painted_lane, shared_lane, separated_path, bvld, protected_lane,
            unknown, total_lane_dist))

#Separate to write:
ct_w_bikes_no_spatial <- ct_w_bikes %>%
  as.data.frame() %>%
  select(-geometry)

ct_w_bikes_spatial <- ct_w_bikes %>% 
  select(geoid10, gisjoin)
  

#Part 5: Write =========
write.csv(ct_w_bikes_no_spatial, "data_in_progress/ct_with_bike_after_04.csv")
st_write(ct_w_bikes_spatial, "data_in_progress/ct_shp_city1.geojson")




#bike_lanes_with_ct <- st_join(bike_lanes_split, ct_shp_city1, left=TRUE)
#bike_lanes_with_ct <- st_join(ct_shp_city1, bike_lanes_split, left=TRUE) %>%
#  as.data.frame() %>%
#  select(gisjoin = GISJOIN,
#         type, 
#         city = city.x, 
#         segment_length) %>%
#  group_by(gisjoin, type, city) %>%
#  summarize(sum_segment_length = sum(segment_length)) %>%
#  pivot_wider(names_from = type, values_from = sum_segment_length, values_fill = 0) %>%
#  mutate(total_lane_dist = rowSums(across(painted_lane:unknown))) %>%
#  select(-"NA")





#PART 4 adding bike lane distance to each CT==== 
#convert to line segments then to centroids
#bike_lanes <- st_read("data_in_progress/city_bikes.geojson") %>%
#  st_transform(2163) %>%
#  st_cast(to = "LINESTRING")
#bike_lanes$segment_length <- st_length(bike_lanes)
#bike_lanes <- st_centroid(bike_lanes) 
#bike_lanes <- drop_units(bike_lanes)

#Potential code to split up bike lanes more:
#bike_lanes_2 <- st_read("data_in_progress/city_bikes.geojson") %>%
#  st_transform(2163) %>%
#  st_cast(to = "LINESTRING")
#bike_lanes_2$id <- c(1:nrow(bike_lanes_2))
#bike_lanes_2 <- split_lines(bike_lanes_2, 50, id="id")

#Move distance measures from centroids to actual census tracts
#Gives a df that lists all census tracts and the length of each type of 

#bike_lanes_with_ct <- st_join(ct_shp_city1, bike_lanes, left=TRUE) %>%
#  as.data.frame() %>%
#  select(gisjoin = GISJOIN,
#         type, 
#         city = city.x, 
#         segment_length) %>%
#  group_by(gisjoin, type, city) %>%
#  summarize(sum_segment_length = sum(segment_length)) %>%
#  pivot_wider(names_from = type, values_from = sum_segment_length, values_fill = 0) %>%
#  mutate(total_lane_dist = rowSums(across(painted_lane:unknown))) %>%
#  select(-"NA")


#PART 5: Join bike lane data to census data and then write:
#ct_2010_2019_w_bike <- inner_join(ct_2010_2019, bike_lanes_with_ct) 
  

