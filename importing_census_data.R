#Importing Census Data
#Likely to be updated with exactly what gentrification data I need



library(sf)
library(tidycensus)
library(tidyverse)
library(tigris)


#Get Block group Spatial Data:
sVarNames <- load_variables(2019, "acs5/subject", cache = TRUE) %>%
  filter(concept == "FINANCIAL CHARACTERISTICS")

sVarNamesCensus <- load_variables(2010, dataset = "sf3", cache = TRUE)

#Variables needed: 
# 2019: 
# Median Household income: Occupied housing units!!HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2018 INFLATION-ADJUSTED DOLLARS)!!Median household income (dollars)  S2503_C01_013
# Education:     Percent Civilian population 25 years and over!!Bachelor's degree or higher                     S2101_C02_030
# Median Housing Value : Owner-occupied housing units!!OWNER CHARACTERISTICS!!Median value (dollars) 	                S0103_C01_098  
# Housing Built 1980-99: Occupied housing units!!Occupied housing units!!YEAR STRUCTURE BUILT!!1980 to 1999     S2504_C01_012
# Housing built 2000-2009: Occupied housing units!!Occupied housing units!!YEAR STRUCTURE BUILT!!2000 to 2009   S2504_C01_011
# Housing built 2010-2013: Occupied housing units!!Occupied housing units!!YEAR STRUCTURE BUILT!!2010 to 2013   S2504_C01_010
# Housing build 2014 - 2019 Occupied housing units!!Occupied housing units!!YEAR STRUCTURE BUILT!!2014 or later S2504_C01_009

#2010:
#Ed: 





bg <- get_acs(geography = "block group",
              variables = c(totPop18 = "B01001_001",
                            med_household_inc = ),
              year = 2018,
              survey = "acs5",
              state = c("AL", "AZ", "AR", "CA", "CO", "CT", "DE", 
                        "FL", "GA", "ID", "IL", "IN", "IA", "KS",
                        "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS",
                        "MO", "MT", "NE", "NE", "NV", "NH", "NJ", "NM",
                        "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI",
                        "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA",
                        "WV", "WI", "WY"), 
              geometry = TRUE
              )

c_tract <-get_acs(geography = "tract",
                       variables = c(totPop18 = "B01001_001"),
                       year = 2018,
                       survey = "acs5",
                       state = c("AL", "AZ", "AR", "CA", "CO", "CT", "DE", 
                                 "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS",
                                 "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS",
                                 "MO", "MT", "NE", "NE", "NV", "NH", "NJ", "NM",
                                 "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI",
                                 "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA",
                                 "WV", "WI", "WY"), 
                       geometry = TRUE
)


#To test to see if Spatial Subset only grabs tracts in Chicago
c_tract_test_IL <-get_acs(geography = "tract",
                          variables = c(totPop18 = "B01001_001"),
                          year = 2018,
                          survey = "acs5", 
                          state = "IL",
                          geometry = TRUE) %>%
  st_transform(2163)


st_write(c_tract_test_IL, "c_tract_test_IL.geojson")


census_tract <- c_tract %>%
  separate(NAME, c("Tract", "County", "State"), sep = ",") %>%
  rename(population = estimate) %>%
  select(-variable)

st_write(census_tract, "data/2018_census_tracts.geojson")
