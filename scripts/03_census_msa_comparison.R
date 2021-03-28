#Creating MSA Comparison for Gent Index:


library(tidyverse)
library(tidylog)
library(janitor)

msa10 <- read.csv("data_raw/census_msa_ipums2/nhgis0017_ds176_20105_2010_cbsa.csv")
msa19 <- read.csv("data_raw/census_msa_ipums2/nhgis0017_ds244_20195_2019_cbsa.csv")

city_names_10 <- c("Atlanta-Sandy Springs-Marietta","Baltimore-Towson", 
                "Boston-Cambridge-Quincy",
                "Buffalo-Niagara Falls", "Charlotte-Gastonia-Rock Hill", "Chicago-Joliet-Naperville", 
                "Cleveland-Elyria-Mentor",
                "Columbus", "Dallas-Fort Worth-Arlington", "Denver-Aurora-Broomfield", 
                "Detroit-Warren-Livonia", "Hartford-West Hartford-East Hartford", 
                "Houston-Sugar Land-Baytown", "Indianapolis-Carmel", "Jacksonville",
                "Kansas City", "Las Vegas-Paradise", "Los Angeles-Long Beach-Santa Ana", 
                "Louisville/Jefferson County", "Memphis", "Miami-Fort Lauderdale-Pompano Beach", 
                "Milwaukee-Waukesha-West Allis", "Minneapolis-St. Paul-Bloomington", "Nashville-Davidson--Murfreesboro--Franklin", 
                "New Orleans-Metairie-Kenner", "New York-Northern New Jersey-Long Island",
                "Oklahoma City", "Orlando-Kissimmee-Sanford", "Philadelphia-Camden-Wilmington", 
                "Phoenix-Mesa-Glendale", "Pittsburgh",
                "Portland-Vancouver-Hillsboro", "Providence-New Bedford-Fall River", "Raleigh-Cary", 
                "Sacramento--Arden-Arcade--Roseville", 
                "Salt Lake City", "San Antonio-New Braunfels", "San Diego-Carlsbad-San Marcos", "San Francisco-Oakland-Fremont",
                "San Jose-Sunnyvale-Santa Clara", "Seattle-Tacoma-Bellevue", "St. Louis", 
                "Tampa-St. Petersburg-Clearwater", "Virginia Beach-Norfolk-Newport News", 
                "Washington-Arlington-Alexandria")


city_names_19 <- c("Atlanta-Sandy Springs-Alpharetta", 
                   "Baltimore-Columbia-Towson", "Boston-Cambridge-Newton",
                "Buffalo-Cheektowaga", "Charlotte-Concord-Gastonia", "Chicago-Naperville-Elgin", 
                "Cleveland-Elyria", "Columbus", "Dallas-Fort Worth-Arlington", 
                "Denver-Aurora-Lakewood", "Detroit-Warren-Dearborn", 
                "Hartford-East Hartford-Middletown", 
                "Houston-The Woodlands-Sugar Land", "Indianapolis-Carmel-Anderson", "Jacksonville",
                "Kansas City", "Las Vegas-Henderson-Paradise", "Los Angeles-Long Beach-Anaheim", 
                "Louisville/Jefferson County", "Memphis", "Miami-Fort Lauderdale-Pompano Beach", 
                "Milwaukee-Waukesha", "Minneapolis-St. Paul-Bloomington", "Nashville-Davidson--Murfreesboro--Franklin", 
                "New Orleans-Metairie", "New York-Newark-Jersey City",
                "Oklahoma City", "Orlando-Kissimmee-Sanford", "Philadelphia-Camden-Wilmington", 
                "Phoenix-Mesa-Chandler", "Pittsburgh",
                "Portland-Vancouver-Hillsboro", "Providence-Warwick", "Raleigh-Cary", 
                "Sacramento-Roseville-Folsom", 
                "Salt Lake City", "San Antonio-New Braunfels", "San Diego-Chula Vista-Carlsbad",
                "San Francisco-Oakland-Berkeley",
                "San Jose-Sunnyvale-Santa Clara", "Seattle-Tacoma-Bellevue", "St. Louis", 
                "Tampa-St. Petersburg-Clearwater", "Virginia Beach-Norfolk-Newport News", 
                "Washington-Arlington-Alexandria")

msa10_cleaned <- msa10 %>%
  select(GISJOIN, YEAR, CBSA,
         pop_over_25 = JN9E001,
         male_bachelors= JN9E015,
         male_masters =  JN9E016,  
         male_prof=  JN9E017,
         male_phd= JN9E018,
         female_bachelors= JN9E032 ,
         female_masters= JN9E033,
         female_prof = JN9E034,
         female_phd = JN9E035,
         pop_25_over = JN9E001,
         med_household_inc_10 = JOIE001,
         tot_housing_through_10 = JSDE001,
         built_90_99 = JSDE004,
         built_00_04 = JSDE003, 
         built_05_10 = JSDE002, 
         med_house_value_10 = JTIE001) %>%
  mutate(ed = rowSums(across(male_bachelors:female_phd))) %>%
  mutate(ed_10 = ed / pop_25_over) %>%
  mutate(housing_last_20_10 = rowSums(across(built_90_99:built_05_10))) %>%
  mutate(pct_housing_last_20_10 = housing_last_20_10/tot_housing_through_10) %>%
  select(GISJOIN, YEAR, CBSA, ed_10, pct_housing_last_20_10, med_household_inc_10, med_house_value_10, pop_25_over) %>%
  mutate(CBSA = gsub(",.*$", "", CBSA)) %>%
  filter(CBSA %in% city_names_10) %>%
  arrange(desc(pop_25_over)) %>%
  distinct(CBSA, .keep_all=TRUE) %>% #Gets rid of duplicates (that have lower pop)
  select(-pop_25_over)

msa10_cleaned$GISJOIN[msa10_cleaned$CBSA == "Los Angeles-Long Beach-Santa Ana"] <- "G31080"
  

msa19_cleaned <- msa19 %>%
  select(GISJOIN, YEAR, CBSA,         
         pop_over_25 = ALWFE001, 
         male_bachelors= ALWFE015,
         male_masters =  ALWFE016,  
         male_prof=  ALWFE017,
         male_phd= ALWFE018,
         female_bachelors= ALWFE032 ,
         female_masters= ALWFE032,
         female_prof = ALWFE034,
         female_phd = ALWFE035,
         pop_25_over = ALWFE001,
         med_house_value_19 = AL1HM001) %>%
  mutate(ed = rowSums(across(male_bachelors:female_phd))) %>%
  mutate(ed_19 = ed / pop_25_over) %>%
  select(GISJOIN, YEAR, CBSA, ed_19, med_house_value_19, pop_over_25) %>%
  mutate(CBSA = gsub(",.*$", "", CBSA)) %>%
  filter(CBSA %in% city_names_19) %>%
  arrange(desc(pop_over_25)) %>%
  distinct(CBSA, .keep_all=TRUE) %>% #Gets rid of duplicates (that have lower pop)
  select(-pop_over_25)

cities <- full_join(msa10_cleaned, msa19_cleaned, by = "GISJOIN") %>%
  mutate(change_ed = ed_19 - ed_10) %>%
  select(-c(YEAR.x, CBSA.x, YEAR.y, ed_10, ed_19)) %>%
  rename(city = CBSA.y) %>%
  relocate(GISJOIN, city, change_ed, med_household_inc_10, med_house_value_10, 
           med_house_value_19) %>%
  mutate(city = gsub("-.*$", "", city))

#Add in Ft. Worth and St. Paul with same data as Dallas/Minneapolis:
ft_worth <- cities %>%
  filter(city == "Dallas")
ft_worth["city"] <- "Fort Worth"

st_paul <- cities %>%
  filter(city=="Minneapolis")
st_paul["city"] <- "St. Paul"

cities <- rbind(cities, ft_worth, st_paul)


#Inflation Adjustment:
#According to: https://data.bls.gov/pdq/SurveyOutputServlet
#in Jan 2010: CPI= 220.086
#in Jan 2019: CPI = 260.122
#Thus inflation between those two times for US is:

inflation<- (260.122-220.086)/220.086 + 1
cities <- cities %>%
  mutate(med_household_inc_10_city_infl_adjusted = med_household_inc_10 *inflation)


#unjoined <- anti_join(msa10_cleaned, msa19_cleaned, by = "GISJOIN")
#unjoined2 <- anti_join(msa19_cleaned, msa10_cleaned, by = "GISJOIN")


#Rename Louisville/Jefferson County Louisville:
cities$city[cities$city == "Louisville/Jefferson County"] <- "Louisville"

#This  

write.csv(cities, "data_in_progress/cities.csv")


