#Regression Analysis

library(tidyverse)
library(tidylog)

ct <- read.csv("data_in_progress/ct_with_bike_clean.csv") 

ct_reg <- ct %>%
  select(city, city_size, city_region, is_not_gentrifying, is_gentrifying, 
         is_advantaged,  dist_to_cbd, pop_density, 
         pop_18_34_2019, total_lane_rate, city, minority_2019)

just_gent_classification <- lm(total_lane_rate ~ is_not_gentrifying + is_gentrifying, data = ct)
summary(just_gent_classification)

full_model <- lm(total_lane_rate ~ is_not_gentrifying + is_advantaged + 
                                    dist_to_cbd + pop_density + pop_18_34_2019 +
                                      minority_2019 + as.factor(city)
                 
                 , data = ct_reg)
summary(full_model)

theorized_model <- lm(total_lane_rate ~ is_not_gentrifying + is_advantaged + 
                        dist_to_cbd + pop_density + pop_18_34_2019
                      , data = ct_reg)
summary(theorized_model)





