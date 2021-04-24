#Regression Analysis

library(tidyverse)
library(tidylog)
library(stargazer)
library(corrgram)

ct <- read.csv("data_in_progress/ct_with_bike_clean.csv") 

ct_reg <- ct %>%
  select(city, city_size, city_region, is_not_gentrifying, is_gentrifying, 
         is_advantaged,  dist_to_cbd_km, pop_density, 
         pop_18_34_2019, total_lane_rate, city, minority_2019)

just_gent_classification <- lm(total_lane_rate ~ is_not_gentrifying + is_gentrifying, data = ct)
summary(just_gent_classification)

full_model <- lm(total_lane_rate ~ is_not_gentrifying + is_advantaged + 
                                    dist_to_cbd_km + pop_density + pop_18_34_2019 +
                                      minority_2019 + as.factor(city)
                 
                 , data = ct_reg)
summary(full_model)
stargazer(full_model,
          omit = "city",
          type = "latex",
          style = "jpam",
          single.row = TRUE,
          out="model_outputs/full_model.html")



theorized_model <- lm(total_lane_rate ~ is_not_gentrifying + is_advantaged + 
                        dist_to_cbd + pop_density + pop_18_34_2019
                      , data = ct_reg)
summary(theorized_model)


city_regions <- unique(ct$city_region)
run_subset_model <- function(category) {
  if (category %in% city_regions){
    df <- ct %>%
      filter(city_region == category)
  }
  else{
    df <- ct %>%
      filter(city_size == category)
  }
  model <- lm(total_lane_rate ~ is_not_gentrifying + is_advantaged + 
                dist_to_cbd_km + pop_density + pop_18_34_2019 +
                minority_2019, data = df)
}


midwest_model <- run_subset_model("midwest")
summary(midwest_model)

west_coast_model <- run_subset_model("west_coast")
summary(west_coast_model)

interior_west_model <- run_subset_model("interior_west")
summary(interior_west_model)

south_model <- run_subset_model("south")
summary(south_model)

northeast_model <- run_subset_model("northeast")
summary(northeast_model)

#Chart for City Regions Model:
stargazer(midwest_model, west_coast_model, interior_west_model, 
          south_model, northeast_model, 
          omit = "city",
          type = "latex",
          style = "jpam",
          single.row = FALSE,
          column.labels = c("Midwest", "West Coast", "Interior West",
                            "South", "Northeast"), 
          out="model_outputs/city_region_models.html")





extremely_large_model <- run_subset_model("extremely_large")
summary(extremely_large_model)

large_model <- run_subset_model("large")
summary(large_model)

medium_model <- run_subset_model("medium")
summary(medium_model)

#Chart for city_size_model: 
stargazer(medium_model, large_model, extremely_large_model, 
          omit = "city",
          type = "latex",
          style = "jpam",
          single.row = TRUE,
          column.labels = c("Medium", "Large", "Extremely Large"), 
          out="model_outputs/city_size_models.html")


#Distance to cbd and cycling infrastructure:
dist_cbd <- lm(total_lane_rate ~ dist_to_cbd_km, data = ct_reg)
summary(dist_cbd)

stargazer(dist_cbd, 
          omit = "city",
          type = "latex",
          style = "jpam",
          single.row = TRUE,
          column.labels = c("Medium", "Large", "Extremely Large"), 
          out="model_outputs/univariate_reg.html")



#Appendix 4: 
mean_dist_by_gent <- ct %>%
  group_by(gent_status) %>%
  summarize(mean(dist_to_cbd_km))

controls <- ct_reg %>%
  select(-c(is_not_gentrifying, is_gentrifying, is_advantaged)) %>%
  corrgram(
    lower.panel=corrgram::panel.ellipse,
    upper.panel=panel.cor,
    diag.panel=panel.density)
