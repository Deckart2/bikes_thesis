#Summary statistics 

library(tidyverse)
library(tidylog)
library(sf)
library(gridExtra)
library(scales)


ct <- read.csv("data_in_progress/ct_with_bike_clean.csv") %>%
  select(-X) 
  

#Summary Tables ====
#Table that shows mean of each lane type grouped by gentrification type
gent_type <- ct %>%
  group_by(gent_status) %>%
  summarize(total_lane_rate = mean(total_lane_rate, na.rm=T), 
            separated_path_rate = mean(separated_path_rate, na.rm=T),
            protected_lane_rate = mean(protected_lane_rate, na.rm=T),
            painted_lane_rate = mean(painted_lane_rate, na.rm=T),
            bvld_rate = mean(bvld_rate, na.rm=T), 
            shared_lane_rate = mean(shared_lane_rate, na.rm=T),
            unknown_rate = mean(unknown_rate, na.rm=T)) %>%
  select(-unknown_rate) %>%
  filter(gent_status != "NA")
  


#Table that shows total lane/area for gent type and region 
gent_region <- ct %>%
  group_by(gent_status, city_region) %>% 
  summarize(mean_total_lane_rate = mean(total_lane_rate, na.rm=T)) %>%
  pivot_wider(names_from = gent_status, values_from = mean_total_lane_rate) %>%
  select(-c("NA")) %>%
  mutate(across(advantaged:not_gentrifying, scientific))


write.table(gent_region, file="summary_tables/gent_region.txt",
            sep = ",", row.names = F, quote=F)


#Table that shows total lane/area for gent_type and city size
gent_size <- ct %>%
  group_by(gent_status, city_size) %>% 
  summarize(mean_total_lane_rate = mean(total_lane_rate, na.rm=T)) %>%
  pivot_wider(names_from = gent_status, values_from = mean_total_lane_rate) %>%
  select(-c("NA")) %>%
  mutate(across(advantaged:not_gentrifying, scientific))

write.table(gent_size, file="summary_tables/gent_size.txt",
            sep = ",", row.names = F, quote=F)


#Table that shows total lane distance for gent_type and each city
gent_city <- ct %>%
  group_by(gent_status, city) %>%
  summarize(mean_total_lane_rate = mean(total_lane_rate, na.rm=T)) %>%
  pivot_wider(names_from = gent_status, values_from = mean_total_lane_rate) %>%
  select(-c("NA")) %>%
  replace_na(list(gentrifying = 0)) %>%
  mutate(across(advantaged:not_gentrifying, scientific))

write.table(gent_city, file="summary_tables/gent_city.txt", 
                sep = ",", row.names=F, quote=F)

#Statistical summaries:

#Gentrifying vs not gentrifying ======
gentrifying <- ct %>%
  filter(is_gentrifying==1) %>%
  pull(total_lane_rate)
not_gentrifying <- ct %>%
  filter(is_gentrifying==0) %>%
  pull(total_lane_rate)
total_gent_t_test <- t.test(gentrifying, not_gentrifying,
                            alternative="two.sided", 
                            var.equal=FALSE)
total_gent_t_test
#p-value of two-sided Welch Two Sample t-test is 0.0027
regions <- unique(ct$city_region)

#Function below takes two arguments: 
#region_or_city_size is a specific city size or region 
#lane data: either bike_presence or total_lane_rate
#Returns the results of the t-test comparing the two groups


run_t_test <- function(region_or_city_size, lane_data) {
  if (region_or_city_size %in% regions){
    df_gent <- ct %>% 
      filter(is_gentrifying == 1) %>%
      filter(city_region == region_or_city_size) %>%
      pull(lane_data)
    df_not_gent <- ct %>%
      filter(is_gentrifying == 0) %>%
      filter(city_region == region_or_city_size) %>%
      pull(lane_data)
  }
  else{
    df_gent <- ct %>% 
      filter(is_gentrifying == 1) %>%
      filter(city_size == region_or_city_size) %>%
      pull(lane_data)
    df_not_gent <- ct %>%
      filter(is_gentrifying == 0) %>%
      filter(city_size == region_or_city_size) %>%
      pull(lane_data)
  }
    results <-t.test(df_gent, df_not_gent, 
                     alternative="two.sided",
                     var.equal=FALSE)
  }
 

cont_west_coast <- run_t_test("west_coast", "total_lane_rate")
disc_west_coast <- run_t_test("west_coast", "bike_presence")
cont_west_coast
disc_west_coast

cont_interior_west <- run_t_test("interior_west", "total_lane_rate")
disc_interior_west <- run_t_test("interior_west", "bike_presence")
cont_interior_west
disc_interior_west

cont_south <- run_t_test("south", "total_lane_rate")
disc_south <- run_t_test("south", "bike_presence")
cont_gent_south
disc_south

cont_northeast <- run_t_test("northeast", "total_lane_rate")
disc_northeast <- run_t_test("northeast", "bike_presence")
cont_northeast
disc_northeast

cont_midwest <- run_t_test("midwest", "total_lane_rate")
disc_midwest <- run_t_test("midwest", "bike_presence")
cont_midwest
disc_midwest

cont_extremely_large <- run_t_test("extremely_large", "total_lane_rate")
disc_extremely_large <- run_t_test("extremely_large", "bike_presence")
cont_extremely_large
disc_extremely_large

cont_large <- run_t_test("large", "total_lane_rate")
disc_large <- run_t_test("large", "bike_presence")
cont_large
disc_large

cont_medium <- run_t_test("medium", "total_lane_rate")
disc_medium <- run_t_test("medium", "bike_presence")
cont_medium
disc_medium

#Gentrifying vs not_advantaged======
not_ad <- ct %>%
  filter(!is.na(gent_status) | gent_status != "advantaged")

overall_gent <- not_ad %>%
  filter(gent_status== "gentrifying") %>%
  pull(total_lane_rate)
overall_disad <- not_ad %>%
  filter(gent_status == "not_gentrifying") %>%
  pull(total_lane_rate)


overall_gent_v_disadvantaged <- t.test(overall_gent, overall_disad, 
                                       alternative = "greater", 
                                       paired = FALSE,
                                       var.equal = FALSE)
overall_gent_v_disadvantaged


run_t_test_not_ad <- function(region_or_city_size, lane_data) {
  if (region_or_city_size %in% regions){
    df_gent <- not_ad %>% 
      filter(is_gentrifying == 1) %>%
      filter(city_region == region_or_city_size) %>%
      pull(lane_data)
    df_not_gent <- not_ad %>%
      filter(is_gentrifying == 0) %>%
      filter(city_region == region_or_city_size) %>%
      pull(lane_data)
  }
  else{
    df_gent <- not_ad %>% 
      filter(is_gentrifying == 1) %>%
      filter(city_size == region_or_city_size) %>%
      pull(lane_data)
    df_not_gent <- not_ad %>%
      filter(is_gentrifying == 0) %>%
      filter(city_size == region_or_city_size) %>%
      pull(lane_data)
  }
  results <-t.test(df_gent, df_not_gent, 
                   alternative="greater",
                   var.equal=FALSE)
}


cont_west_coast_not_ad <- run_t_test_not_ad("west_coast", "total_lane_rate")
disc_west_coast_not_ad <- run_t_test_not_ad("west_coast", "bike_presence")
cont_west_coast_not_ad
disc_west_coast_not_ad

cont_interior_west_not_ad <- run_t_test_not_ad("interior_west", "total_lane_rate")
disc_interior_west_not_ad <- run_t_test_not_ad("interior_west", "bike_presence")
cont_interior_west_not_ad
disc_interior_west_not_ad

cont_south_not_ad <- run_t_test_not_ad("south", "total_lane_rate")
disc_south_not_ad <- run_t_test_not_ad("south", "bike_presence")
cont_south_not_ad
disc_south_not_ad

cont_northeast_not_ad <- run_t_test_not_ad("northeast", "total_lane_rate")
disc_northeast_not_ad <- run_t_test_not_ad("northeast", "bike_presence")
cont_northeast_not_ad
disc_northeast_not_ad

cont_midwest_not_ad <- run_t_test_not_ad("midwest", "total_lane_rate")
disc_midwest_not_ad <- run_t_test_not_ad("midwest", "bike_presence")
cont_midwest_not_ad
disc_midwest_not_ad

cont_extremely_large_not_ad <- run_t_test_not_ad("extremely_large", "total_lane_rate")
disc_extremely_large_not_ad <- run_t_test_not_ad("extremely_large", "bike_presence")
cont_extremely_large_not_ad
disc_extremely_large_not_ad

cont_large_not_ad <- run_t_test_not_ad("large", "total_lane_rate")
disc_large_not_ad <- run_t_test_not_ad("large", "bike_presence")
cont_large_not_ad
disc_large_not_ad

cont_medium_not_ad <- run_t_test_not_ad("medium", "total_lane_rate")
disc_medium_not_ad <- run_t_test_not_ad("medium", "bike_presence")
cont_medium_not_ad
disc_medium_not_ad

#Gentrification Summary Statistics =====
gent_sum <- ct %>%
  group_by(city) %>%
  count(gent_status) %>%
  pivot_wider(names_from = "gent_status", values_from = "n")
gent_sum[2:5] <- gent_sum[-1]/rowSums(gent_sum[-1], na.rm=TRUE)


#Pairwise t-tests and ANOVA======
ct_no_unknown <- ct %>%
  filter(gent_status!="unknown")


whole_data <- pairwise.t.test(ct_no_unknown$total_lane_rate, ct_no_unknown$gent_status, 
                p.adjust.method = "bonferroni",
                alternative = "two.sided")
whole_data

run_pairwise_t  <- function(region_or_city_size) {
  if (region_or_city_size %in% regions){
    df <- not_ad %>% 
      filter(city_region == region_or_city_size)
  }
  else{
    df <- not_ad %>% 
      filter(city_size == region_or_city_size)
  }
  results <-pairwise.t.test(df$total_lane_rate, df$gent_status,
                   p.adjust.methods = "bonferroni",
                   var.equal = FALSE,
                   alternative="two.sided",
                   paired=FALSE,
                   pooled.sd = FALSE)
}

midwest_pairwise_t <- run_pairwise_t("midwest")
west_coast_pairwise_t <- run_pairwise_t("west_coast")
interior_west_pairwise_t <- run_pairwise_t("interior_west")
south_pairwise_t <- run_pairwise_t("south")
northeast_pairwise_t <- run_pairwise_t("northeast")
medium_pairwise_t <- run_pairwise_t("medium")
large_pairwise_t <- run_pairwise_t("large")
extremely_large_pairwise_t <- run_pairwise_t("extremely_large")

midwest_pairwise_t
west_coast_pairwise_t
interior_west_pairwise_t
south_pairwise_t
northeast_pairwise_t
medium_pairwise_t
large_pairwise_t
extremely_large_pairwise_t

#Tract Size Considerations =====
#Calculating rough area and side lengths for tracts
median_area_ct <- median(ct$area)

#Just square roots the average area: ie x*x 
ct_side_length_if_square <- sqrt(mean_area_ct)
# rectange with sides x and 3x has area 3x*x 
ct_side_length_if_rect_x_3x <- sqrt(mean_area_ct/3)


median_ratio <- median(ct$total_lane_rate)
mean_ratio <- mean(ct$total_lane_rate)

median_dist <- median_area_ct * median_ratio
mean_dist <- median_area_ct * mean_ratio
  
#Size of tract:
soccer_field <- 7140
soc_fields_per_tract <- mean_area_ct / soccer_field

