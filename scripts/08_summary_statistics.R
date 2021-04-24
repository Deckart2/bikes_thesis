#Summary statistics 

library(tidyverse)
library(tidylog)
library(sf)
library(gridExtra)
library(scales)
library(janitor)


ct <- read.csv("data_in_progress/ct_with_bike_clean.csv") %>%
  select(-X) 
  

#Summary Tables ====
#Table that shows mean of each lane type grouped by gentrification type; TABLE 2
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
  filter(gent_status != "NA") %>%
  mutate(across(total_lane_rate:shared_lane_rate, round, 1))
gent_type <- t(gent_type)

write.table(gent_type, file="summary_tables/gent_type.txt",
            sep = ",", row.names = T, col.names = F, quote=F)


#Pairwise t test to test for significance:
ct_no_unknown <- ct %>%
  filter(gent_status!="unknown")


whole_data <- pairwise.t.test(ct_no_unknown$total_lane_rate, ct_no_unknown$gent_status, 
                              p.adjust.method = "bonferroni",
                              alternative = "two.sided",
                              pool.sd = FALSE, 
                              var.equal=FALSE,
                              conf.level = 0.95)
whole_data

sep_lane <- pairwise.t.test(ct_no_unknown$separated_path_rate, ct_no_unknown$gent_status, 
                           p.adjust.method = "bonferroni",
                           alternative = "two.sided",
                           pool.sd = FALSE, 
                           var.equal=FALSE,
                           conf.level = 0.95)
sep_lane

protected_lane <- pairwise.t.test(ct_no_unknown$protected_lane_rate, ct_no_unknown$gent_status, 
                                  p.adjust.method = "bonferroni",
                                  alternative = "two.sided",
                                  pool.sd = FALSE, 
                                  var.equal=FALSE,
                                  conf.level = 0.95)
protected_lane

painted_lane <- pairwise.t.test(ct_no_unknown$painted_lane_rate, ct_no_unknown$gent_status, 
                                p.adjust.method = "bonferroni",
                                alternative = "two.sided",
                                pool.sd = FALSE, 
                                var.equal=FALSE,
                                conf.level = 0.95)
painted_lane

bike_bvld <- pairwise.t.test(ct_no_unknown$bvld_rate, ct_no_unknown$gent_status, 
                             p.adjust.method = "bonferroni",
                             alternative = "two.sided",
                             pool.sd = FALSE, 
                             var.equal=FALSE,
                             conf.level = 0.95)
bike_bvld

shared_lane <- pairwise.t.test(ct_no_unknown$shared_lane_rate, ct_no_unknown$gent_status, 
                               p.adjust.method = "bonferroni",
                               alternative = "two.sided",
                               pool.sd = FALSE, 
                               var.equal=FALSE,
                               conf.level = 0.95)
shared_lane







#Table that shows total lane/area for gent type and region 
gent_region <- ct %>%
  group_by(gent_status, city_region) %>% 
  summarize(mean_total_lane_rate = mean(total_lane_rate, na.rm=T)) %>%
  pivot_wider(names_from = gent_status, values_from = mean_total_lane_rate) %>%
  select(-c("NA")) %>%
  mutate(across(advantaged:not_gentrifying, round, 1))

write.table(gent_region, file="summary_tables/gent_region.txt",
            sep = ",", row.names = F, quote=F)


#Table that shows total lane/area for gent_type and city size
#Table 4
gent_size <- ct %>%
  group_by(gent_status, city_size) %>% 
  summarize(mean_total_lane_rate = mean(total_lane_rate, na.rm=T)) %>%
  pivot_wider(names_from = gent_status, values_from = mean_total_lane_rate) %>%
  select(-c("NA")) %>%
  mutate(across(advantaged:not_gentrifying, round, 1))

write.table(gent_size, file="summary_tables/gent_size.txt",
            sep = ",", row.names = F, quote=F)


#Table that shows total lane distance for gent_type and each city
#Table 3
gent_city <- ct %>%
  group_by(gent_status, city, city_size, city_region) %>%
  summarize(mean_total_lane_rate = mean(total_lane_rate, na.rm=T)) %>%
  pivot_wider(names_from = gent_status, values_from = mean_total_lane_rate) %>%
  select(-c("NA")) %>%
  replace_na(list(gentrifying = 0)) %>%
  mutate(across(advantaged:not_gentrifying, scientific))

write.table(gent_city, file="summary_tables/gent_city.txt", 
                sep = ",", row.names=F, quote=F)

#Statistical summaries:


#Gentrification Summary Statistics =====
#Table in appendix: Individual Cities' mean rates of bicycle infrastructure by gent status
gent_sum <- ct_no_unknown %>%
  group_by(city, gent_status) %>%
  summarize(mean_total_lane_rate = mean(total_lane_rate)) %>%
  pivot_wider(names_from = "gent_status", values_from = "mean_total_lane_rate") %>%
  mutate(across(advantaged:not_gentrifying, round, 1))

write.table(gent_sum, file="summary_tables/gent_sum.txt", 
            sep = ",", row.names=F, quote=F)

#Other table for appendix (ndication of city size and region)
city_description <- ct %>%
  select(city, city_region, city_size) %>%
  distinct() %>%
  arrange(city)

write.table(city_description, file="summary_tables/city_description.txt", 
            sep = ",", row.names=F, quote=F)

gent_tracts_by_city <-ct %>%
  group_by(city, gent_status) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = gent_status, values_from = count) %>%
  select(-"NA")


write.table(gent_tracts_by_city, file="summary_tables/gent_tracts_by_city.txt", 
            sep = ",", row.names=F, quote=F)

#Pairwise t-tests and ANOVA======
ct_no_unknown <- ct %>%
  filter(gent_status!="unknown")


whole_data <- pairwise.t.test(ct_no_unknown$total_lane_rate, ct_no_unknown$gent_status, 
                p.adjust.method = "bonferroni",
                alternative = "two.sided")
whole_data

regions <- unique(ct$city_region)
run_pairwise_t  <- function(region_or_city_size) {
  if (region_or_city_size %in% regions){
    df <- ct_no_unknown %>% 
      filter(city_region == region_or_city_size)
  }
  else{
    df <- ct_no_unknown %>% 
      filter(city_size == region_or_city_size)
  }
  results <-pairwise.t.test(df$total_lane_rate, df$gent_status,
                   p.adjust.methods = "bonferroni",
                   var.equal = FALSE,
                   alternative="two.sided",
                   pool.sd = FALSE)
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
med_ct_side_length_if_square <- sqrt(median_area_ct)
# rectangle with sides x and 3x has area 3x*x 
med_ct_side_length_if_rect_x_3x <- sqrt(median_area_ct/3)


median_ratio <- median(ct$total_lane_rate)

median_dist <- median_area_ct * median_ratio

#Size of tract:
soccer_field <- 7140
soc_fields_per_tract <- median_area_ct / soccer_field


#Relative difference in gent v disadvantaged ======

gent_city <- gent_city %>%
  mutate(across(advantaged:not_gentrifying, as.numeric)) %>%
  mutate(pct_dif_gent_disadvantaged = (gentrifying-not_gentrifying)/gentrifying) %>%
  mutate(pct_dif_gent_advantaged = (gentrifying-advantaged)/gentrifying)

gent_city_gent_v_dis <- arrange(gent_city, pct_dif_gent_disadvantaged)
gent_city_gent_v_dis$row_num <- 1:nrow(gent_city)

gent_dis <- ggplot(gent_city_gent_v_dis, aes(y=pct_dif_gent_disadvantaged, x = row_num, fill = city_size)) +
  geom_bar(position="dodge", stat="identity") + 
  labs(x = "City", 
       y = "Percent Difference",
       title = "Percent Difference in Disadvantaged and Gentrifying Cycling Infrastructure") +
  theme_classic() 

gent_city_gent_v_ad <- arrange(gent_city, pct_dif_gent_advantaged)
gent_city_gent_v_ad$row_num <- 1:nrow(gent_city)


gent_ad <- ggplot(gent_city_gent_v_ad, aes(y=pct_dif_gent_advantaged, x = row_num, , fill = city_size)) +
  geom_bar(position="dodge", stat="identity") + 
  labs(x = "City", 
       y = "Percent Difference",
       title = "Percent Difference in Advantaged and Gentrifying Cycling Infrastructure") +
  theme_classic() 

combined <- cbind(gent_dis, gent_ad)


