#Assess Coefficient of Variation, Block group Measures:
#Note SE = z * MOE 
# CV = SE/ Estimate
# CV = z * MOE / Estimate
#https://en.wikipedia.org/wiki/Margin_of_error
#ACS z = .90
#https://ccrpc.org/wp-content/uploads/2015/02/american-community-survey-guide.pdf

bg_2009 <- read.csv("census_ipums_bgs/nhgis0011_ds195_20095_2009_blck_grp.csv") %>%
  select(RKYE001, #total population
         bike_to_work = RLZE018, #workers who commute by bike
         bike_to_work_moe = RLZM018, 
         pop_25_over = RM8E001,
         pop_25_over_moe = RM8M001,
         med_household_inc=RNHE001, #median household income
         med_household_inc_moe = RNHM001, 
         white = RLIE013, #Race white not hispanic
         white_moe = RLIM013, 
         black = RLIE014, #black not hispanic
         black_moe = RLIM014,
         med_home_val= RR7E001,
         med_home_val_moe = RR7M001)

col_of_interest = c("bike_to_work", "pop_25_over", "med_household_inc", "white", "black", "med_home_val")

bg_2009_cv <- bg_2009 %>%
  mutate(bike_to_work_cv = .9*bike_to_work_moe/bike_to_work) %>%
  mutate(pop_25_over_cv = .9*pop_25_over_moe/pop_25_over) %>%
  mutate(med_household_inc_cv = .9*med_household_inc_moe/med_household_inc) %>%
  mutate(white_cv = .9*white_moe/white) %>%
  mutate(black_cv = .9*black_moe/black) %>%
  mutate(med_home_val_cv = .9*med_home_val_moe/med_home_val) %>%
  select(bike_to_work_cv, pop_25_over_cv, med_household_inc_cv, white_cv,
         black_cv, med_home_val_cv)

summary(bg_2009_cv, na.rm = T)
#Conclusion: While data is a little fuzzy, cvs of above .12 are not super strong a
#and .4 are really an issue. Suggests correct spatial scale is not bgs 


