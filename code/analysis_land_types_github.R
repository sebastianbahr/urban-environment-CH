# Analysis land types Switzerland
# Author: Sebastian Bahr

library(tidyverse)
library(haven)
library(car)
library(sandwich)
library(lmtest)
library(stats)
library(lme4)
library(plm)
library(parameters)
library(psych)
library(lavaan)
library(semPlot)
library(multiwayvcov)

"%ni%" <- Negate("%in%")

ICC <- function(var_l2, var_l1){
  print(var_l2/(var_l2+var_l1))
}


# Load data ----
dir_h = "YOUR_DIRECTORY/"

shp21 = read_csv(paste0(dir_h, "shp_stat_analysis.csv"))


# Add children dummy and children squared ----
shp21$children_dummy = ifelse(shp21$nchildren >= 1, 1, 0)
shp21$children_u5_dummy = ifelse(shp21$n_children_u5 >= 1, 1, 0)
shp21$children_17_5_dummy = ifelse(shp21$n_children_17_5 >= 1, 1, 0)
shp21$children_o17_dummy = ifelse(shp21$n_children_o17 >= 1, 1, 0)
shp21$nchildren2 = shp21$nchildren*shp21$nchildren

# Add age squared ----
shp21$age2 = shp21$age * shp21$age
shp21$age_25 = shp21$age - 25
shp21$age_25_2 = shp21$age_25*shp21$age_25

# Education categorical ----
shp21 = shp21 %>% 
  mutate(educat = case_when(educat <= 1 ~ 1,
                            educat > 1 & educat <= 6 ~2,
                            educat >= 7 ~3,
                            TRUE ~ NA_real_))

# Sport dummy ----
shp21$sport_d = ifelse(shp21$sport==0, 0 ,1)

# Problem with the environment dummy ----
shp21$prob_env = ifelse(shp21$prob_env==2, 0, 1)

# Swiss region ----
shp21$CH_region = shp21$CH_region + 1


# Group land types ----
shp21$trees_630 = shp21$trees_forest_630 + shp21$trees_park_630 + shp21$trees_other_630
shp21$meadow_garden_parks_630 = shp21$meadow_garden_630 + shp21$meadow_parks_630
shp21$meadow_garden_parks_210 = shp21$meadow_garden_210 + shp21$meadow_parks_210
shp21$meadow_630 = shp21$meadow_garden_parks_630 + shp21$meadow_recreation_630 + shp21$meadow_playground_630 + shp21$meadow_other_630

# summary statistics exogenous variable: life satisfaction ----
summary(shp21$lifesat[shp21$pop_density >= 1500])
sd(shp21$lifesat[shp21$pop_density >= 1500])
nrow(subset(shp21, !is.na(shp21$lifesat) & shp21$pop_density >= 1500))
hist(shp21$lifesat)

# summary statistic endogenous variables: greenery ----

# trees forest
summary(shp21$trees_forest_630)
hist(shp21$trees_forest_630)

summary(shp21$trees_forest_630[shp21$pop_density >= 1500])
sd(shp21$trees_forest_630[shp21$pop_density >= 1500])
nrow(subset(shp21, !is.na(shp21$trees_forest_630) & shp21$pop_density >= 1500))

# trees parks
summary(shp21$trees_park_630)
hist(shp21$trees_park_630)

summary(shp21$trees_park_630[shp21$pop_density >= 1500])
sd(shp21$trees_park_630[shp21$pop_density >= 1500])
nrow(subset(shp21, !is.na(shp21$trees_park_630) & shp21$pop_density >= 1500))

# trees other 
summary(shp21$trees_other_630)
hist(shp21$trees_other_630)

summary(shp21$trees_other_630[shp21$pop_density >= 1500])
sd(shp21$trees_other_630[shp21$pop_density >= 1500])
nrow(subset(shp21, !is.na(shp21$trees_other_630) & shp21$pop_density >= 1500))


# meadows parks
summary(shp21$meadow_garden_parks_630)
hist(shp21$meadow_garden_parks_630)

summary(shp21$meadow_garden_parks_630[shp21$pop_density >= 1500])
sd(shp21$meadow_garden_parks_630[shp21$pop_density >= 1500])
nrow(subset(shp21, !is.na(shp21$meadow_garden_parks_630) & shp21$pop_density >= 1500))

# meadows recreation
summary(shp21$meadow_recreation_630)
hist(shp21$meadow_recreation_630)

summary(shp21$meadow_recreation_630[shp21$pop_density >= 1500])
sd(shp21$meadow_recreation_630[shp21$pop_density >= 1500])
nrow(subset(shp21, !is.na(shp21$meadow_recreation_630) & shp21$pop_density >= 1500))

# meadows playground
summary(shp21$meadow_playground_630)
hist(shp21$meadow_playground_630)

summary(shp21$meadow_playground_630[shp21$pop_density >= 1500])
sd(shp21$meadow_playground_630[shp21$pop_density >= 1500])
nrow(subset(shp21, !is.na(shp21$meadow_playground_630) & shp21$pop_density >= 1500))

# meadows other
summary(shp21$meadow_other_630)
hist(shp21$meadow_other_630)

summary(shp21$meadow_other_630[shp21$pop_density >= 1500])
sd(shp21$meadow_other_630[shp21$pop_density >= 1500])
nrow(subset(shp21, !is.na(shp21$meadow_other_630) & shp21$pop_density >= 1500))


# agriculture allotments
summary(shp21$agri_allotments_630)
hist(shp21$agri_allotments_630)

summary(shp21$agri_allotments_630[shp21$pop_density >= 1500])
sd(shp21$agri_allotments_630[shp21$pop_density >= 1500])
nrow(subset(shp21, !is.na(shp21$agri_allotments_630) & shp21$pop_density >= 1500))

# agriculture other
summary(shp21$agri_other_630)
hist(shp21$agri_other_630)

summary(shp21$agri_other_630[shp21$pop_density >= 1500])
sd(shp21$agri_other_630[shp21$pop_density >= 1500])
nrow(subset(shp21, !is.na(shp21$agri_other_630) & shp21$pop_density >= 1500))

# correlation matrix green land types
cor(shp21[, c("lifesat", "trees_forest_630", "trees_park_630", "trees_other_630",
              "meadow_recreation_630", "meadow_garden_parks_630",
              "meadow_playground_630", "meadow_other_630", "agri_other_630",
              "agri_allotments_630")])


# summary statistic endogenous variables: land use mix ----

# residential
summary(shp21$residential_630)
hist(shp21$residential_630)

# commercial
summary(shp21$commercial_630)
hist(shp21$commercial_630)

# groceries 
summary(shp21$groceries_630)
hist(shp21$groceries_630)


# commercial + groceries
summary(shp21$commercial_groceries_630)
hist(shp21$commercial_groceries_630)

# CCE
summary(shp21$CCE_630)
hist(shp21$CCE_630)

# recreation
summary(shp21$recreation_use_630)
hist(shp21$recreation_use_630)

# public services
summary(shp21$public_services_630)
hist(shp21$public_services_630)

# entroy 5
summary(shp21$entropy_5_630)
hist(shp21$entropy_5_630)

summary(shp21$entropy_5_630[shp21$pop_density >= 1500])
sd(shp21$entropy_5_630[shp21$pop_density >= 1500])
nrow(subset(shp21, !is.na(shp21$entropy_5_630) & shp21$pop_density >= 1500))

# entroy 6
summary(shp21$entropy_6_630)
hist(shp21$entropy_6_630)


# correlation matrix mixed land use
cor(shp21[, c("lifesat","residential_630", "commercial_630", "commercial_groceries_630",
              "CCE_630", "recreation_use_630", "public_services_630", "entropy_5_630",
              "entropy_6_630")])

# summary statistics covariates: sex, age, civstat, educat, occupa, children, hh_income, owner, ----
# residence_type, pop_density, distance_to_center, CH_region, satisfaction with accomodation, problems with environment and health 

# sex
summary(shp21$sex)
hist(shp21$sex)

summary(shp21$sex[shp21$pop_density >= 1500])
sd(shp21$sex[shp21$pop_density >= 1500])
nrow(subset(shp21, !is.na(shp21$sex) & shp21$pop_density >= 1500))

# age
summary(shp21$age)
hist(shp21$age)

summary(shp21$age[shp21$pop_density >= 1500])
sd(shp21$age[shp21$pop_density >= 1500])
nrow(subset(shp21, !is.na(shp21$age) & shp21$pop_density >= 1500))

# civstat
summary(shp21$civstat)
table(shp21$civstat)
nrow(subset(shp21, !is.na(shp21$civstat) & shp21$pop_density >= 1500))

# educat
summary(shp21$educat)
table(shp21$educat)
nrow(subset(shp21, !is.na(shp21$educat) & shp21$pop_density >= 1500))

# occupa
summary(shp21$occupa)
table(shp21$occupa)
nrow(subset(shp21, !is.na(shp21$occupa) & shp21$pop_density >= 1500))

# Dummy if children under 5
summary(shp21$children_u5_dummy)
nrow(subset(shp21, !is.na(shp21$children_u5_dummy) & shp21$pop_density >= 1500))
  
  
# Dummy if children between 5 and 17
summary(shp21$children_17_5_dummy)
nrow(subset(shp21, !is.na(shp21$children_17_5_dummy) & shp21$pop_density >= 1500))
  
# Dummy if children over 18
summary(shp21$children_o17_dummy)
nrow(subset(shp21, !is.na(shp21$children_o17_dummy) & shp21$pop_density >= 1500))

# hh_income_log
summary(shp21$hh_eq_income_log)
hist(shp21$hh_eq_income_log)

summary(shp21$hh_eq_income[shp21$pop_density >= 1500])
sd(shp21$hh_eq_income[shp21$pop_density >= 1500], na.rm=T)
nrow(subset(shp21, !is.na(shp21$hh_eq_income) & shp21$pop_density >= 1500))

# hh_income_imp_log
summary(shp21$hh_eq_income_imp_log)
hist(shp21$hh_eq_income_imp_log) # some 0 values

# owner
summary(shp21$owner)
table(shp21$owner)

summary(shp21$owner[shp21$pop_density >= 1500])
nrow(subset(shp21, !is.na(shp21$owner) & shp21$pop_density >= 1500))

# residence_type
summary(shp21$residence_type)
table(shp21$residence_type)

summary(shp21$residence_type[shp21$pop_density >= 1500])
nrow(subset(shp21, !is.na(shp21$residence_type) & shp21$pop_density >= 1500))


# CH region
summary(shp21$CH_region)
table(shp21$CH_region)

summary(shp21$CH_region[shp21$pop_density >= 1500])
nrow(subset(shp21, !is.na(shp21$CH_region) & shp21$pop_density >= 1500))

# pop_density
summary(shp21$pop_density)
hist(shp21$pop_density) # maybe log

summary(shp21$pop_density[shp21$pop_density >= 1500])
sd(shp21$pop_density[shp21$pop_density >= 1500])
nrow(subset(shp21, !is.na(shp21$pop_density) & shp21$pop_density >= 1500))

# distance_to_center
summary(shp21$distance_to_center_2)
hist(shp21$distance_to_center_2) # maybe log

summary(shp21$distance_to_center_2[shp21$pop_density >= 1500])
sd(shp21$distance_to_center_2[shp21$pop_density >= 1500])
nrow(subset(shp21, !is.na(shp21$distance_to_center_2) & shp21$pop_density >= 1500))

# problem with environment
summary(shp21$prob_env)
hist(shp21$prob_env) # maybe log

summary(shp21$prob_env[shp21$pop_density >= 1500])
sd(shp21$prob_env[shp21$pop_density >= 1500], na.rm=T)
nrow(subset(shp21, !is.na(shp21$prob_env) & shp21$pop_density >= 1500))

# physical activity in day per week
summary(shp21$sport)
hist(shp21$sport) # maybe log

summary(shp21$sport[shp21$pop_density >= 1500])
sd(shp21$sport[shp21$pop_density >= 1500], na.rm=T)
nrow(subset(shp21, !is.na(shp21$sport) & shp21$pop_density >= 1500))

# feelings of anxiety
summary(shp21$depression)
hist(shp21$depression) # maybe log

summary(shp21$depression[shp21$pop_density >= 1500])
sd(shp21$depression[shp21$pop_density >= 1500], na.rm=T)
nrow(subset(shp21, !is.na(shp21$depression) & shp21$pop_density >= 1500))




# restrict sample to postcode with high population density ----

shp21 = subset(shp21, pop_density >= 1500)

table(shp21$center_name)
mean(table(shp21$center_name))
length(unique(shp21$npa))
length(unique(shp21$center_name))


# Trimmed variables ----



shp21$trees_forest_630_trimmed = ifelse(quantile(shp21$trees_forest_630, p=0.95) <= shp21$trees_forest_630, NA, shp21$trees_forest_630)
shp21$trees_park_630_trimmed = ifelse(quantile(shp21$trees_park_630, p=0.95) <= shp21$trees_park_630, NA, shp21$trees_park_630)
shp21$trees_other_630_trimmed = ifelse(quantile(shp21$trees_other_630, p=0.95) <= shp21$trees_other_630, NA, shp21$trees_other_630)

shp21$meadow_parks_630_trimmed = ifelse(quantile(shp21$meadow_parks_630, p=0.975) <= shp21$meadow_parks_630, NA, shp21$meadow_parks_630)
shp21$meadow_recreation_630_trimmed = ifelse(quantile(shp21$meadow_recreation_630, p=0.975) <= shp21$meadow_recreation_630, NA, shp21$meadow_recreation_630)
shp21$meadow_garden_630_trimmed = ifelse(quantile(shp21$meadow_garden_630, p=0.975) <= shp21$meadow_garden_630, NA, shp21$meadow_garden_630)
shp21$meadow_playground_630_trimmed = ifelse(quantile(shp21$meadow_playground_630, p=0.975) <= shp21$meadow_playground_630, NA, shp21$meadow_playground_630)
shp21$meadow_other_630_trimmed = ifelse(quantile(shp21$meadow_other_630, p=0.975) <= shp21$meadow_other_630, NA, shp21$meadow_other_630)

shp21$agri_other_630_trimmed = ifelse(quantile(shp21$agri_other_630, p=0.975) <= shp21$agri_other_630, NA, shp21$agri_other_630)
shp21$agri_allotments_630_trimmed = ifelse(quantile(shp21$agri_allotments_630, p=0.975) <= shp21$agri_allotments_630, NA, shp21$agri_allotments_630)

shp21$entropy_5_630_trimmed = ifelse(quantile(shp21$entropy_5_630, p=0.025) <= shp21$entropy_5_630 & 
                                             quantile(shp21$entropy_5_630, p=0.975) >= shp21$entropy_5_630, shp21$entropy_5_630, NA)
shp21$entropy_6_630_trimmed = ifelse(quantile(shp21$entropy_6_630, p=0.025) <= shp21$entropy_6_630 & 
                                             quantile(shp21$entropy_6_630, p=0.975) >= shp21$entropy_6_630, shp21$entropy_6_630, NA)

# Standardize variables ----

shp21 = shp21 %>% mutate(lifesat_sd = scale(lifesat),
                         accommodation_sat_sd = scale(accommodation_sat), 
                         age_sd = scale(age),
                         age2_sd = scale(age2), 
                         hh_eq_income_log_sd = scale(hh_eq_income_log),
                         hh_eq_income_imp_log_sd = scale(hh_eq_income_imp_log),
                         pop_density_sd = scale(pop_density),
                         distance_to_center_2_sd = scale(distance_to_center_2),
                         trees_forest_630_sd = scale(trees_forest_630),
                         trees_park_630_sd = scale(trees_park_630),
                         trees_other_630_sd = scale(trees_other_630),
                         trees_630_sd = scale(trees_630),
                         trees_forest_210_sd = scale(trees_forest_630),
                         trees_park_210_sd = scale(trees_park_210),
                         trees_other_210_sd = scale(trees_other_210),
                         meadow_parks_630_sd = scale(meadow_parks_210),
                         meadow_recreation_630_sd = scale(meadow_recreation_630),
                         meadow_garden_630_sd = scale(meadow_garden_630),
                         meadow_playground_630_sd = scale(meadow_playground_630),
                         meadow_other_630_sd = scale(meadow_other_630),
                         meadow_garden_parks_630_sd = scale(meadow_garden_parks_630),
                         meadow_630_sd = scale(meadow_630),
                         meadow_recreation_210_sd = scale(meadow_recreation_210),
                         meadow_playground_210_sd = scale(meadow_playground_210),
                         meadow_other_210_sd = scale(meadow_other_210),
                         meadow_garden_parks_210_sd = scale(meadow_garden_parks_210),
                         agri_other_630_sd = scale(agri_other_630),
                         agri_allotments_630_sd = scale(agri_allotments_630),
                         agri_other_210_sd = scale(agri_other_210),
                         agri_allotments_210_sd = scale(agri_allotments_210),
                         entropy_5_630_sd = scale(entropy_5_630),
                         entropy_6_630_sd = scale(entropy_6_630),
                         entropy_5_210_sd = scale(entropy_5_210),
                         sport_sd = scale(sport),
                         depression_sd = scale(depression),
                         age_class_10 = case_when(age < 30 ~ 1,
                                               age >= 30 & age < 40 ~ 2,
                                               age >= 40 & age < 50 ~ 3,
                                               age >= 50 & age < 60 ~ 4,
                                               age >= 60 & age < 70 ~ 5,
                                               age >= 70 & age < 80 ~ 6,
                                               age >= 80  ~ 7),
                         age_class_5 = case_when(age < 25 ~ 1,
                                                  age >= 25 & age < 30 ~ 2,
                                                  age >= 30 & age < 35 ~ 3,
                                                  age >= 35 & age < 40 ~ 4,
                                                  age >= 40 & age < 45 ~ 5,
                                                  age >= 45 & age < 50 ~ 6,
                                                  age >= 50 & age < 55 ~ 7,
                                                  age >= 55 & age < 60 ~ 8,
                                                  age >= 60 & age < 65 ~ 9,
                                                  age >= 65 & age < 70 ~ 10,
                                                  age >= 70 & age < 75 ~ 11,
                                                  age >= 75 & age < 80 ~ 12,
                                                  age >= 80  ~ 13),)


shp21 = shp21 %>% mutate(trees_forest_630_imp = ifelse(trees_forest_630 == 0, median(trees_forest_630), trees_forest_630),
                         trees_forest_630_d = ifelse(trees_forest_630 == 0, 1, 0),
                         trees_park_630_imp = ifelse(trees_park_630 == 0, median(trees_park_630), trees_park_630),
                         trees_park_630_d = ifelse(trees_park_630 == 0, 1, 0),
                         trees_other_630_imp = ifelse(trees_other_630 == 0, median(trees_other_630), trees_other_630),
                         trees_other_630_d = ifelse(trees_other_630 == 0, 1, 0),
                         meadow_parks_630_imp = ifelse(meadow_parks_630 == 0, median(meadow_parks_630), meadow_parks_630),
                         meadow_parks_630_d = ifelse(meadow_parks_630 == 0, 1, 0),
                         meadow_recreation_630_imp = ifelse(meadow_recreation_630 == 0, median(meadow_recreation_630), meadow_recreation_630),
                         meadow_recreation_630_d = ifelse(meadow_recreation_630 == 0, 1, 0),
                         meadow_garden_630_imp = ifelse(meadow_garden_630 == 0, median(meadow_garden_630), meadow_garden_630),
                         meadow_garden_630_d = ifelse(meadow_garden_630 == 0, 1, 0),
                         meadow_playground_630_imp = ifelse(meadow_playground_630 == 0, median(meadow_playground_630), meadow_playground_630),
                         meadow_playground_630_d = ifelse(meadow_playground_630 == 0, 1, 0),
                         meadow_garden_parks_630_imp = ifelse(meadow_garden_parks_630 == 0, median(meadow_garden_parks_630), meadow_garden_parks_630),
                         meadow_garden_parks_630_d = ifelse(meadow_garden_parks_630 == 0, 1, 0),
                         agri_allotments_630_imp = ifelse(agri_allotments_630 == 0, mean(agri_allotments_630), agri_allotments_630),
                         agri_allotments_630_d = ifelse(agri_allotments_630 == 0, 1, 0),
                         agri_other_630_imp = ifelse(agri_other_630 == 0, mean(agri_other_630), agri_other_630),
                         agri_other_630_d = ifelse(agri_other_630 == 0, 1, 0),)




# Explore variance ----

# overall variance
var(shp21$trees_forest_630)
var(shp21$trees_park_630)
var(shp21$trees_other_630)


# Life satisfaction
# within variance
shp21$lifesat_city_mean = ave(shp21$lifesat, shp21$center_name)
sum((shp21$lifesat - shp21$lifesat_city_mean)^2)/(length(shp21$lifesat) -1 )
# 2.228

# between variance
lifesat_grandmean = mean(shp21$lifesat)
lifesat_city_mean = aggregate(shp21$lifesat, list(shp21$center_name), mean)
lifesat_city_mean
sum((lifesat_city_mean$x - lifesat_grandmean)^2)/(length(lifesat_city_mean$x) -1 )
# 0.159


# Trees forest
# within variance
shp21$trees_forest_630_city_mean = ave(shp21$trees_forest_630, shp21$center_name)
sum((shp21$trees_forest_630 - shp21$trees_forest_630_city_mean)^2)/(length(shp21$trees_forest_630) -1 )
# 0.001261052

# between variance
trees_forest_630_grandmean = mean(shp21$trees_forest_630)
trees_forest_630_city_mean = aggregate(shp21$trees_forest_630, list(shp21$center_name), mean)
trees_forest_630_city_mean
sum((trees_forest_630_city_mean$x - trees_forest_630_grandmean)^2)/(length(trees_forest_630_city_mean$x) -1 )
# 0.0006188983

# Trees park
# within variance
shp21$trees_park_630_city_mean = ave(shp21$trees_park_630, shp21$center_name)
sum((shp21$trees_park_630 - shp21$trees_park_630_city_mean)^2)/(length(shp21$trees_park_630) -1 )
# 0.00009

# between variance
trees_park_630_grandmean = mean(shp21$trees_park_630)
trees_park_630_city_mean = aggregate(shp21$trees_park_630, list(shp21$center_name), mean)
trees_park_630_city_mean
sum((trees_park_630_city_mean$x - trees_park_630_grandmean)^2)/(length(trees_park_630_city_mean$x) -1 )
# 0.00005

# Trees other
# within variance
shp21$trees_other_630_city_mean = ave(shp21$trees_other_630, shp21$center_name)
sum((shp21$trees_other_630 - shp21$trees_other_630_city_mean)^2)/(length(shp21$trees_other_630) -1 )
# 0.005

# between variance
trees_other_630_grandmean = mean(shp21$trees_other_630)
trees_other_630_city_mean = aggregate(shp21$trees_other_630, list(shp21$center_name), mean)
trees_other_630_city_mean
sum((trees_other_630_city_mean$x - trees_other_630_grandmean)^2)/(length(trees_other_630_city_mean$x) -1 )
# 0.005

# Meadows park
# within variance
shp21$meadow_parks_630_city_mean = ave(shp21$meadow_parks_630, shp21$center_name)
sum((shp21$meadow_parks_630 - shp21$meadow_parks_630_city_mean)^2)/(length(shp21$meadow_parks_630) -1 )
# 0.00009

# between variance
meadow_parks_630_grandmean = mean(shp21$meadow_parks_630)
meadow_parks_630_city_mean = aggregate(shp21$meadow_parks_630, list(shp21$center_name), mean)
meadow_parks_630_city_mean
sum((meadow_parks_630_city_mean$x - meadow_parks_630_grandmean)^2)/(length(meadow_parks_630_city_mean$x) -1 )
# 0.00005

# Meadows recreation
# within variance
shp21$meadow_recreation_630_city_mean = ave(shp21$meadow_recreation_630, shp21$center_name)
sum((shp21$meadow_recreation_630 - shp21$meadow_recreation_630_city_mean)^2)/(length(shp21$meadow_recreation_630) -1 )
# 0.00005

# between variance
meadow_recreation_630_grandmean = mean(shp21$meadow_recreation_630)
meadow_recreation_630_city_mean = aggregate(shp21$meadow_recreation_630, list(shp21$center_name), mean)
meadow_recreation_630_city_mean
sum((meadow_recreation_630_city_mean$x - meadow_parks_630_grandmean)^2)/(length(meadow_recreation_630_city_mean$x) -1 )
# 0.000036

# Meadows garden
# within variance
shp21$meadow_garden_630_city_mean = ave(shp21$meadow_garden_630, shp21$center_name)
sum((shp21$meadow_garden_630 - shp21$meadow_garden_630_city_mean)^2)/(length(shp21$meadow_garden_630) -1 )
# 0.00019

# between variance
meadow_garden_630_grandmean = mean(shp21$meadow_garden_630)
meadow_garden_630_city_mean = aggregate(shp21$meadow_garden_630, list(shp21$center_name), mean)
meadow_garden_630_city_mean
sum((meadow_garden_630_city_mean$x - meadow_garden_630_grandmean)^2)/(length(meadow_garden_630_city_mean$x) -1 )
# 0.00005

# Meadows playground
# within variance
shp21$meadow_playground_630_city_mean = ave(shp21$meadow_playground_630, shp21$center_name)
sum((shp21$meadow_playground_630 - shp21$meadow_playground_630_city_mean)^2)/(length(shp21$meadow_playground_630) -1 )
# 0.0000004

# between variance
meadow_playground_630_grandmean = mean(shp21$meadow_playground_630)
meadow_playground_630_city_mean = aggregate(shp21$meadow_playground_630, list(shp21$center_name), mean)
meadow_playground_630_city_mean
sum((meadow_playground_630_city_mean$x - meadow_playground_630_grandmean)^2)/(length(meadow_playground_630_city_mean$x) -1 )
# 0.0000004

# Meadows other
# within variance
shp21$meadow_other_630_city_mean = ave(shp21$meadow_other_630, shp21$center_name)
sum((shp21$meadow_other_630 - shp21$meadow_other_630_city_mean)^2)/(length(shp21$meadow_other_630) -1 )
# 0.004

# between variance
meadow_other_630_grandmean = mean(shp21$meadow_other_630)
meadow_other_630_city_mean = aggregate(shp21$meadow_other_630, list(shp21$center_name), mean)
meadow_other_630_city_mean
sum((meadow_other_630_city_mean$x - meadow_other_630_grandmean)^2)/(length(meadow_other_630_city_mean$x) -1 )
# 0.007

# Agriculture allotments
# within variance
shp21$agri_allotments_630_city_mean = ave(shp21$agri_allotments_630, shp21$center_name)
sum((shp21$agri_allotments_630 - shp21$agri_allotments_630_city_mean)^2)/(length(shp21$agri_allotments_630) -1 )
# 0.00000026

# between variance
agri_allotments_630_grandmean = mean(shp21$agri_allotments_630)
agri_allotments_630_city_mean = aggregate(shp21$agri_allotments_630, list(shp21$center_name), mean)
agri_allotments_630_city_mean
sum((agri_allotments_630_city_mean$x - agri_allotments_630_grandmean)^2)/(length(agri_allotments_630_city_mean$x) -1 )
# 0.00000009

# Agriculture other
# within variance
shp21$agri_other_630_city_mean = ave(shp21$agri_other_630, shp21$center_name)
sum((shp21$agri_other_630 - shp21$agri_other_630_city_mean)^2)/(length(shp21$agri_other_630) -1 )
# 0.0089

# between variance
agri_other_630_grandmean = mean(shp21$agri_other_630)
agri_other_630_city_mean = aggregate(shp21$agri_other_630, list(shp21$center_name), mean)
agri_other_630_city_mean
sum((agri_other_630_city_mean$x - agri_other_630_grandmean)^2)/(length(agri_other_630_city_mean$x) -1 )
# 0.0128

# Mixed land use
# within variance
shp21$entropy_5_630_city_mean = ave(shp21$entropy_5_630, shp21$center_name)
sum((shp21$entropy_5_630 - shp21$entropy_5_630_city_mean)^2)/(length(shp21$entropy_5_630) -1 )
# 0.0172

# between variance
entropy_5_630_grandmean = mean(shp21$entropy_5_630)
entropy_5_630_city_mean = aggregate(shp21$entropy_5_630, list(shp21$center_name), mean)
entropy_5_630_city_mean
sum((entropy_5_630_city_mean$x - entropy_5_630_grandmean)^2)/(length(entropy_5_630_city_mean$x) -1 )
# 0.0348



# Multilevel null model ----
mlm_0 = lmer(lifesat ~ 1 + (1| city), data=shp21)
summary(mlm_0)
ICC(0.04855, 2.10505) # 2% of variance on city level

# restrict sample to postcodes with density >= 1000
mlm_0 = lmer(lifesat ~ 1 + (1| city), data=subset(shp21, pop_density>=1500))
summary(mlm_0)
ICC(0.0327, 2.2487) # 2% of variance on city level


mlm_0 = lmer(lifesat ~ 1 + (1| center_name), data=shp21)
summary(mlm_0)
ICC(0.03824, 2.11950) # 2% of variance on city level

# restrict sample to postcodes with density >= 1000
mlm_0 = lmer(lifesat ~ 1 + (1| center_name), data=subset(shp21, pop_density>=1000))
summary(mlm_0)
ICC(0.03356, 2.19497) # 2% of variance on city level

# regular OLS can be used


# Multiple OLS regression with tree land type ----


# all age groups
fit = lm(lifesat_sd ~
           sex +
           age +
           I(age^2) +
           factor(civstat) +
           factor(educat) +
           factor(occupa) +
           hh_eq_income_log +
           children_u5_dummy +
           children_17_5_dummy +
           children_o17_dummy +
           owner +
           factor(residence_type) +
           factor(CH_region) +
           pop_density_sd +
           distance_to_center_2_sd +
           trees_forest_630_sd +
           trees_park_630_sd + 
           trees_other_630_sd,
         data=subset(shp21, ))
summary(fit)
nobs(fit)
CL_SE = model_parameters(fit,
                         vcov = "vcovCL",
                         vcov_args=list(type="HC3",
                                        cluster=subset(shp21, )$idhous21))


crPlots(fit)

# imputed income
fit = lm(lifesat_sd ~
           sex +
           age +
           I(age^2) +
           factor(civstat) +
           factor(educat) +
           factor(occupa) +
           hh_eq_income_imp_log +
           children_u5_dummy +
           children_17_5_dummy +
           children_o17_dummy +
           owner +
           factor(residence_type) +
           factor(CH_region) +
           pop_density_sd +
           distance_to_center_2_sd +
           trees_forest_630_sd +
           trees_park_630_sd + 
           trees_other_630_sd,
         data=subset(shp21, ))
summary(fit)
nobs(fit)
CL_SE = model_parameters(fit,
                         vcov = "vcovCL",
                         vcov_args=list(type="HC3",
                                        cluster=subset(shp21, )$idhous21))


# with squared effect
fit = lm(lifesat_sd ~
           sex +
           age +
           factor(civstat) +
           factor(educat) +
           factor(occupa) +
           hh_eq_income_log +
           children_u5_dummy +
           children_17_5_dummy +
           children_o17_dummy +
           owner +
           factor(residence_type) +
           factor(CH_region) +
           pop_density_sd +
           distance_to_center_2_sd +
           trees_forest_630_sd +
           I(trees_forest_630_sd^2) +
           trees_park_630_sd + 
           I(trees_park_630_sd^2) +
           trees_other_630_sd +
           I(trees_other_630_sd^2),
         data=subset(shp21, ))
summary(fit)
nobs(fit)
CL_SE = model_parameters(fit,
                         vcov = "vcovCL",
                         vcov_args=list(type="HC3",
                                        cluster=subset(outliers_removed, )$idhous21))


# with 0 dummies
fit = lm(lifesat_sd ~
           sex +
           age +
           factor(civstat) +
           factor(educat) +
           factor(occupa) +
           hh_eq_income_log +
           children_u5_dummy +
           children_17_5_dummy +
           children_o17_dummy +
           owner +
           factor(residence_type) +
           factor(CH_region) +
           pop_density_sd +
           distance_to_center_2_sd +
           trees_forest_630_sd +
           trees_forest_630_d +
           trees_park_630_sd + 
           trees_park_630_d +
           trees_other_630_sd,
         data=subset(shp21))
summary(fit)
nobs(fit)
CL_SE = model_parameters(fit,
                         vcov = "vcovCL",
                         vcov_args=list(type="HC3",
                                        cluster=subset(shp21, )$idhous21))



# multilevel model with city random intercepts
mlm_1 = lmer(lifesat_sd ~ (1| center_name) +
               sex +
               age +
               factor(civstat) +
               factor(educat) +
               factor(occupa) +
               hh_eq_income_log +
               children_u5_dummy +
               children_17_5_dummy +
               children_o17_dummy +
               owner +
               factor(residence_type) +
               factor(CH_region) +
               pop_density_sd +
               distance_to_center_2_sd +
               trees_forest_630_sd +
               trees_park_630_sd +
               trees_other_630_sd,
             data=subset(shp21,))
summary(mlm_1)




# Sample restricted to >= 33 years
fit = lm(lifesat_sd ~
           sex +
           age +
           factor(civstat) +
           factor(educat) +
           factor(occupa) +
           hh_eq_income_log +
           children_u5_dummy +
           children_17_5_dummy +
           children_o17_dummy +
           owner +
           factor(residence_type) +
           factor(CH_region) +
           pop_density_sd +
           distance_to_center_2_sd +
           trees_forest_630_sd +
           trees_park_630_sd + 
           trees_other_630_sd,
         data=subset(shp21, age >= 33))
summary(fit)
nobs(fit)
CL_SE = model_parameters(fit,
                         vcov = "vcovCL",
                         vcov_args=list(type="HC3",
                                        cluster=subset(shp21, age>=33)$idhous21))


crPlots(fit)

# sample restricted to >= 40
fit = lm(lifesat_sd ~
           sex +
           age +
           factor(civstat) +
           factor(educat) +
           factor(occupa) +
           hh_eq_income_log +
           children_u5_dummy +
           children_17_5_dummy +
           children_o17_dummy +
           owner +
           factor(residence_type) +
           factor(CH_region) +
           pop_density_sd +
           distance_to_center_2_sd +
           trees_forest_630_sd +
           trees_park_630_sd + 
           trees_other_630_sd,
         data=subset(shp21, age >= 40))
summary(fit)
nobs(fit)
CL_SE = model_parameters(fit,
                         vcov = "vcovCL",
                         vcov_args=list(type="HC3",
                                        cluster=subset(shp21, age>=40)$idhous21))



# sample restricted to >= 45
fit = lm(lifesat_sd ~
           sex +
           age +
           factor(civstat) +
           factor(educat) +
           factor(occupa) +
           hh_eq_income_log +
           children_u5_dummy +
           children_17_5_dummy +
           children_o17_dummy +
           owner +
           factor(residence_type) +
           factor(CH_region) +
           pop_density_sd +
           distance_to_center_2_sd +
           trees_forest_630_sd +
           trees_park_630_sd + 
           trees_other_630_sd,
         data=subset(shp21, age >= 45))
summary(fit)
nobs(fit)
CL_SE = model_parameters(fit,
                         vcov = "vcovCL",
                         vcov_args=list(type="HC3",
                                        cluster=subset(shp21, age>=45)$idhous21))


# sample restricted to >= 50
fit = lm(lifesat_sd ~
           sex +
           age +
           factor(civstat) +
           factor(educat) +
           factor(occupa) +
           hh_eq_income_log +
           children_u5_dummy +
           children_17_5_dummy +
           children_o17_dummy +
           owner +
           factor(residence_type) +
           factor(CH_region) +
           pop_density_sd +
           distance_to_center_2_sd +
           trees_forest_630_sd +
           trees_park_630_sd + 
           trees_other_630_sd,
         data=subset(shp21, age >= 50))
summary(fit)
nobs(fit)
CL_SE = model_parameters(fit,
                         vcov = "vcovCL",
                         vcov_args=list(type="HC3",
                                        cluster=subset(shp21, age>=50)$idhous21))


# imputed income
fit = lm(lifesat_sd ~
           sex +
           age +
           factor(civstat) +
           factor(educat) +
           factor(occupa) +
           hh_eq_income_imp_log +
           children_u5_dummy +
           children_17_5_dummy +
           children_o17_dummy +
           owner +
           factor(residence_type) +
           factor(CH_region) +
           pop_density_sd +
           distance_to_center_2_sd +
           trees_forest_630_sd +
           trees_park_630_sd + 
           trees_other_630_sd,
         data=subset(shp21, age >= 33))
summary(fit)
nobs(fit)
CL_SE = model_parameters(fit,
                         vcov = "vcovCL",
                         vcov_args=list(type="HC3",
                                        cluster=subset(shp21, age>=33)$idhous21))


# with squared effect
fit = lm(lifesat_sd ~
           sex +
           age +
           factor(civstat) +
           factor(educat) +
           factor(occupa) +
           hh_eq_income_log +
           children_u5_dummy +
           children_17_5_dummy +
           children_o17_dummy +
           owner +
           factor(residence_type) +
           factor(CH_region) +
           pop_density_sd +
           distance_to_center_2_sd +
           trees_forest_630_sd +
           I(trees_forest_630_sd^2) +
           trees_park_630_sd + 
           I(trees_park_630_sd^2) +
           trees_other_630_sd +
           I(trees_other_630_sd^2),
         data=subset(shp21, age >= 33))
summary(fit)
nobs(fit)
CL_SE = model_parameters(fit,
                         vcov = "vcovCL",
                         vcov_args=list(type="HC3",
                                        cluster=subset(outliers_removed, age>=33)$idhous21))


# with 0 dummies
fit = lm(lifesat_sd ~
           sex +
           age +
           factor(civstat) +
           factor(educat) +
           factor(occupa) +
           hh_eq_income_log +
           children_u5_dummy +
           children_17_5_dummy +
           children_o17_dummy +
           owner +
           factor(residence_type) +
           factor(CH_region) +
           pop_density_sd +
           distance_to_center_2_sd +
           trees_forest_630_sd +
           trees_forest_630_d +
           trees_park_630_sd + 
           trees_park_630_d +
           trees_other_630_sd,
         data=subset(shp21, age >= 33))
summary(fit)
nobs(fit)
CL_SE = model_parameters(fit,
                         vcov = "vcovCL",
                         vcov_args=list(type="HC3",
                                        cluster=subset(outliers_removed, age>=33)$idhous21))



# multilevel model with city random intercepts
mlm_1 = lmer(lifesat_sd ~ (1| center_name) +
               sex +
               age +
               factor(civstat) +
               factor(educat) +
               factor(occupa) +
               hh_eq_income_log +
               children_u5_dummy +
               children_17_5_dummy +
               children_o17_dummy +
               owner +
               factor(residence_type) +
               factor(CH_region) +
               pop_density_sd +
               distance_to_center_2_sd +
               trees_forest_630_sd +
               trees_park_630_sd + 
               trees_other_630_sd,
             data=subset(shp21, age >= 33))
summary(mlm_1)



# Multiple OLS regression with meadows land types ----
# all age groups
fit = lm(lifesat_sd ~
           sex +
           age +
           I(age^2) +
           factor(civstat) +
           factor(educat) +
           factor(occupa) +
           hh_eq_income_log +
           children_u5_dummy +
           children_17_5_dummy +
           children_o17_dummy +
           owner +
           factor(residence_type) +
           factor(CH_region) +
           factor(CH_region) +
           pop_density_sd +
           distance_to_center_2_sd +
           meadow_garden_parks_630_sd +
           meadow_recreation_630_sd +
           meadow_playground_630_sd +
           meadow_other_630_sd,
         data=subset(shp21))
summary(fit)
nobs(fit)
CL_SE = model_parameters(fit,
                         vcov = "vcovCL",
                         vcov_args=list(type="HC3",
                                        cluster=subset(shp21)$idhous21))
crPlots(fit)


# imputed income
fit = lm(lifesat_sd ~
           sex +
           age +
           I(age^2) +
           factor(civstat) +
           factor(educat) +
           factor(occupa) +
           hh_eq_income_imp_log +
           children_u5_dummy +
           children_17_5_dummy +
           children_o17_dummy +
           owner +
           factor(residence_type) +
           factor(CH_region) +
           factor(CH_region) +
           pop_density_sd +
           distance_to_center_2_sd +
           meadow_garden_parks_630_sd +
           meadow_recreation_630_sd +
           meadow_playground_630_sd +
           meadow_other_630_sd,
         data=subset(shp21))
summary(fit)
nobs(fit)
CL_SE = model_parameters(fit,
                         vcov = "vcovCL",
                         vcov_args=list(type="HC3",
                                        cluster=subset(shp21)$idhous21))


# squared effects
fit = lm(lifesat_sd ~
           sex +
           age +
           factor(civstat) +
           factor(educat) +
           factor(occupa) +
           hh_eq_income_log +
           children_u5_dummy +
           children_17_5_dummy +
           children_o17_dummy +
           owner +
           factor(residence_type) +
           factor(CH_region) +
           pop_density_sd +
           distance_to_center_2_sd +
           meadow_garden_parks_630_sd +
           I(meadow_garden_parks_630_sd^2) +
           meadow_recreation_630_sd +
           I(meadow_recreation_630_sd^2) +
           meadow_playground_630_sd +
           I(meadow_playground_630_sd^2) +
           meadow_other_630_sd +
           I(meadow_other_630_sd^2),
         data=subset(shp21))
summary(fit)
nobs(fit)
CL_SE = model_parameters(fit,
                         vcov = "vcovCL",
                         vcov_args=list(type="HC3",
                                        cluster=subset(shp21)$idhous21))


# with 0 dummies
fit = lm(lifesat_sd ~
           sex +
           age +
           factor(civstat) +
           factor(educat) +
           factor(occupa) +
           hh_eq_income_log +
           children_u5_dummy +
           children_17_5_dummy +
           children_o17_dummy +
           owner +
           factor(residence_type) +
           factor(CH_region) +
           pop_density_sd +
           distance_to_center_2_sd +
           meadow_garden_parks_630_sd +
           meadow_garden_parks_630_d +
           meadow_recreation_630_sd +
           meadow_recreation_630_d +
           meadow_playground_630_sd +
           meadow_playground_630_d +
           meadow_other_630_sd,
         data=subset(shp21))
summary(fit)
nobs(fit)
CL_SE = model_parameters(fit,
                         vcov = "vcovCL",
                         vcov_args=list(type="HC3",
                                        cluster=subset(shp21)$idhous21))


# multilevel model with city random intercepts
mlm_1 = lmer(lifesat_sd ~ (1| center_name) +
               sex +
               age +
               factor(civstat) +
               factor(educat) +
               factor(occupa) +
               hh_eq_income_log +
               children_u5_dummy +
               children_17_5_dummy +
               children_o17_dummy +
               owner +
               factor(residence_type) +
               factor(CH_region) +
               pop_density_sd +
               distance_to_center_2_sd +
               meadow_garden_parks_630_sd +
               meadow_recreation_630_sd +
               meadow_playground_630_sd +
               meadow_other_630_sd,
             data=subset(shp21))
summary(mlm_1)



# sample restricted to >= 35 years
fit = lm(lifesat_sd ~
           sex +
           age +
           factor(civstat) +
           factor(educat) +
           factor(occupa) +
           hh_eq_income_log +
           children_u5_dummy +
           children_17_5_dummy +
           children_o17_dummy +
           owner +
           factor(residence_type) +
           factor(CH_region) +
           factor(CH_region) +
           pop_density_sd +
           distance_to_center_2_sd +
           meadow_garden_parks_630_sd +
           meadow_recreation_630_sd +
           meadow_playground_630_sd +
           meadow_other_630_sd,
         data=subset(shp21, age >= 35 ))
summary(fit)
nobs(fit)
CL_SE = model_parameters(fit,
                         vcov = "vcovCL",
                         vcov_args=list(type="HC3",
                                        cluster=subset(shp21, age>=35)$idhous21))


# sample restricted to >= 40 years
fit = lm(lifesat_sd ~
           sex +
           age +
           I(age^2) +
           factor(civstat) +
           factor(educat) +
           factor(occupa) +
           hh_eq_income_log +
           children_u5_dummy +
           children_17_5_dummy +
           children_o17_dummy +
           owner +
           factor(residence_type) +
           factor(CH_region) +
           factor(CH_region) +
           pop_density_sd +
           distance_to_center_2_sd +
           meadow_garden_parks_630_sd +
           meadow_recreation_630_sd +
           meadow_playground_630_sd +
           meadow_other_630_sd,
         data=subset(shp21, age >= 40))
summary(fit)
nobs(fit)
CL_SE = model_parameters(fit,
                         vcov = "vcovCL",
                         vcov_args=list(type="HC3",
                                        cluster=subset(shp21, age>=40)$idhous21))


# sample restricted to >= 45 years
fit = lm(lifesat_sd ~
           sex +
           age +
           I(age^2) +
           factor(civstat) +
           factor(educat) +
           factor(occupa) +
           hh_eq_income_log +
           children_u5_dummy +
           children_17_5_dummy +
           children_o17_dummy +
           owner +
           factor(residence_type) +
           factor(CH_region) +
           factor(CH_region) +
           pop_density_sd +
           distance_to_center_2_sd +
           meadow_garden_parks_630_sd +
           meadow_recreation_630_sd +
           meadow_playground_630_sd +
           meadow_other_630_sd,
         data=subset(shp21, age >= 45))
summary(fit)
nobs(fit)
CL_SE = model_parameters(fit,
                         vcov = "vcovCL",
                         vcov_args=list(type="HC3",
                                        cluster=subset(shp21, age>=45)$idhous21))


# sample restricted to >= 50 years
fit = lm(lifesat_sd ~
           sex +
           age +
           I(age^2) +
           factor(civstat) +
           factor(educat) +
           factor(occupa) +
           hh_eq_income_log +
           children_u5_dummy +
           children_17_5_dummy +
           children_o17_dummy +
           owner +
           factor(residence_type) +
           factor(CH_region) +
           factor(CH_region) +
           pop_density_sd +
           distance_to_center_2_sd +
           meadow_garden_parks_630_sd +
           meadow_recreation_630_sd +
           meadow_playground_630_sd +
           meadow_other_630_sd,
         data=subset(shp21, age >= 50))
summary(fit)
nobs(fit)
CL_SE = model_parameters(fit,
                         vcov = "vcovCL",
                         vcov_args=list(type="HC3",
                                        cluster=subset(shp21, age >=50)$idhous21))


# imputed income
fit = lm(lifesat_sd ~
           sex +
           age +
           I(age^2) +
           factor(civstat) +
           factor(educat) +
           factor(occupa) +
           hh_eq_income_imp_log +
           children_u5_dummy +
           children_17_5_dummy +
           children_o17_dummy +
           owner +
           factor(residence_type) +
           factor(CH_region) +
           factor(CH_region) +
           pop_density_sd +
           distance_to_center_2_sd +
           meadow_garden_parks_630 +
           meadow_recreation_630 +
           meadow_playground_630 +
           meadow_other_630,
         data=subset(shp21, age >= 35))
summary(fit)
nobs(fit)
CL_SE = model_parameters(fit,
                         vcov = "vcovCL",
                         vcov_args=list(type="HC3",
                                        cluster=subset(shp21, age>=35)$idhous21))
crPlots(fit)

# squared effects
fit = lm(lifesat_sd ~
           sex +
           age +
           I(age^2) +
           factor(civstat) +
           factor(educat) +
           factor(occupa) +
           hh_eq_income_log +
           children_u5_dummy +
           children_17_5_dummy +
           children_o17_dummy +
           owner +
           factor(residence_type) +
           factor(CH_region) +
           pop_density_sd +
           distance_to_center_2_sd +
           meadow_garden_parks_630_sd +
           I(meadow_garden_parks_630_sd^2) +
           meadow_recreation_630_sd +
           I(meadow_recreation_630_sd^2) +
           meadow_playground_630_sd +
           I(meadow_playground_630_sd^2) +
           meadow_other_630_sd +
           I(meadow_other_630_sd^2),
         data=subset(shp21, age >= 35))
summary(fit)
nobs(fit)
CL_SE = model_parameters(fit,
                         vcov = "vcovCL",
                         vcov_args=list(type="HC3",
                                        cluster=subset(shp21, age>=35)$idhous21))


# with 0 dummies
fit = lm(lifesat_sd ~
           sex +
           age +
           I(age^2) +
           factor(civstat) +
           factor(educat) +
           factor(occupa) +
           hh_eq_income_log +
           children_u5_dummy +
           children_17_5_dummy +
           children_o17_dummy +
           owner +
           factor(residence_type) +
           factor(CH_region) +
           pop_density_sd +
           distance_to_center_2_sd +
           meadow_garden_parks_630_sd +
           meadow_garden_parks_630_d +
           meadow_recreation_630_sd +
           meadow_recreation_630_d +
           meadow_playground_630_sd +
           meadow_playground_630_d +
           meadow_other_630_sd,
         data=subset(shp21, age >= 35))
summary(fit)
nobs(fit)
CL_SE = model_parameters(fit,
                         vcov = "vcovCL",
                         vcov_args=list(type="HC3",
                                        cluster=subset(shp21, age>=35)$idhous21))



# multilevel model with city random intercepts
mlm_1 = lmer(lifesat_sd ~ (1| center_name) +
               sex +
               age +
               I(age^2) +
               factor(civstat) +
               factor(educat) +
               factor(occupa) +
               hh_eq_income_log +
               children_u5_dummy +
               children_17_5_dummy +
               children_o17_dummy +
               owner +
               factor(residence_type) +
               factor(CH_region) +
               factor(CH_region) +
               pop_density_sd +
               distance_to_center_2_sd +
               meadow_garden_parks_630_sd +
               meadow_recreation_630_sd +
               meadow_playground_630_sd +
               meadow_other_630_sd,
             data=subset(shp21, age >= 35))
summary(mlm_1)


# Multiple OLS regression with agriculture land types ----

# all age groups
fit = lm(lifesat_sd ~
           sex +
           age +
           age2 +
           factor(civstat) +
           factor(educat) +
           factor(occupa) +
           hh_eq_income_log +
           children_u5_dummy +
           children_17_5_dummy +
           children_o17_dummy +
           owner +
           factor(residence_type) +
           factor(CH_region) +
           pop_density_sd +
           distance_to_center_2_sd +
           agri_other_630_sd +
           agri_allotments_630_sd,
         data=subset(shp21,))
summary(fit)
nobs(fit)
CL_SE = model_parameters(fit,
                         vcov = "vcovCL",
                         vcov_args=list(type="HC3",
                                        cluster=subset(shp21, )$idhous21))


# imputed income
fit = lm(lifesat_sd ~
           sex +
           age +
           age2 +
           factor(civstat) +
           factor(educat) +
           factor(occupa) +
           hh_eq_income_imp_log +
           children_u5_dummy +
           children_17_5_dummy +
           children_o17_dummy +
           owner +
           factor(residence_type) +
           factor(CH_region) +
           pop_density_sd +
           distance_to_center_2_sd +
           agri_other_630_sd +
           agri_allotments_630_sd,
         data=subset(shp21,))
summary(fit)
nobs(fit)
CL_SE = model_parameters(fit,
                         vcov = "vcovCL",
                         vcov_args=list(type="HC3",
                                        cluster=subset(shp21, )$idhous21))



# with squared effect
fit = lm(lifesat_sd ~
           sex +
           age +
           age2 +
           factor(civstat) +
           factor(educat) +
           factor(occupa) +
           hh_eq_income_log +
           children_u5_dummy +
           children_17_5_dummy +
           children_o17_dummy +
           owner +
           factor(residence_type) +
           factor(CH_region) +
           pop_density_sd +
           distance_to_center_2_sd +
           agri_other_630_sd +
           I(agri_other_630_sd^2) +
           agri_allotments_630_sd +
           I(agri_allotments_630_sd^2),
         data=subset(shp21,))
summary(fit)
nobs(fit)
CL_SE = model_parameters(fit,
                         vcov = "vcovCL",
                         vcov_args=list(type="HC3",
                                        cluster=subset(shp21, )$idhous21))


# with 0 dummy
fit = lm(lifesat_sd ~
           sex +
           age +
           age2 +
           factor(civstat) +
           factor(educat) +
           factor(occupa) +
           hh_eq_income_log +
           children_u5_dummy +
           children_17_5_dummy +
           children_o17_dummy +
           owner +
           factor(residence_type) +
           factor(CH_region) +
           pop_density_sd +
           distance_to_center_2_sd +
           agri_other_630_sd +
           agri_other_630_d +
           agri_allotments_630_sd,
         data=subset(shp21,))
summary(fit)
nobs(fit)
CL_SE = model_parameters(fit,
                         vcov = "vcovCL",
                         vcov_args=list(type="HC3",
                                        cluster=subset(shp21, )$idhous21))


# multilevel model with city random slopes
mlm_1 = lmer(lifesat_sd ~ (1| center_name) +
               sex +
               age +
               age2 +
               factor(civstat) +
               factor(educat) +
               factor(occupa) +
               hh_eq_income_log +
               children_u5_dummy +
               children_17_5_dummy +
               children_o17_dummy +
               owner +
               factor(residence_type) +
               factor(CH_region) +
               pop_density_sd +
               distance_to_center_2_sd +
               agri_other_630_sd +
               agri_allotments_630_sd,
             data=subset(shp21,))
summary(mlm_1)


# DAG mediation meadows garden and park, meadows playground ----

shp21$civstat_2 = ifelse(shp21$civstat==2, 1, 0)
shp21$civstat_3 = ifelse(shp21$civstat==3, 1, 0)
shp21$educat_2 = ifelse(shp21$educat==2, 1, 0)
shp21$educat_3 = ifelse(shp21$educat==3, 1, 0)
shp21$occupa_2 = ifelse(shp21$occupa==2, 1, 0)
shp21$occupa_3 = ifelse(shp21$occupa==3, 1, 0)
shp21$occupa_4 = ifelse(shp21$occupa==4, 1, 0)
shp21$occupa_5 = ifelse(shp21$occupa==5, 1, 0)
shp21$occupa_6 = ifelse(shp21$occupa==6, 1, 0)
shp21$residence_type_2 = ifelse(shp21$residence_type==2, 1, 0)
shp21$residence_type_3 = ifelse(shp21$residence_type==3, 1, 0)
shp21$CH_region_2 = ifelse(shp21$CH_region==1, 1, 0)
shp21$CH_region_3 = ifelse(shp21$CH_region==2, 1, 0)
shp21$health_1 = ifelse(shp21$health==1, 1, 0)
shp21$health_2 = ifelse(shp21$health==2, 1, 0)
shp21$health_3 = ifelse(shp21$health==3, 1, 0)
shp21$health_4 = ifelse(shp21$health==4, 1, 0)
shp21$entropy_5_630_age = shp21$entropy_5_630_sd * shp21$age



fit = lm(stress ~
           sex +
           age +
           factor(civstat) +
           factor(educat) +
           factor(occupa) +
           hh_eq_income_log +
           children_u5_dummy +
           children_17_5_dummy +
           children_o17_dummy +
           owner +
           factor(residence_type) +
           factor(CH_region) +
           pop_density_sd +
           distance_to_center_2_sd +
           trees_forest_630_sd +
           trees_park_630_sd + 
           trees_other_630_sd,
         data=subset(shp21, age >= 33))
summary(fit)
# possible mediators stress, depression, relationship sat



fit = lm(depression ~
           sex +
           age +
           hh_eq_income_log +
           children_u5_dummy +
           children_17_5_dummy +
           children_o17_dummy +
           owner +
           factor(residence_type) +
           factor(CH_region) +
           pop_density_sd +
           distance_to_center_2_sd +
           sport +
           meadow_playground_630_sd,
         data=subset(shp21,))
summary(fit)



fit = lm(lifesat_sd ~
           sex +
           age +
           factor(civstat) +
           factor(educat) +
           factor(occupa) +
           hh_eq_income_log +
           children_u5_dummy +
           children_17_5_dummy +
           children_o17_dummy +
           owner +
           factor(residence_type) +
           factor(CH_region) +
           factor(CH_region) +
           pop_density_sd +
           distance_to_center_2_sd +
           prob_env +
           sport_sd +
           depression_sd +
           meadow_garden_parks_630_sd +
           meadow_recreation_630_sd +
           meadow_playground_630_sd +
           meadow_other_630_sd,
         data=subset(shp21, age >= 35))
summary(fit)


fit = lm(depression ~
           sex +
           age +
           factor(civstat) +
           factor(educat) +
           factor(occupa) +
           hh_eq_income_log +
           children_u5_dummy +
           children_17_5_dummy +
           children_o17_dummy +
           owner +
           factor(residence_type) +
           factor(CH_region) +
           factor(CH_region) +
           pop_density_sd +
           distance_to_center_2_sd +
           prob_env +
           sport_sd +
           meadow_garden_parks_630_sd +
           meadow_recreation_630_sd +
           meadow_playground_630_sd +
           meadow_other_630_sd,
         data=subset(shp21, age >= 35))
summary(fit)


fit = lm(sport_sd ~
           sport +
           sex +
           age +
           factor(civstat) +
           factor(educat) +
           factor(occupa) +
           hh_eq_income_log +
           children_u5_dummy +
           children_17_5_dummy +
           children_o17_dummy +
           owner +
           factor(residence_type) +
           factor(CH_region) +
           factor(CH_region) +
           pop_density_sd +
           distance_to_center_2_sd,
           data=subset(shp21, age >= 33))
summary(fit)


# meadows
sem_model <- '#parallel mediation
prob_env ~ 
a*meadow_garden_parks_630_sd +
sex +
age +
civstat_2 +
civstat_3 +
educat_2 +
educat_3 +
occupa_2 + 
occupa_3 +
occupa_4 +
occupa_5 +
occupa_6 +
hh_eq_income_log_sd +
children_u5_dummy +
children_17_5_dummy +
children_o17_dummy +
owner +
residence_type_2 +
residence_type_3 +
CH_region_2 +
CH_region_3 +
pop_density_sd +
distance_to_center_2_sd +
meadow_recreation_630_sd +
meadow_playground_630_sd +
meadow_other_630_sd
sport_sd ~
d*meadow_playground_630_sd +
sex +
age +
civstat_2 +
civstat_3 +
educat_2 +
educat_3 +
occupa_2 + 
occupa_3 +
occupa_4 +
occupa_5 +
occupa_6 +
hh_eq_income_log_sd +
children_u5_dummy +
children_17_5_dummy +
children_o17_dummy +
owner +
residence_type_2 +
residence_type_3 +
CH_region_2 +
CH_region_3 +
pop_density_sd +
distance_to_center_2_sd +
meadow_recreation_630_sd +
meadow_garden_parks_630_sd +
meadow_other_630_sd
depression_sd ~ 
f*sport_sd +
h*prob_env +
sex +
age +
civstat_2 +
civstat_3 +
educat_2 +
educat_3 +
occupa_2 + 
occupa_3 +
occupa_4 +
occupa_5 +
occupa_6 +
hh_eq_income_log_sd +
children_u5_dummy +
children_17_5_dummy +
children_o17_dummy +
owner +
residence_type_2 +
residence_type_3 +
CH_region_2 +
CH_region_3 +
pop_density_sd +
distance_to_center_2_sd
lifesat_sd ~
b*prob_env +
c*meadow_garden_parks_630_sd +
e*sport_sd +
g*depression_sd +
sex +
age +
civstat_2 +
civstat_3 +
educat_2 +
educat_3 +
occupa_2 + 
occupa_3 +
occupa_4 +
occupa_5 +
occupa_6 +
hh_eq_income_log_sd +
children_u5_dummy +
children_17_5_dummy +
children_o17_dummy +
owner +
residence_type_2 +
residence_type_3 +
CH_region_2 +
CH_region_3 +
pop_density_sd +
distance_to_center_2_sd +
meadow_recreation_630_sd +
meadow_playground_630_sd +
meadow_other_630_sd
#indirect effect
ab:=a*b
fg:=f*g
#total effect
total:=c+(a*b)+(d*e)'
fit_par <- sem(sem_model, data = subset(shp21, age >= 35),
               se = "bootstrap", bootstrap = 1000, parallel = "snow", ncpus = 4, iseed = 1234) # se = "bootstrap"
summary(fit_par, fit.measures = TRUE)


# Multiple OLS regression with entropy ----

fit = lm(lifesat_sd ~
           sex +
           age_25 +
           factor(civstat) +
           factor(educat) +
           factor(occupa) +
           hh_eq_income_log +
           children_u5_dummy +
           children_17_5_dummy +
           children_o17_dummy +
           owner +
           factor(residence_type) +
           factor(CH_region) +
           pop_density_sd +
           distance_to_center_2_sd +
           entropy_5_630_sd*age_25,
         data=subset(shp21,))
summary(fit)
nobs(fit)
CL_SE = model_parameters(fit,
                         vcov = "vcovCL",
                         vcov_args=list(type="HC3",
                                        cluster=subset(shp21,)$idhous21))





# imputed income
fit = lm(lifesat_sd ~
           sex +
           age_25 +
           factor(civstat) +
           factor(educat) +
           factor(occupa) +
           hh_eq_income_imp_log +
           children_u5_dummy +
           children_17_5_dummy +
           children_o17_dummy +
           owner +
           factor(residence_type) +
           factor(CH_region) +
           pop_density_sd +
           distance_to_center_2_sd +
           entropy_5_630_sd*age_25,
         data=subset(shp21,))
summary(fit)
nobs(fit)
CL_SE = model_parameters(fit,
                         vcov = "vcovCL",
                         vcov_args=list(type="HC3",
                                        cluster=subset(shp21,)$idhous21))



# with age squared
# linear age effect no longer significant -> remove
fit = lm(lifesat_sd ~
           sex +
           I(age-25) +
           I((age-25)^2) +
           factor(civstat) +
           factor(educat) +
           factor(occupa) +
           hh_eq_income_log +
           children_u5_dummy +
           children_17_5_dummy +
           children_o17_dummy +
           owner +
           factor(residence_type) +
           factor(CH_region) +
           pop_density_sd +
           distance_to_center_2_sd +
           entropy_5_630*I(age-25),
         data=subset(shp21))
summary(fit)
nobs(fit)
CL_SE = model_parameters(fit,
                         vcov = "vcovCL",
                         vcov_args=list(type="HC3",
                                        cluster=subset(shp21,)$idhous21))

# with age binned in 5 year categories
fit = lm(lifesat_sd ~
           sex +
           age_class_5 +
           factor(civstat) +
           factor(educat) +
           factor(occupa) +
           hh_eq_income_log +
           children_u5_dummy +
           children_17_5_dummy +
           children_o17_dummy +
           owner +
           factor(residence_type) +
           factor(CH_region) +
           pop_density_sd +
           distance_to_center_2_sd +
           entropy_5_630_sd*age_class_5,
         data=subset(shp21,))
summary(fit)
nobs(fit)
CL_SE = model_parameters(fit,
                         vcov = "vcovCL",
                         vcov_args=list(type="HC3",
                                        cluster=subset(shp21,)$idhous21))

# with age binned in 10 year categories
fit = lm(lifesat_sd ~
           sex +
           age_class_10 +
           factor(civstat) +
           factor(educat) +
           factor(occupa) +
           hh_eq_income_log +
           children_u5_dummy +
           children_17_5_dummy +
           children_o17_dummy +
           owner +
           factor(residence_type) +
           factor(CH_region) +
           pop_density_sd +
           distance_to_center_2_sd +
           entropy_5_630_sd*age_class_10,
         data=subset(shp21,))
summary(fit)
nobs(fit)
CL_SE = model_parameters(fit,
                         vcov = "vcovCL",
                         vcov_args=list(type="HC3",
                                        cluster=subset(shp21,)$idhous21))




# multilevel model with city random slopes
mlm_1 = lmer(lifesat_sd ~ (1| center_name) +
               sex +
               I(age-25) +
               I((age-25)^2) +
               factor(civstat) +
               factor(educat) +
               factor(occupa) +
               hh_eq_income_log +
               children_u5_dummy +
               children_17_5_dummy +
               children_o17_dummy +
               owner +
               factor(residence_type) +
               factor(CH_region) +
               pop_density_sd +
               distance_to_center_2_sd +
               entropy_5_630*I(age-25),
             data=subset(shp21,))
summary(mlm_1)




sem_model <- '#parallel mediation
health ~ 
a*entropy_6_630_sd +
sex +
age +
civstat_2 +
civstat_3 +
educat_2 +
educat_3 +
occupa_2 + 
occupa_3 +
occupa_4 +
occupa_5 +
occupa_6 +
hh_eq_income_log_sd +
children_u5_dummy +
children_17_5_dummy +
children_o17_dummy +
owner +
residence_type_2 +
residence_type_3 +
CH_region_2 +
CH_region_3 +
pop_density_sd +
distance_to_center_2_sd
lifesat_sd ~ 
b*health +
c*entropy_6_630_age +
sex +
age +
civstat_2 +
civstat_3 +
educat_2 +
educat_3 +
occupa_2 + 
occupa_3 +
occupa_4 +
occupa_5 +
occupa_6 +
hh_eq_income_log_sd +
children_u5_dummy +
children_17_5_dummy +
children_o17_dummy +
owner +
residence_type_2 +
residence_type_3 +
CH_region_2 +
CH_region_3 +
pop_density_sd +
distance_to_center_2_sd
#indirect effect
ab:=a*b
#total effect
total:=c+(a*b)'
fit_par <- sem(sem_model, data = subset(shp21, age >= 33 & pop_density >= 1500), se = "robust") # se = "bootstrap"
summary(fit_par, fit.measures = TRUE)


# Robustness analysis 210 m tiles ----

# trees all observations
fit = lm(lifesat_sd ~
           sex +
           age +
           age2 +
           factor(civstat) +
           factor(educat) +
           factor(occupa) +
           hh_eq_income_log +
           children_u5_dummy +
           children_17_5_dummy +
           children_o17_dummy +
           owner +
           factor(residence_type) +
           factor(CH_region) +
           pop_density_sd +
           distance_to_center_2_sd +
           trees_forest_210_sd +
           trees_park_210_sd + 
           trees_other_210_sd,
         data=subset(shp21, ))
summary(fit)
nobs(fit)
CL_SE = model_parameters(fit,
                         vcov = "vcovCL",
                         vcov_args=list(type="HC3",
                                        cluster=subset(shp21, )$idhous21))



# trees Sample restricted to >= 33 years
fit = lm(lifesat_sd ~
           sex +
           age +
           factor(civstat) +
           factor(educat) +
           factor(occupa) +
           hh_eq_income_log +
           children_u5_dummy +
           children_17_5_dummy +
           children_o17_dummy +
           owner +
           factor(residence_type) +
           factor(CH_region) +
           pop_density_sd +
           distance_to_center_2_sd +
           trees_forest_210_sd +
           trees_park_210_sd + 
           trees_other_210_sd,
         data=subset(shp21, age >= 33))
summary(fit)
nobs(fit)
CL_SE = model_parameters(fit,
                         vcov = "vcovCL",
                         vcov_args=list(type="HC3",
                                        cluster=subset(shp21, age>=33)$idhous21))


# meadows all observations
fit = lm(lifesat_sd ~
           sex +
           age +
           age2 +
           factor(civstat) +
           factor(educat) +
           factor(occupa) +
           hh_eq_income_log +
           children_u5_dummy +
           children_17_5_dummy +
           children_o17_dummy +
           owner +
           factor(residence_type) +
           factor(CH_region) +
           factor(CH_region) +
           pop_density_sd +
           distance_to_center_2_sd +
           meadow_garden_parks_210_sd +
           meadow_recreation_210_sd +
           meadow_playground_210_sd +
           meadow_other_210_sd,
         data=subset(shp21, ))
summary(fit)
nobs(fit)
CL_SE = model_parameters(fit,
                         vcov = "vcovCL",
                         vcov_args=list(type="HC3",
                                        cluster=subset(shp21, )$idhous21))



# meadows sample restricted to >= 35 years
fit = lm(lifesat_sd ~
           sex +
           age +
           factor(civstat) +
           factor(educat) +
           factor(occupa) +
           hh_eq_income_log +
           children_u5_dummy +
           children_17_5_dummy +
           children_o17_dummy +
           owner +
           factor(residence_type) +
           factor(CH_region) +
           factor(CH_region) +
           pop_density_sd +
           distance_to_center_2_sd +
           meadow_garden_parks_210_sd +
           meadow_recreation_210_sd +
           meadow_playground_210_sd +
           meadow_other_210_sd,
         data=subset(shp21, age >= 35))
summary(fit)
nobs(fit)
CL_SE = model_parameters(fit,
                         vcov = "vcovCL",
                         vcov_args=list(type="HC3",
                                        cluster=subset(shp21, age>=35)$idhous21))




# agriculture all age groups
fit = lm(lifesat_sd ~
           sex +
           age +
           age2 +
           factor(civstat) +
           factor(educat) +
           factor(occupa) +
           hh_eq_income_log +
           children_u5_dummy +
           children_17_5_dummy +
           children_o17_dummy +
           owner +
           factor(residence_type) +
           factor(CH_region) +
           pop_density_sd +
           distance_to_center_2_sd +
           agri_other_210_sd +
           agri_allotments_210_sd,
         data=subset(shp21,))
summary(fit)
nobs(fit)
CL_SE = model_parameters(fit,
                         vcov = "vcovCL",
                         vcov_args=list(type="HC3",
                                        cluster=subset(shp21, )$idhous21))


# entropy

fit = lm(lifesat_sd ~
           sex +
           age_25 +
           factor(civstat) +
           factor(educat) +
           factor(occupa) +
           hh_eq_income_log +
           children_u5_dummy +
           children_17_5_dummy +
           children_o17_dummy +
           owner +
           factor(residence_type) +
           factor(CH_region) +
           pop_density_sd +
           distance_to_center_2_sd +
           entropy_5_210_sd*age_25,
         data=subset(shp21,))
summary(fit)
nobs(fit)
CL_SE = model_parameters(fit,
                         vcov = "vcovCL",
                         vcov_args=list(type="HC3",
                                        cluster=subset(shp21,)$idhous21))






plot(fit, 4)
model_data = fit[["model"]]
n = 1054
cooksD <- cooks.distance(fit)
influential_obs <- as.numeric(names(cooksD)[(cooksD > (4/n))])
outliers_removed_CD = shp21[-influential_obs, ]


dffits_ = dffits(fit)
p <- length(fit$coefficients)-1
tresh <- 2*sqrt(p/n)
influential_obs <- as.numeric(names(dffits_)[(dffits_ > tresh)])
outliers_removed_dffits = shp21[-influential_obs, ]

qqnorm(fit$residuals)
qqline(fit$residuals)
