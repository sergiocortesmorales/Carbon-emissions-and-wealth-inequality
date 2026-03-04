#Install and load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here, dplyr, tidyr)

ineq <- readRDS(here("data", "ineq.rds"))

######################
### MISSINGS CHECK ###
######################
### Run this and adapt WRANGLE section as needed until there is no NAs ###
# Calculate percentage of missing data per column
missing_indicators <- ineq %>%
  summarise(across(everything(), ~ mean(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "Indicator", values_to = "Missing indicators") %>%
  arrange(desc("Missing indicators"))
print(missing_indicators, n = 15)
# See missings by country
missing_countries <- ineq %>%
  filter(if_any(everything(), is.na)) %>%
  count(country_name, name = "Missing years") %>%
  arrange(desc("Missing years"))
head(missing_countries, 15)
#See missings by year
missing_years <- ineq %>%
  filter(if_any(everything(), is.na)) %>%
  arrange(country_name, year)
head(missing_years, 15)
#Clean memory after check
rm(missing_indicators, missing_countries, missing_years)

#####################
### LOGICAL CHECK ###
#####################
### All ineq values should fall between 0 and 1
bounds <- ineq %>%
  filter(if_any(wealth_gini:postinc_rest9, ~ . < 0 | . > 1)) %>%
  select(country_name, year, wealth_gini:postinc_rest9)
print(bounds)
rm(bounds)
#*** South Africa has W gini over 1 because people below 50% have more debt than assets