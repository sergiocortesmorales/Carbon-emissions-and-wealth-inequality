#Install and load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here, dplyr, tidyr)

                ##################
                ### INEQUALITY ###
                ##################
ineq <- readRDS(here("data", "ineq.rds"))
######################
### MISSINGS CHECK ###
######################
### Run this and adapt "2-clean" section as needed until there is no NAs ###
# Calculate percentage of missing data per column
missing_indicators_i <- ineq %>%
  summarise(across(everything(), ~ mean(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "Indicator", values_to = "Missing indicators") %>%
  arrange(desc(`Missing indicators`))
print(missing_indicators_i, n = 15)
# See missings by country
missing_countries_i <- ineq %>%
  filter(if_any(everything(), is.na)) %>%
  count(country, name = "Missing years") %>%
  arrange(desc(`Missing years`))
head(missing_countries_i, 15)
#See missings by year
missing_years_i <- ineq %>%
  filter(if_any(everything(), is.na)) %>%
  arrange(country, year)
head(missing_years_i, 15)
#Clean memory after check
rm(missing_indicators_i, missing_countries_i, missing_years_i)

#####################
### LOGICAL CHECK ###
#####################
### All ineq values should fall between 0 and 1
bounds <- ineq %>%
  filter(if_any(wealth_gini:postinc_rest9, ~ . < 0 | . > 1)) %>%
  select(country, year, wealth_gini:postinc_rest9)
print(bounds)
rm(bounds)
#*** South Africa has W gini over 1 because people below 50% have more debt than assets

                ##################
                ### EMISSIONS ###
                #################
emis <- readRDS(here("data", "emis.rds"))
########################
### MISSINGS CHECK  ###
#######################
missing_indicators_e <- emis %>%
  summarise(across(everything(), ~ mean(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "Indicator", values_to = "Missing indicators") %>%
  arrange(desc(`Missing indicators`))
print(missing_indicators_e, n = 10)

# See missings by country
missing_countries_e <- emis %>%
  filter(if_any(everything(), is.na)) %>%
  count(country, name = "Missing years") %>%
  arrange(desc(`Missing years`))
head(missing_countries_e, 15)

# See missings by year
missing_years_e <- emis %>%
  filter(if_any(everything(), is.na)) %>%
  arrange(country, year)
head(missing_years_e, 15)
###PREVIOUS CHECKS ARE JUST INFORMATIONAL
###See rows with missing in the five indicators. ADJUSTE 2-clean UNTIL NO FINDINGS HERE
###REMOVE COUNTRIES MISSING THE FOUR INDICATORS
emis_empty <- emis %>%
  group_by(country) %>%
  filter(all(is.na(co2_per_capita) & 
               is.na(co2_including_luc_per_capita) & 
               is.na(ghg_per_capita) & 
               is.na(ghg_excluding_lucf_per_capita) & 
               is.na(consumption_co2_per_capita))) %>%
  distinct(country)
print(emis_empty)

rm(missing_indicators_e, missing_countries_e, missing_years_e, emis_empty)

#######################
### LOGICAL CHECK  ###
######################
### All emissions per capita should be non-negative
### EXCEPT CO2 including land use because carbon is sequestered by net reforestation
neg_emis <- emis %>%
  filter(if_any(co2_per_capita:consumption_co2_per_capita, ~ . < 0)) %>%
  select(country, year, co2_per_capita:consumption_co2_per_capita)
print(neg_emis)

rm(neg_emis)

### Check for extreme outliers (e.g. > 100 tonnes per capita)
extreme_emis <- emis %>%
  filter(if_any(co2_per_capita:consumption_co2_per_capita, ~ . > 100)) %>%
  select(country, year, co2_per_capita:consumption_co2_per_capita)
print(extreme_emis)

rm(extreme_emis)

                #############################
                ### WORLD BANK INDICATORS ###
                #############################
# Check for high rate of missing by each indicator
ctrl %>%
  summarise(across(gdp_pc:voice_rnk, ~ mean(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "indicator", values_to = "missing_pct") %>%
  filter(missing_pct >= 0.2) %>%
  arrange(desc(missing_pct)) %>%
  print(n = 25)

