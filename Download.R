#Install and load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(wid, dplyr, tidyr, countrycode)

################
### DOWNLOAD ###
################
#Download top 1% share of the net wealth
ineq_base <- download_wid(
  indicators = c("shweal", "ghweal", "sptinc", "gptinc", "sdiinc", "gdiinc"), #Indicators, e.g. "s" = Share, "hweal" = Net personal wealth
  perc = c("p99p100", "p90p99", "p90p100", "p0p100", "p0p50"), #quantiles
  ages = 992, #Adults aged 20 and over
  pop = "j", #Equal-split adults
  areas = "all", #All available countries/regions
  years = 1991:2023
)

###############
### WRANGLE ###
###############
#Filter to keep only countries removing codes that contain a hyphen
ineq <- ineq_base %>%
  filter(!grepl("-", country)) %>% #Drop codes with hyphen (i.e., non-countries)
  mutate(country_name = countrycode(
                        country, origin = "iso2c",
                        destination = "country.name",
                        custom_match = c("KS" = "Kosovo")
                        )) %>% #Obtain country name 
filter(!is.na(country_name)) %>% #Just in case there are any regions or subregions remaining
#Reshape to wide (one row per country in a given year)
#Unique indicators
mutate(variable = substr(variable, 1, 6)) %>%
mutate(variable_name = paste0(variable, "_", percentile)) %>%
filter(!(grepl("^s", variable) & percentile == "p0p100")) %>%
select(country_name, country, year, variable_name, value) %>%
pivot_wider(names_from = variable_name, values_from = value) %>%
rename(
  #Wealth (Net personal wealth: hweal)
  wealth_gini = ghweal_p0p100,
  wealth_top1 = shweal_p99p100,
  wealth_rest9 = shweal_p90p99,
  wealth_top10 = shweal_p90p100,
  wealth_bot50 = shweal_p0p50,
  #Pre-tax income (national income: ptinc)
  preinc_gini = gptinc_p0p100,
  preinc_top1 = sptinc_p99p100,
  preinc_rest9 = sptinc_p90p99,
  preinc_top10 = sptinc_p90p100,
  preinc_bot50 = sptinc_p0p50,
  # Post-tax income (disposable income: diinc)
  postinc_gini = gdiinc_p0p100,
  postinc_top1 = sdiinc_p99p100,
  # rest 9% not retrieving from API so calculating it manually bellow
  postinc_top10 = sdiinc_p90p100,
  postinc_bot50 = sdiinc_p0p50
  ) %>%
###IF NEEDED: mutate(wealth_topbottomratio = wealth_top10/wealth_bot50) %>%
# CALCULATING MISSING VALUES
# Calculating rest 9% post tax income manually
mutate(postinc_rest9 = postinc_top10 - postinc_top1) %>%
# For rest 9% pre tax income, I calculate one missing value for in India 2023
mutate(preinc_rest9 = preinc_top10 - preinc_top1) %>%
# in the API India has missing years for wealth_rest9. So I calculate those years manually from 1% - 10%
mutate(wealth_rest9 = coalesce(wealth_rest9, wealth_top10 - wealth_top1)) %>%
#Reorder columns
select(
  country_name, country, year,
  wealth_gini, preinc_gini, postinc_gini,
  wealth_top10, preinc_top10, postinc_top10,
  wealth_top1, preinc_top1, postinc_top1,
  wealth_rest9, preinc_rest9, postinc_rest9,
  wealth_bot50, preinc_bot50, postinc_bot50
)

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

############
### SAVE ###
############
saveRDS(ineq, "ineq.rds")