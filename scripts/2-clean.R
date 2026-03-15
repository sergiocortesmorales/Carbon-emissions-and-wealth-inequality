#Install and load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here, dplyr, tidyr, countrycode)
                ##################
                ### INEQUALITY ###
                ##################
ineq <- readRDS(here("rawdata", "ineq_raw.rds")) %>% #Read
  rename(iso2c = country) %>%
  filter(!grepl("-", iso2c)) %>% #Drop codes with hyphen (i.e., non-countries)
  mutate(country = countrycode( #Obtain country name
                        iso2c, origin = "iso2c",
                        destination = "country.name",
                        custom_match = c("KS" = "Kosovo")
                        )) %>% 
  mutate(iso3c = countrycode( #Obtain country iso3c
                        iso2c, origin = "iso2c",
                        destination = "iso3c",
                        custom_match = c("KS" = "XKX")
  )) %>% 
filter(!is.na(country)) %>% #Just in case there are any regions or subregions remaining
#Reshape to wide (one row per country in a given year)
#Unique indicators
mutate(variable = substr(variable, 1, 6)) %>%
mutate(variable_name = paste0(variable, "_", percentile)) %>%
filter(!(grepl("^s", variable) & percentile == "p0p100")) %>%
select(country, iso2c, iso3c, year, variable_name, value) %>%
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
  country, iso2c, iso3c, year,
  wealth_gini, preinc_gini, postinc_gini,
  wealth_top10, preinc_top10, postinc_top10,
  wealth_top1, preinc_top1, postinc_top1,
  wealth_rest9, preinc_rest9, postinc_rest9,
  wealth_bot50, preinc_bot50, postinc_bot50
)
### SAVE ###
saveRDS(ineq, here("data", "ineq.rds"))
#If csv needed#write.csv(ineq, here("data", "ineqcsv.csv"))

emis_wid <- readRDS(here("rawdata", "emis_wid_raw.rds"))
emis_wid <- emis_wid %>%
  rename(iso2c = country) %>%
  filter(!grepl("-", iso2c)) %>%
  mutate(
    country = countrycode(iso2c, origin = "iso2c", destination = "country.name",
                          custom_match = c("KS" = "Kosovo")),
    iso3c = countrycode(iso2c, origin = "iso2c", destination = "iso3c",
                        custom_match = c("KS" = "XKX"))
  ) %>%
  filter(!is.na(country)) %>%
  mutate(variable = substr(variable, 1, 6)) %>%
  select(country, iso2c, iso3c, year, variable, value) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  rename(
    wid_co2_territorial = kntcar,
    wid_co2_consumption   = knfcar,
    wid_ghg_territorial = kntghg,
    wid_ghg_consumption   = knfghg
  )
saveRDS(emis_wid, here("data", "emis_wid.rds"))




                #################
                ### EMISSIONS ###
                #################
emis <- read.csv(here("rawdata","owid-co2-data.csv")) %>% #Read
#Select columns, year range, and drop regions
rename(iso3c = iso_code) %>%
select(country, iso3c, year, co2_per_capita, co2_including_luc_per_capita, ghg_excluding_lucf_per_capita, ghg_per_capita,  consumption_co2_per_capita) %>%
filter(year >= 1991 & year <= 2023) %>%
filter(iso3c != "") %>%
filter(!country %in% c("Antarctica", "Christmas Island", "Monaco", "San Marino", "Vatican"))
# DROPPING COUNTRIES WHICH NONE OF THE FIVE INDICATORS
### SAVE ###
saveRDS(emis, here("data", "emis.rds"))
#If csv needed#write.csv(emis, here("data", "emiscsv.csv"))

                #############################
                ### WORLD BANK INDICATORS ###
                #############################
ctrl <- readRDS(here("rawdata", "ctrl_raw.rds")) #Read
ctrl <- ctrl %>%
#Drop irrelevant columns
select(-status, -lastupdated, -longitude, -latitude, -lending, -capital) %>%
#Drop indicators not (yet) needed for the analysis
select(country, iso2c, iso3c, year, gdp_pc, region, income) %>%
mutate(region = case_when(
iso2c == "PR" ~ "Latin America & Caribbean",
iso2c == "SO" ~ "Sub-Saharan Africa",
TRUE ~ region
)) %>%
filter(region != "Aggregates") %>%
filter(!is.na(region))
saveRDS(ctrl, here("data", "ctrl.rds"))
