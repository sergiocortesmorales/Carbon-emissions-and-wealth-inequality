if (!require("pacman")) install.packages("pacman")
pacman::p_load(here, dplyr, fixest, pcse, prais, haven, broom, modelsummary)

panel <- readRDS(here("data", "panel.rds"))
panel <- panel %>%
  mutate(
    log_wid_co2_pc      = log10(wid_co2_consumption),
    log_co2_pc          = log10(consumption_co2_per_capita),
    log_wealth_top10    = log10(wealth_top10),
    log_gdp_pc          = log10(gdp_pc),
    log_gini            = log10(postinc_gini)
  )

#############################
### KNIGHT ET AL. REPLICA ###
#############################
panel <- readRDS(here("data", "panel.rds"))
panel <- panel %>%
  mutate(
    log_wid_co2_pc      = log10(wid_co2_consumption),
    log_co2_pc          = log10(consumption_co2_per_capita),
    log_wealth_top10    = log10(wealth_top10),
    log_gdp_pc          = log10(gdp_pc),
    log_gini            = log10(postinc_gini)
  )

knight_countries <- c("AUS", "AUT", "BEL", "CAN", "CZE", "DNK", "FIN", 
                      "FRA", "DEU", "GRC", "IRL", "ISR", "ITA", "JPN", 
                      "NLD", "NZL", "NOR", "POL", "PRT", "SGP", "KOR", 
                      "ESP", "SWE", "CHE", "GBR", "USA")
#Load credit suisse

cs_wealth <- read.csv(here("rawdata", "creditsuisse2010.csv"))
cs_wealth <- cs_wealth %>%                    
  pivot_longer(
    cols = -country, 
    names_to = "year", 
    values_to = "wealth_top10_cs"
  ) %>%
  mutate(
    year = as.numeric(gsub("X", "", year)) # Strips the 'X' before converting
  )

#Build balanced panel: 26 countries, 2000-2010
knight <- panel %>%
  filter(iso3c %in% knight_countries, year >= 2000, year <= 2010) %>%
  # Keep only country-years with all required variables
  filter(!is.na(wid_co2_consumption) & wid_co2_consumption > 0,
         !is.na(wealth_top10) & wealth_top10 > 0,
         !is.na(gdp_pc) & gdp_pc > 0,
         !is.na(postinc_gini) & postinc_gini > 0) %>%
  # Merge Credit Suisse data by country and year
  left_join(cs_wealth, by = c("country", "year")) %>%
  filter(!is.na(wealth_top10_cs) & wealth_top10_cs > 0)

#Check balance
cat("Countries:", n_distinct(knight$iso3c), "\n")
cat("Years:", n_distinct(knight$year), "\n")
cat("Observations:", nrow(knight), "\n")
table(knight$iso3c)
knight <- knight %>%
  mutate(
    log_wid_co2_pc      = log10(wid_co2_consumption),
    log_co2_pc          = log10(consumption_co2_per_capita),
    log_wealth_top10_cs = log10(wealth_top10_cs),
    log_wealth_top10    = log10(wealth_top10),
    log_gdp_pc          = log10(gdp_pc),
    log_gini            = log10(postinc_gini)
  )

#write_dta(knight, here("data", "knight.dta")) #FOR STATA REPLICATION

# Step 1 (prais-winsten coefficients)
k_wid_pw <- prais_winsten(log_wid_co2_pc ~ log_wealth_top10_cs + log_gdp_pc + log_gini + 
                            factor(iso3c) + factor(year),
                          data = knight,
                          index = c("iso3c", "year"),
                          panelwise = FALSE)
summary(k_wid_pw)
# Step 2: (correct standard errors)
k_wid_lm <- lm(log_wid_co2_pc ~ log_wealth_top10_cs + log_gdp_pc + log_gini + 
                 factor(iso3c) + factor(year),
               data = knight)
coeftest(k_wid_lm, vcov = vcovPC(k_wid_lm, cluster = ~ iso3c + year, pairwise = TRUE))

### COMPARING CREDIT SUISSE WITH WID TOP 10% SHARE DATA
### Checking global data
knight %>%
  select(country, year, wealth_top10, wealth_top10_cs) %>%
  mutate(diff = wealth_top10 - wealth_top10_cs/100) %>%  # CS might be in % not fraction
  summary() ## Similar global data structure

### Checking within country trends
knight %>%
  mutate(cs_fraction = wealth_top10_cs / 100) %>%
  group_by(iso3c) %>%
  summarise(
    wid_trend = cor(year, wealth_top10),
    cs_trend = cor(year, cs_fraction)
  ) %>%
  mutate(trend_agrees = sign(wid_trend) == sign(cs_trend)) %>%
  print(n = 26)
### Half of the countries change trend.
### This seems to explain most of the difference between the Knight study and replicating with WID data

###########################################
###########################################