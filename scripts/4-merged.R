#Install and load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here, dplyr, tidyr, countrycode)

# Read cleaned data

ineq <- readRDS(here("data", "ineq.rds"))
emis <- readRDS(here("data", "emis.rds"))
emis_wid <- readRDS(here("data", "emis_wid.rds"))
ctrl <- readRDS(here("data", "ctrl.rds"))
claw <- readRDS(here("data", "claw.rds"))

# Merge ineq + OWID emissions
merge1 <- inner_join(ineq, emis, by = c("iso3c", "year"))

# Add WID emissions
merge1 <- left_join(merge1, 
                    emis_wid %>% select(-country, -iso2c),
                    by = c("iso3c", "year"))

# Add climate laws
merge1 <- left_join(merge1,
                    claw %>% select(-country, -iso2c),
                    by = c("iso3c", "year"))

# Add WDI controls
panel <- inner_join(merge1, ctrl, by = c("iso3c", "year"))

# Check dimensions
cat("Countries:", n_distinct(panel$iso3c), "\n")
cat("Years:", n_distinct(panel$year), "\n")
cat("Observations:", nrow(panel), "\n")
# Add binary for OECD countries
oecd <- c("AUS", "AUT", "BEL", "CAN", "CHL", "COL", "CRI", "CZE", 
          "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "ISL", 
          "IRL", "ISR", "ITA", "JPN", "KOR", "LVA", "LTU", "LUX", 
          "MEX", "NLD", "NZL", "NOR", "POL", "PRT", "SVK", "SVN", 
          "ESP", "SWE", "CHE", "TUR", "GBR", "USA")

panel <- panel %>%
  mutate(oecd = ifelse(iso3c %in% oecd, 1, 0)) %>%
  select(-country.x,-country.y, -iso2c.x, -iso2c.y)

saveRDS(panel, here("data", "panel.rds"))
rm(oecd)
### Check for best range for balanced panel
balanced_core <- data.frame()
for (start in 1991:2005) {
  for (end in 2015:2023) {
    bp <- panel %>%
      filter(year >= start, year <= end) %>%
      filter(!is.na(co2pc_consumption_wid), !is.na(co2pc_territorial_wid), !is.na(ghgpc_consumption_wid),!is.na(ghgpc_territorial_wid),#!is.na(co2pc_territorial), !is.na(co2pc_consumption), !is.na(ghgpc_territorial_excluc), !is.na(ghgpc_territorial), !is.na(wealth_top1), !is.na(wealth_top10),
             !is.na(preinc_gini), !is.na(postinc_gini),
             !is.na(gdppc), !is.na(urban), !is.na(age_dep), !is.na(pop_growth),
             !is.na(claw_legi), !is.na(claw_total)) %>%
      group_by(iso3c) %>%
      filter(n() == (end - start + 1)) %>%
      ungroup()
    balanced_core <- rbind(balanced_core, data.frame(
      start = start, end = end, 
      years = end - start + 1,
      countries = n_distinct(bp$iso3c),
      obs = nrow(bp)
    ))
  }
}
balanced_core %>% arrange(desc(obs)) %>% head(20) #1991-2020
rm(end, start, balanced_core, bp)
### Build balanced panel
panel <- panel %>%
  filter(year >= 1991, year <= 2020) %>%
  filter(!is.na(co2pc_consumption_wid), !is.na(co2pc_territorial_wid), !is.na(ghgpc_consumption_wid),!is.na(ghgpc_territorial_wid),#!is.na(co2pc_territorial), !is.na(co2pc_consumption), !is.na(ghgpc_territorial_excluc), !is.na(ghgpc_territorial),
         !is.na(wealth_top1), !is.na(wealth_top10),
         !is.na(preinc_gini), !is.na(postinc_gini),
         !is.na(gdppc), !is.na(urban), !is.na(age_dep), !is.na(pop_growth),
         !is.na(claw_legi), !is.na(claw_total))  %>%
  group_by(iso3c) %>%
  filter(n() == 30) %>%
  ungroup()

cat("Countries:", n_distinct(panel$iso3c), "\n", "Years:", n_distinct(panel$year), "\n", "Observations:", nrow(panel), "\n")

saveRDS(panel, here("data", "panel.rds"))