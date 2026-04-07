if (!require("pacman")) install.packages("pacman")
pacman::p_load(here, dplyr, fixest, pcse, prais, haven, broom, modelsummary, plm, lmtest, sandwich, pgmm, car)
panel <- readRDS(here("data", "panel.rds"))
# Log-transform key variables

panel <- panel %>%
  mutate(
    ln_co2pc_territorial = log(co2pc_territorial),
    ln_co2pc_territorial_wid = log(co2pc_territorial_wid),
    ln_co2pc_consumption = log(co2pc_consumption),
    ln_co2pc_consumption_wid = log(co2pc_consumption_wid),
    ln_ghgpc_territorial_excluc = log(ghgpc_territorial_excluc),
    ln_ghgpc_territorial_wid = log(ghgpc_territorial_wid),
    
    ln_wealth_top10 = log(wealth_top10),
    ln_wealth_top1 = log(wealth_top1),
    ln_wealth_rest9 = log(wealth_rest9),
    ln_preinc_gini = log(preinc_gini),
    ln_postinc_gini = log(postinc_gini),
    
    ln_gdppc = log(gdppc),
    ln_gdppc_sq = ln_gdppc^2,
    ln_urban = log(urban),
    ln_age_dep = log(age_dep),
  )
##########################
### PRAIS-WINSTEN 2WFE ###
##########################
#Driscoll-Drakay SE would be better #*#*#*#*#??
###################################################################
### CO2 COMP WID ~ W TOP 10%
# prais-winsten coefficients
rpw_co2wid <- prais_winsten(
  ln_co2pc_consumption_wid ~ ln_wealth_top10 + ln_gdppc + ln_preinc_gini +
  factor(iso3c) + factor(year),
  data      = panel,
  index     = c("iso3c", "year"),
  panelwise = FALSE
)
# Get PCSE corrected vcov matrix
vcov_corrected <- prais:::vcovPC.prais(rpw_co2wid, pairwise = TRUE)
trpw_co2wid <- coeftest(rpw_co2wid, vcov = vcov_corrected)
trpw_co2wid <- trpw_co2wid[!grepl("factor", rownames(trpw_co2wid)), ]
class(trpw_co2wid) <- "coeftest"
trpw_co2wid # NON-SIGNIFICANT RELATION
#DRISCOLL-KRAAY



### CO2 COMP WID ~ W TOP 1%
# prais-winsten coefficients
rpw_co2wid <- prais_winsten(
  ln_co2pc_consumption_wid ~ ln_wealth_top1 + ln_gdppc + ln_preinc_gini +
  factor(iso3c) + factor(year),
  data      = panel,
  index     = c("iso3c", "year"),
  panelwise = FALSE
)
# Get PCSE corrected vcov matrix
vcov_corrected <- prais:::vcovPC.prais(rpw_co2wid, pairwise = TRUE)
trpw_co2wid <- coeftest(rpw_co2wid, vcov = vcov_corrected)
trpw_co2wid <- trpw_co2wid[!grepl("factor", rownames(trpw_co2wid)), ]
class(trpw_co2wid) <- "coeftest"
trpw_co2wid # NON-SIGNIFICANT RELATION

###################################################################
### CO2 COMP WID ~ W TOP 10% Interact
# prais-winsten coefficients
rpw_co2_ekc <- prais_winsten(
  ln_co2pc_consumption_wid ~ ln_wealth_top10*ln_gdppc + preinc_gini +
  factor(iso3c) + factor(year),
  data      = panel,
  index     = c("iso3c", "year"),
  panelwise = FALSE
)
# Get PCSE corrected vcov matrix
vcov_corrected <- prais:::vcovPC.prais(rpw_co2_ekc, pairwise = TRUE)
trpw_co2_ekc <- coeftest(rpw_co2_ekc, vcov = vcov_corrected)
trpw_co2_ekc <- trpw_co2_ekc[!grepl("factor", rownames(trpw_co2_ekc)), ]
class(trpw_co2_ekc) <- "coeftest"
trpw_co2_ekc # SIGNIFICANT RELATION

### CO2 COMP WID ~ WTOP1%*GDPPC
# prais-winsten coefficients
rpw_co2_ekc <- prais_winsten(
  ln_co2pc_consumption_wid ~ ln_wealth_top1*ln_gdppc +
  ln_preinc_gini +
  factor(iso3c) + factor(year),
  data      = panel,
  index     = c("iso3c", "year"),
  panelwise = FALSE
)
# Get PCSE corrected vcov matrix
vcov_corrected <- prais:::vcovPC.prais(rpw_co2_ekc, pairwise = TRUE)
trpw_co2_ekc <- coeftest(rpw_co2_ekc, vcov = vcov_corrected)
trpw_co2_ekc <- trpw_co2_ekc[!grepl("factor", rownames(trpw_co2_ekc)), ]
class(trpw_co2_ekc) <- "coeftest"
trpw_co2_ekc

#################### Adding controls #################
### CO2 COMP WID ~ WTOP10%*GDPPC
# prais-winsten coefficients
rpw_co2_10 <- prais_winsten(
  ln_co2pc_consumption_wid ~ ln_wealth_top10*ln_gdppc +
  ln_preinc_gini + + pop_growth + ln_urban + ln_age_dep +
  factor(iso3c) + factor(year),
  data      = panel,
  index     = c("iso3c", "year"),
  panelwise = FALSE
)
# Get PCSE corrected vcov matrix
vcov_corrected <- prais:::vcovPC.prais(rpw_co2_10, pairwise = TRUE)
trpw_co2_10 <- coeftest(rpw_co2_10, vcov = vcov_corrected)
trpw_co2_10 <- trpw_co2_10[!grepl("factor", rownames(trpw_co2_10)), ]
class(trpw_co2_10) <- "coeftest"
trpw_co2_10
#DRISCOLL-KRAAY SE
fe_co2_dk_10 <- feols(
  ln_co2pc_consumption_wid ~
    ln_wealth_top10*ln_gdppc +
    ln_preinc_gini +
    pop_growth +
    ln_urban +
    ln_age_dep | iso3c + year,
  data     = panel,
  panel.id = ~iso3c + year,
  vcov     = "DK"
)
summary(fe_co2_dk_10)


### CO2 COMP WID ~ WTOP1%*GDPPC - rest 9% controlled###
# prais-winsten coefficients
rpw_co2_1 <- prais_winsten(
  ln_co2pc_consumption_wid ~ ln_wealth_top1*ln_gdppc +
    ln_wealth_rest9 + ln_preinc_gini + pop_growth + ln_urban + ln_age_dep +
    factor(iso3c) + factor(year),
  data      = panel,
  index     = c("iso3c", "year"),
  panelwise = FALSE
)
# Get PCSE corrected vcov matrix
vcov_corrected <- prais:::vcovPC.prais(rpw_co2_1, pairwise = TRUE)
trpw_co2_1 <- coeftest(rpw_co2_1, vcov = vcov_corrected)
trpw_co2_1 <- trpw_co2_1[!grepl("factor", rownames(trpw_co2_10)), ]
class(trpw_co2_1) <- "coeftest"
trpw_co2_1
#DRISCOLL-KRAAY SE
# Newey-West 1987 — most common default
#lag = "NW1987"    # gives floor(4 * (T/100)^(2/9))
#
# Newey-West 1994 — slightly more generous
#lag = "NW1994"    # gives floor(4 * (T/100)^(4/25))
fe_co2_dk_1 <- feols(
  ln_co2pc_consumption_wid ~
    ln_wealth_top1*ln_gdppc +
    ln_wealth_rest9 +
    ln_preinc_gini +
    pop_growth +
    ln_urban +
    ln_age_dep | iso3c + year,
  data     = panel,
  panel.id = ~iso3c + year,
  vcov     = "DK"
)
summary(fe_co2_dk_1)

# Helper function
extract_results <- function(obj, label) {
  if (inherits(obj, "coeftest")) {
    df <- as.data.frame(unclass(obj))
    names(df) <- c("Estimate", "SE", "t", "p")
    library(kableExtra)
    
    # Helper function
    extract_results <- function(obj, label) {
      if (inherits(obj, "coeftest")) {
        df <- as.data.frame(unclass(obj))
        names(df) <- c("Estimate", "SE", "t", "p")
      } else {
        s <- summary(obj)
        df <- data.frame(Estimate = coef(s), SE = se(s), t = tstat(s), p = pvalue(s))
      }
      df$Variable <- rownames(df)
      df$Model <- label
      rownames(df) <- NULL
      df
    }
    
    results <- bind_rows(
      extract_results(trpw_co2_10, "PW+PCSE (Top10%)"),
      extract_results(fe_co2_dk_10, "DK (Top10%)"),
      extract_results(trpw_co2_1,  "PW+PCSE (Top1%)"),
      extract_results(fe_co2_dk_1, "DK (Top1%)")
    ) %>%
      filter(!grepl("factor|Intercept", Variable))
    
    # Clean variable names
    name_map <- c(
      "ln_wealth_top10"            = "Wealth Top 10% (ln)",
      "ln_gdppc"                   = "GDP per capita (ln)",
      "ln_wealth_top10:ln_gdppc"   = "Top 10% × GDP pc",
      "ln_preinc_gini"             = "Pre-tax Gini (ln)",
      "pop_growth"                 = "Population Growth",
      "ln_urban"                   = "Urbanisation (ln)",
      "ln_age_dep"                 = "Age Dependency (ln)",
      "ln_wealth_top1"             = "Wealth Top 1% (ln)",
      "ln_wealth_rest9"            = "Wealth Next 9% (ln)",
      "ln_wealth_top1:ln_gdppc"    = "Top 1% × GDP pc"
    )
    results$Variable <- ifelse(results$Variable %in% names(name_map),
                               name_map[results$Variable], results$Variable)
    
    # Format cells: coeff with stars, SE in parentheses below
    results <- results %>%
      mutate(
        stars = case_when(p < 0.01 ~ "***", p < 0.05 ~ "**", p < 0.1 ~ "*", TRUE ~ ""),
        cell  = paste0(sprintf("%.4f", Estimate), stars, "<br><small>(", sprintf("%.4f", SE), ")</small>")
      ) %>%
      select(Variable, Model, cell) %>%
      pivot_wider(names_from = Model, values_from = cell, values_fill = "—")
    
    # Define desired row order
    row_order <- c(
      "Wealth Top 10% (ln)", "Wealth Top 1% (ln)", "Wealth Next 9% (ln)",
      "GDP per capita (ln)", "Top 10% × GDP pc", "Top 1% × GDP pc",
      "Pre-tax Gini (ln)", "Population Growth", "Urbanisation (ln)", "Age Dependency (ln)"
    )
    results <- results %>%
      mutate(order = match(Variable, row_order)) %>%
      arrange(order) %>%
      select(-order)
    
    # Build HTML table
    results %>%
      kbl(
        format    = "html",
        escape    = FALSE,
        caption   = "Table X. Wealth Inequality and CO₂ Emissions per Capita (Consumption-Based)",
        col.names = c("", "PW+PCSE", "FE+DK", "PW+PCSE", "FE+DK"),
        align     = c("l", "c", "c", "c", "c")
      ) %>%
      kable_classic(full_width = FALSE, html_font = "Cambria") %>%
      add_header_above(c(" " = 1, "Top 10% Specification" = 2, "Top 1% Specification" = 2)) %>%
      footnote(
        general = c(
          "Standard errors in parentheses. *** p<0.01, ** p<0.05, * p<0.1",
          "PW+PCSE = Prais-Winsten 2WFE with Panel-Corrected SE (Beck & Katz 1995; Knight et al. 2017)",
          "FE+DK = OLS 2WFE with Driscoll-Kraay SE, using lag=2",
          "Dependent variable: ln(CO₂ per capita, consumption-based). All continuous variables in logs except Population Growth."
        ),
        general_title = "Notes: "
      ) %>%
      save_kable("regression_table.html")
    
    browseURL("regression_table.html")save_kable("regression_table.html")

browseURL("regression_table.html")


#################### Adding TRADE OPENESS AND INSTITUTIONAL #################
#################### TRADE FROM CEPII OR PWT ################################
#################### INSTI FROM V-DEM,QoG(ht_ipolity2),Freedom house,ICRG ###
#################### Why renewable energy?
### CO2 COMP WID ~ WTOP10%*GDPPC
# prais-winsten coefficients
rpw_co2_ekc <- prais_winsten(
  ln_co2pc_consumption_wid ~ ln_wealth_top10*ln_gdppc +
    ln_preinc_gini + pop_growth + ln_urban + ln_age_dep +
    factor(iso3c) + factor(year),
  data      = panel,
  index     = c("iso3c", "year"),
  panelwise = FALSE
)
# Get PCSE corrected vcov matrix
vcov_corrected <- prais:::vcovPC.prais(rpw_co2_ekc, pairwise = TRUE)
trpw_co2_ekc <- coeftest(rpw_co2_ekc, vcov = vcov_corrected)
trpw_co2_ekc <- trpw_co2_ekc[!grepl("factor", rownames(trpw_co2_ekc)), ]
class(trpw_co2_ekc) <- "coeftest"
trpw_co2_ekc

### CO2 COMP WID ~ WTOP1%*GDPPC - rest 9% controlled
# prais-winsten coefficients
rpw_co2_ekc <- prais_winsten(
  ln_co2pc_consumption_wid ~ ln_wealth_top1*ln_gdppc + ln_wealth_rest9 +
    ln_preinc_gini + pop_growth + ln_urban + ln_age_dep +
    factor(iso3c) + factor(year),
  data      = panel,
  index     = c("iso3c", "year"),
  panelwise = FALSE
)
# Get PCSE corrected vcov matrix
vcov_corrected <- prais:::vcovPC.prais(rpw_co2_ekc, pairwise = TRUE)
trpw_co2_ekc <- coeftest(rpw_co2_ekc, vcov = vcov_corrected)
trpw_co2_ekc <- trpw_co2_ekc[!grepl("factor", rownames(trpw_co2_ekc)), ]
class(trpw_co2_ekc) <- "coeftest"
trpw_co2_ekc

#########################################################
################## Drop tests ###########################
#########################################################

# ============================================================
# LEAVE-ONE-OUT ROBUSTNESS — PRAIS-WINSTEN
# ============================================================

countries <- unique(panel$iso3c)
n_countries <- length(countries)

loo_results <- data.frame(
  dropped = character(),
  coef_w1 = numeric(),
  p_w1    = numeric(),
  stringsAsFactors = FALSE
)

for (i in seq_along(countries)) {
  c <- countries[i]
  cat(sprintf("Running %d/%d: %s\n", i, n_countries, c))
  
  tryCatch({
    rpw_loo <- prais_winsten(
      ln_co2pc_consumption_wid ~ ln_wealth_top1 * ln_gdppc +
        ln_wealth_rest9 + ln_preinc_gini + pop_growth +
        ln_urban + ln_age_dep +
        factor(iso3c) + factor(year),
      data      = panel %>% filter(iso3c != c),
      index     = c("iso3c", "year"),
      panelwise = FALSE
    )
    
    s <- summary(rpw_loo)$coefficients
    
    loo_results <- rbind(loo_results, data.frame(
      dropped = c,
      coef_w1 = s["ln_wealth_top1", "Estimate"],
      p_w1    = s["ln_wealth_top1", "Pr(>|t|)"],
      stringsAsFactors = FALSE
    ))
    
  }, error = function(e) {
    cat(sprintf("  Failed for %s: %s\n", c, e$message))
    loo_results <<- rbind(loo_results, data.frame(
      dropped = c,
      coef_w1 = NA,
      p_w1    = NA,
      stringsAsFactors = FALSE
    ))
  })
}

# ============================================================
# SUMMARY OF RESULTS
# ============================================================

cat("\n=== Leave-One-Out Summary ===\n")
cat(sprintf("Total countries tested : %d\n", nrow(loo_results)))
cat(sprintf("Significant (p<0.05)   : %d (%.1f%%)\n",
            sum(loo_results$p_w1 < 0.05, na.rm = TRUE),
            mean(loo_results$p_w1 < 0.05, na.rm = TRUE) * 100))
cat(sprintf("Significant (p<0.10)   : %d (%.1f%%)\n",
            sum(loo_results$p_w1 < 0.10, na.rm = TRUE),
            mean(loo_results$p_w1 < 0.10, na.rm = TRUE) * 100))
cat(sprintf("Mean coefficient       : %.4f\n",
            mean(loo_results$coef_w1, na.rm = TRUE)))
cat(sprintf("Coefficient range      : %.4f to %.4f\n",
            min(loo_results$coef_w1, na.rm = TRUE),
            max(loo_results$coef_w1, na.rm = TRUE)))

# Countries whose exclusion kills significance
cat("\n--- Countries that kill significance when dropped ---\n")
loo_results %>%
  filter(p_w1 >= 0.05) %>%
  arrange(desc(p_w1)) %>%
  print()

# Countries whose exclusion strengthens significance
cat("\n--- Top 10 countries that strengthen result when dropped ---\n")
loo_results %>%
  filter(!is.na(p_w1)) %>%
  arrange(p_w1) %>%
  head(10) %>%
  print()

# Coefficient stability plot
cat("\n--- Coefficient distribution ---\n")
hist(loo_results$coef_w1,
     main = "Distribution of ln_wealth_top1 coefficient\nacross leave-one-out samples",
     xlab = "Coefficient estimate",
     col  = "steelblue",
     border = "white")
abline(v = 0, col = "red", lty = 2, lwd = 2)


##########################################################
####################### Feols ############################
##########################################################

### CO2 COMP WID ~ WTOP1%*GDPPC - rest 9% controlled

rfe_co2_ekc <- feols(
  ln_co2pc_consumption_wid ~ ln_wealth_top1*ln_gdppc + ln_wealth_rest9 +
  ln_preinc_gini + pop_growth + ln_urban + ln_age_dep | iso3c + year,
  data=panel, cluster = ~iso3c
)
summary(rfe_co2_ekc)


# ============================================================
# INCOME GROUP DISAGGREGATION
# ============================================================


income_groups <- unique(panel$income)
for (grp in income_groups) {
  cat(sprintf("\n=== Income Group: %s ===\n", grp))
  
  panel_grp <- panel %>% filter(income == grp)
  
  cat(sprintf("Countries: %d, Obs: %d\n", 
              n_distinct(panel_grp$iso3c), nrow(panel_grp)))
  
  tryCatch({
    rpw_grp <- prais_winsten(
      ln_co2pc_consumption_wid ~ ln_wealth_top1 * ln_gdppc + 
        ln_wealth_rest9 + ln_preinc_gini + pop_growth + 
        ln_urban + ln_age_dep +
        factor(iso3c) + factor(year),
      data      = panel_grp,
      index     = c("iso3c", "year"),
      panelwise = FALSE
    )
    
    vcov_grp <- prais:::vcovPC.prais(rpw_grp, pairwise = TRUE)
    res_grp  <- coeftest(rpw_grp, vcov = vcov_grp)
    res_grp  <- res_grp[!grepl("factor", rownames(res_grp)), ]
    class(res_grp) <- "coeftest"
    print(res_grp)
    
  }, error = function(e) {
    cat(sprintf("Could not estimate for %s: %s\n", grp, e$message))
  })
}
### ONLY SIGNIFICANT IN UPPER MIDDLE INCOME IS SIGNIFICANT
###(EMERGENT, BAD INSTITUIONS)
# ============================================================
# SYSTEM GMM — equivalent specification
# ============================================================

pacman::p_load(pgmm)

# Create interaction term manually for pgmm
# pgmm does not support formula interactions directly
panel <- panel %>%
  mutate(ln_wealth_top1Igdppc = ln_wealth_top1 * ln_gdppc)

# pgmm requires pdata.frame
pdata_gmm <- pdata.frame(panel, index = c("iso3c", "year"))

# System GMM
# Dependent variable lagged to capture persistence
# ln_wealth_top1 treated as endogenous — instrumented with lags
# Controls treated as predetermined

sgmm <- pgmm(
  ln_co2pc_consumption_wid ~ lag(ln_co2pc_consumption_wid, 1) +
    ln_wealth_top1 + ln_gdppc + ln_wealth_top1Igdppc +
    ln_wealth_rest9 + ln_preinc_gini + pop_growth +
    ln_urban + ln_age_dep |
    lag(ln_wealth_top1, 2:4),
  data        = pdata_gmm,
  effect      = "twoways",
  model       = "twosteps",
  transformation = "d" ### ADRESS NICKEL BIAS WHEN USING "ld"
)

cat("\n=== System GMM Results ===\n")
sgmm_summary <- summary(sgmm, robust = TRUE)
print(sgmm_summary)

# Arellano-Bond tests — validate instrument structure
# AR(1) should be significant, AR(2) should NOT be significant
sgmm_summary <- summary(sgmm, robust = TRUE)

# Correct extraction for pgmm summary object
cat("AR(1) p-value:", sgmm_summary$m1$p.value, "\n")
cat("AR(2) p-value:", sgmm_summary$m2$p.value, "\n")
cat("Sargan p-value:", sgmm_summary$sargan$p.value, "\n")

### RESULTS SURVIVE ###******************
# Is this exactly Blundell-Bond two-step?

#diagnostic: estimate panel-specific ρ̂_i values and examine their distribution; if the interquartile range exceeds 0.3, the common-ρ assumption is clearly untenable
#####CHECK
########## principal components analysis
########## Group fixed effects
########## Lewbel IV
########## Alternative time samples
########## 2SLS with regional wealth inequality
########## Placebo test
########## Alternative wealth measures, alternative income control
########## Alternative emissions measures
########## Estimate turning point (apeti table 7 column 5), check if same that EKC
########## CHANNELS: Democracy, climate change policy, CO2pc as dependent variables
########## Quantile regression OR MMQR
########## Descriptive statistics
