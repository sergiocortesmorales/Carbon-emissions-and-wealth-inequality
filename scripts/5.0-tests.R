options(repos = c(CRAN = "https://cran.rstudio.com/"))
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  here, dplyr, tidyr,
  fixest,
  plm,
  lmtest,
  sandwich,
  car,
  moments,
  broom,
  modelsummary,
  pgmm)

panel <- readRDS(here("data", "panel.rds"))
cat("Countries:", n_distinct(panel$iso3c), "\n")
cat("Years:",     n_distinct(panel$year),  "\n")
cat("Obs:",       nrow(panel),             "\n")
cat("Columns:",   paste(names(panel), collapse = ", "), "\n")

panel <- panel %>%
  mutate(
    ln_co2pc_consumption           = log(co2pc_consumption),
    ln_co2pc_consumption_wid       = log(co2pc_consumption_wid),
    ln_wealth_top1      = log(wealth_top1),
    ln_wealth_top10     = log(wealth_top10),
    ln_gdp_pc           = log(gdp_pc),
    ln_gdp_pc_sq        = log(gdp_pc)^2,
    ln_preinc_gini      = log(preinc_gini),
    ln_postinc_gini     = log(postinc_gini)
  ) %>%
  arrange(iso3c, year) %>%
  group_by(iso3c) %>%
  mutate(
    ln_wealth_top1_lag1 = lag(ln_wealth_top1, 1),
    ln_wealth_top1_lag2 = lag(ln_wealth_top1, 2),
    ln_co2_pc_lag1      = lag(ln_co2_pc, 1)
  ) %>%
  ungroup()

pdata <- pdata.frame(panel, index = c("iso3c", "year"))
print(pdim(pdata))

diag_vars <- c("ln_co2_pc", "ln_wid_co2_pc",
               "ln_wealth_top1", "ln_wealth_top10",
               "ln_gdp_pc", "preinc_gini", "postinc_gini")

# 1.1 Descriptive statistics 

desc_stats <- panel %>%
  select(all_of(diag_vars)) %>%
  summarise(across(everything(), list(
    N        = ~sum(!is.na(.)),
    Mean     = ~round(mean(.,     na.rm = TRUE), 3),
    SD       = ~round(sd(.,       na.rm = TRUE), 3),
    Min      = ~round(min(.,      na.rm = TRUE), 3),
    Max      = ~round(max(.,      na.rm = TRUE), 3),
    Skewness = ~round(skewness(., na.rm = TRUE), 3),
    Kurtosis = ~round(kurtosis(., na.rm = TRUE), 3)
  ))) %>%
  pivot_longer(
    everything(),
    names_to  = c("Variable", "Stat"),
    names_sep = "_(?=[^_]+$)"
  ) %>%
  pivot_wider(names_from = Stat, values_from = value)
print(desc_stats)

# 1.2 Jarque-Bera Normality Tests
for (v in diag_vars) {
  vals <- as.numeric(na.omit(panel[[v]]))
  if (length(vals) >= 8) {
    jb <- jarque.test(vals)
    cat(sprintf("%-22s: JB = %8.2f, p = %.4f %s\n",
                v, jb$statistic, jb$p.value,
                ifelse(jb$p.value < 0.05, "[Non-normal]", "[Normal]")))
  } else {
    cat(sprintf("%-22s: insufficient observations\n", v))
  }
}

# Correlation matrix
cor_mat <- panel %>%
  select(all_of(diag_vars)) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(3)
print(cor_mat)

# Flag correlations above 0.8
high_pairs <- which(abs(cor_mat) > 0.8 & upper.tri(cor_mat), arr.ind = TRUE)
if (nrow(high_pairs) > 0) {
  cat("\nWARNING: Correlations above 0.8 detected:\n")
  for (i in seq_len(nrow(high_pairs))) {
    r  <- high_pairs[i, 1]
    cc <- high_pairs[i, 2]
    cat(sprintf("  %s -- %s: r = %.3f\n",
                rownames(cor_mat)[r], colnames(cor_mat)[cc],
                cor_mat[r, cc]))
  }
} else {
  cat("\nNo correlations above 0.8 detected.\n")
}

# 1.3 VIF — pooled OLS (for diagnostic only, not for inference)

vif_ols <- lm(
  ln_co2_pc ~ ln_wealth_top1 + ln_gdp_pc + preinc_gini + postinc_gini,
  data = panel, na.action = na.omit
)

vif_vals <- vif(vif_ols)
for (v in names(vif_vals)) {
  flag <- ifelse(vif_vals[v] > 10, " *** HIGH ***",
                 ifelse(vif_vals[v] >  5, " * moderate *", ""))
  cat(sprintf("  %-25s: %.3f%s\n", v, vif_vals[v], flag))
}


# ============================================================
# STAGE 2 — PANEL STRUCTURE TESTS
# ============================================================

# 2.1 Cross-Sectional Dependence — Pesaran CD Test

# Run on main model residuals using plm two-way FE
fe_for_cd <- plm(
  ln_co2_pc ~ ln_wealth_top1 + ln_gdp_pc + preinc_gini + postinc_gini,
  data   = pdata,
  model  = "within",
  effect = "twoways"
)

cd_test     <- pcdtest(fe_for_cd, test = "cd")
csd_present <- cd_test$p.value < 0.05

cat(sprintf("CD statistic : %.4f\n", cd_test$statistic))
cat(sprintf("p-value      : %.4f\n", cd_test$p.value))
cat(sprintf("Decision     : CSD %s\n",
            ifelse(csd_present,
                   "CONFIRMED — use 2nd gen unit root tests",
                   "NOT detected — 1st gen tests acceptable")))

# 2.2 Slope Heterogeneity — Poolability Test

pool_test <- pooltest(
  ln_co2_pc ~ ln_wealth_top1 + ln_gdp_pc + preinc_gini,
  data  = pdata,
  model = "within"
)

print(pool_test)

slope_het <- pool_test$p.value < 0.05
cat(sprintf("\nDecision: Slopes %s\n",
            ifelse(slope_het,
                   "HETEROGENEOUS — FE estimates are country-average effects",
                   "HOMOGENEOUS — pooled estimation consistent")))
# Heterogeneous slopes - Average within-country inequality

# ============================================================
# STAGE 3 — STATIONARITY TESTS
# ============================================================

# Unit root
cat(sprintf("%-22s | %-12s | %-15s | %s\n",
            "Variable", "Level p-val", "1st Diff p-val", "Order"))
cat(strrep("-", 70), "\n")

ur_results <- data.frame(
  Variable = character(),
  Level_p  = numeric(),
  Diff_p   = numeric(),
  Order    = character(),
  stringsAsFactors = FALSE
)

for (v in ur_vars) {
  tryCatch({
    # Level test on pdata
    form_lev <- as.formula(paste(v, "~ 1"))
    ips_lev  <- purtest(form_lev, data = pdata,
                        test = "ips", lags = 1, exo = "intercept")
    p_lev    <- ips_lev$statistic$p.value
    
    # First difference
    diff_vals <- panel %>%
      arrange(iso3c, year) %>%
      group_by(iso3c) %>%
      mutate(d_var = c(NA, diff(.data[[v]]))) %>%
      ungroup() %>%
      select(iso3c, year, d_var) %>%
      filter(!is.na(d_var))
    
    pdata_diff <- pdata.frame(diff_vals, index = c("iso3c", "year"))
    
    ips_dif  <- purtest(d_var ~ 1, data = pdata_diff,
                        test = "ips", lags = 1, exo = "intercept")
    p_dif    <- ips_dif$statistic$p.value
    
    order_v  <- ifelse(p_lev > 0.05 & p_dif < 0.05, "I(1)",
                       ifelse(p_lev < 0.05, "I(0)", "Unclear"))
    
    cat(sprintf("%-22s | %11.4f   | %14.4f   | %s\n",
                v, p_lev, p_dif, order_v))
    
    ur_results <- rbind(ur_results, data.frame(
      Variable = v, Level_p = p_lev,
      Diff_p = p_dif, Order = order_v,
      stringsAsFactors = FALSE
    ))
    
  }, error = function(e) {
    cat(sprintf("%-22s | Could not compute: %s\n", v, e$message))
  })
}

all_I1 <- all(ur_results$Order == "I(1)", na.rm = TRUE)
all_I0 <- all(ur_results$Order == "I(0)", na.rm = TRUE)
mixed  <- !all_I1 & !all_I0

cat("\n--- Unit Root Decision ---\n")
if (all_I1) {
  cat("All I(1) — proceed to cointegration testing\n")
} else if (all_I0) {
  cat("All I(0) — estimate in levels, skip Stage 4\n")
} else {
  cat("Mixed integration orders detected\n")
  cat("Options:\n")
  cat("  1. ARDL bounds approach — handles mixed I(0)/I(1)\n")
  cat("  2. Proceed with FE in levels — common in practice\n")
  cat("  3. Validate with CIPS in Stata before deciding\n")
  print(ur_results[, c("Variable", "Order")])
}
# MOR, OMN, and SAU have no variance for variable preinc_gini

# ============================================================
# STAGE 4 — COINTEGRATION (only if all I(1))
# ============================================================

if (!all_I1) {
  cat("Skipping — not all variables confirmed I(1)\n")
} else {
  cat("All I(1) confirmed — cointegration testing required\n\n")
  
  cat("NOTE: Westerlund (2007) ECM test most reliable in Stata:\n")
  cat("  xtwest ln_co2_pc ln_wealth_top1 ln_gdp_pc preinc_gini postinc_gini,\n")
  cat("         lags(1) leads(1) lrwindow(3) bootstrap(500)\n\n")
  
  # R fallback: Pedroni test via plm
  cat("--- Pedroni Cointegration Test (R fallback) ---\n")
  cat("H0: No cointegration\n\n")
  
  tryCatch({
    # Manual Pedroni via residual-based approach
    # Fit FE model and test residuals for unit root
    fe_resid <- plm(
      ln_co2_pc ~ ln_wealth_top1 + ln_gdp_pc + preinc_gini + postinc_gini,
      data = pdata, model = "within", effect = "twoways"
    )
    
    resids <- residuals(fe_resid)
    
    # ADF on pooled residuals — rudimentary cointegration check
    adf_resid <- adf.test(resids)
    cat(sprintf("ADF on FE residuals: stat = %.4f, p = %.4f\n",
                adf_resid$statistic, adf_resid$p.value))
    cat(sprintf("Decision: %s\n",
                ifelse(adf_resid$p.value < 0.05,
                       "Residuals stationary — cointegration likely",
                       "Residuals non-stationary — no cointegration")))
    cat("\nCAUTION: This is a simplified check.\n")
    cat("Use Westerlund in Stata for dissertation-quality evidence.\n")
    
  }, error = function(e) {
    cat("Pedroni fallback failed:", e$message, "\n")
    cat("Proceed with Stata Westerlund test.\n")
  })
}


# ============================================================
# STAGE 5 — ENDOGENEITY TESTS
# ============================================================

# 5.1 Hausman Test — FE vs RE

fe_haus <- plm(
  ln_co2_pc ~ ln_wealth_top1 + ln_gdp_pc + preinc_gini,
  data = pdata, model = "within", effect = "twoways"
)

re_haus <- plm(
  ln_co2_pc ~ ln_wealth_top1 + ln_gdp_pc + preinc_gini,
  data = pdata, model = "random", effect = "twoways"
)

haus <- phtest(fe_haus, re_haus)
print(haus)
cat(sprintf("\nDecision: %s\n",
            ifelse(haus$p.value < 0.05,
                   "FE preferred — correlated country effects confirmed",
                   "RE not rejected — but FE still appropriate for this panel")))


# 5.2 Durbin-Wu-Hausman — Endogeneity of ln_wealth_top1
# Uses lagged 1 and 2 wealth as instrument (available without external data)

# Complete cases only — lags create NAs in first two years per country
panel_dwh <- panel %>%
  filter(
    !is.na(ln_wealth_top1_lag1),
    !is.na(ln_wealth_top1_lag2),
    !is.na(ln_co2_pc),
    !is.na(ln_gdp_pc),
    !is.na(preinc_gini),
    !is.na(postinc_gini)
  )

# First stage: regress wealth on instruments + controls + FE
first_stage <- lm(
  ln_wealth_top1 ~ ln_wealth_top1_lag1 +
    ln_gdp_pc + preinc_gini + postinc_gini +
    factor(iso3c) + factor(year),
  data = panel_dwh
)

panel_dwh$fs_resid <- residuals(first_stage)

# Second stage: include first-stage residuals
# Significant residual = endogeneity confirmed
panel_dwh <- panel %>%
  filter(!is.na(ln_wealth_top1_lag1),
         !is.na(ln_co2_pc),
         !is.na(ln_gdp_pc),
         !is.na(postinc_gini))

# First stage — lag 1 only
first_stage <- feols(
  ln_wealth_top1 ~ ln_wealth_top1_lag1 + ln_gdp_pc + postinc_gini | iso3c + year,
  data = panel_dwh
)

panel_dwh$fs_resid <- residuals(first_stage)

# Second stage
second_stage <- feols(
  ln_co2_pc ~ ln_wealth_top1 + ln_gdp_pc + postinc_gini +
    fs_resid | iso3c + year,
  data = panel_dwh
)

resid_row  <- summary(second_stage)$coeftable["fs_resid", ]
endogenous <- resid_row["Pr(>|t|)"] < 0.05

cat(sprintf("First-stage residual coef : %.6f\n", resid_row["Estimate"]))
cat(sprintf("t-statistic               : %.4f\n", resid_row["t value"]))
cat(sprintf("p-value                   : %.4f\n", resid_row["Pr(>|t|)"]))
cat(sprintf("\nDecision: ln_wealth_top1 is %s\n",
            ifelse(endogenous,
                   "ENDOGENOUS — System-GMM required as primary estimator",
                   "EXOGENOUS — FE with clustered SE is consistent")))

# Run the IV model directly in one step!
# The syntax is: dependent ~ exogenous_controls | fixed_effects | endogenous_var ~ instrument
iv_model <- feols(
  ln_co2_pc ~ ln_gdp_pc + preinc_gini + postinc_gini | 
    iso3c + year | 
    ln_wealth_top1 ~ ln_wealth_top1_lag1,
  data = panel_dwh
)

# Ask fixest to report the Wu-Hausman endogeneity test
fitstat(iv_model, ~ wh + ivf1)

cat("\n--- Endogeneity Assessment ---\n")

# DWH test not viable for this panel:
# Two-way FE absorbs sufficient variation that lagged instruments
# become collinear after demeaning — a known limitation with
# persistent variables like wealth inequality in short-T panels.
# Endogeneity is assessed via:
# 1. Dumitrescu-Hurlin causality test (Stage 6)
# 2. System-GMM as robustness estimator (Stage 8)

# Weak instrument check — first stage F statistic
first_stage <- feols(
  ln_wealth_top1 ~ ln_wealth_top1_lag1 + ln_gdp_pc + postinc_gini |
    iso3c + year,
  data = panel_dwh
)

cat("\nFirst stage F-statistic (instrument strength):\n")
print(fitstat(first_stage, "f"))

cat("\nNote: Endogeneity formally tested via System-GMM in robustness stage\n")
cat("and causality direction assessed via Dumitrescu-Hurlin in Stage 6\n")

# ============================================================
# STAGE 6 — CAUSALITY DIRECTION
# ============================================================

# R approximation: country-by-country Granger, then summarise

countries   <- unique(panel$iso3c)
w_to_co2    <- numeric(length(countries))
co2_to_w    <- numeric(length(countries))

for (i in seq_along(countries)) {
  cdata <- panel %>%
    filter(iso3c == countries[i]) %>%
    arrange(year) %>%
    select(ln_co2_pc, ln_wealth_top1) %>%
    na.omit()
  
  if (nrow(cdata) >= 10) {
    tryCatch({
      gt1 <- grangertest(ln_co2_pc    ~ ln_wealth_top1, order = 2, data = cdata)
      gt2 <- grangertest(ln_wealth_top1 ~ ln_co2_pc,    order = 2, data = cdata)
      w_to_co2[i]  <- gt1$`Pr(>F)`[2]
      co2_to_w[i]  <- gt2$`Pr(>F)`[2]
    }, error = function(e) {
      w_to_co2[i]  <<- NA
      co2_to_w[i]  <<- NA
    })
  } else {
    w_to_co2[i] <- NA
    co2_to_w[i] <- NA
  }
}

pct_w_co2  <- mean(w_to_co2  < 0.05, na.rm = TRUE) * 100
pct_co2_w  <- mean(co2_to_w  < 0.05, na.rm = TRUE) * 100

cat(sprintf("Wealth → CO2  significant in %.1f%% of countries\n", pct_w_co2))
cat(sprintf("CO2 → Wealth  significant in %.1f%% of countries\n", pct_co2_w))


# Stage 6 - Dumitrescu-Hurlin Panel Causality Test
# Test 1: Wealth inequality → CO2
dh_w_to_co2 <- pgrangertest(ln_co2_pc ~ ln_wealth_top1, 
                            order = 2, 
                            data = pdata)

cat("\nWealth inequality → CO2 emissions:\n")
print(dh_w_to_co2)

# Test 2: CO2 → Wealth inequality
dh_co2_to_w <- pgrangertest(ln_wealth_top1 ~ ln_co2_pc, 
                            order = 2, 
                            data = pdata)

cat("\nCO2 emissions → Wealth inequality:\n")
print(dh_co2_to_w)

# Summary
w_causes_co2 <- dh_w_to_co2$p.value < 0.05
co2_causes_w <- dh_co2_to_w$p.value < 0.05

cat("\n--- Causality Summary ---\n")
cat(sprintf("Wealth → CO2 : %s\n",
            ifelse(w_causes_co2, "YES (p < 0.05)", "NO")))
cat(sprintf("CO2 → Wealth : %s\n",
            ifelse(co2_causes_w, "YES (p < 0.05)", "NO")))

if (w_causes_co2 & co2_causes_w) {
  cat("BIDIRECTIONAL — endogeneity concern confirmed\n")
} else if (w_causes_co2) {
  cat("UNIDIRECTIONAL: Wealth → CO2 — consistent with hypothesis\n")
} else if (co2_causes_w) {
  cat("REVERSE causality detected — GMM robustness check essential\n")
} else {
  cat("No panel-level Granger causality detected\n")
}



# ============================================================
# STAGE 7 — MODEL SPECIFICATION
# ============================================================

# 7.1 AIC / BIC across specifications — using fixest

cat("\n--- Model Fit: AIC / BIC ---\n")

m1 <- feols(ln_co2_pc ~ ln_wealth_top1 | iso3c + year,
            data = panel, cluster = ~iso3c)

m2 <- feols(ln_co2_pc ~ ln_wealth_top1 + ln_gdp_pc | iso3c + year,
            data = panel, cluster = ~iso3c)

m3 <- feols(ln_co2_pc ~ ln_wealth_top1 + ln_gdp_pc +
              preinc_gini | iso3c + year,
            data = panel, cluster = ~iso3c)

m4 <- feols(ln_co2_pc ~ ln_wealth_top1 + ln_gdp_pc +
              preinc_gini + postinc_gini | iso3c + year,
            data = panel, cluster = ~iso3c)

m5 <- feols(ln_co2_pc ~ ln_wealth_top1 + ln_gdp_pc + ln_gdp_pc_sq +
              preinc_gini + postinc_gini | iso3c + year,
            data = panel, cluster = ~iso3c)

spec_table <- data.frame(
  Model = c("M1: Base", "M2: +GDP", "M3: +preGini",
            "M4: +postGini", "M5: +EKC"),
  N     = sapply(list(m1, m2, m3, m4, m5), nobs),
  AIC   = round(sapply(list(m1, m2, m3, m4, m5), AIC), 1),
  BIC   = round(sapply(list(m1, m2, m3, m4, m5), BIC), 1)
)

print(spec_table)

########
# M5 already has ln_gdp_pc_sq = log(gdp_pc)^2
# This is the standard EKC specification — squaring the log
# For comparison also test squaring GDP in levels before logging

panel <- panel %>%
  mutate(ln_gdp_pc_sq2 = log(gdp_pc^2))  # equivalent to 2*ln_gdp_pc
# this is perfectly collinear
# so not useful

# What you actually want to check is whether the turning point
# is within your data range
m5 <- feols(
  ln_co2_pc ~ ln_wealth_top1 + ln_gdp_pc + ln_gdp_pc_sq +
    preinc_gini | iso3c + year,
  data = panel, cluster = ~iso3c
)

cat("\n--- EKC Turning Point Check ---\n")
coefs <- coef(m5)
beta1 <- coefs["ln_gdp_pc"]
beta2 <- coefs["ln_gdp_pc_sq"]

cat(sprintf("ln_gdp_pc coef    : %.4f\n", beta1))
cat(sprintf("ln_gdp_pc_sq coef : %.4f\n", beta2))

if (beta2 < 0) {
  # Turning point in log GDP terms: -beta1 / (2*beta2)
  turning_point_log <- -beta1 / (2 * beta2)
  turning_point_gdp <- exp(turning_point_log)
  
  cat(sprintf("\nInverted U confirmed (beta2 < 0)\n"))
  cat(sprintf("Turning point log GDP : %.4f\n", turning_point_log))
  cat(sprintf("Turning point GDP pc  : $%.0f\n", turning_point_gdp))
  
  # What share of your sample is above the turning point
  pct_above <- mean(panel$ln_gdp_pc > turning_point_log, na.rm = TRUE) * 100
  cat(sprintf("Sample above turning point: %.1f%%\n", pct_above))
  
  cat("\nInterpretation: EKC confirmed if turning point is\n")
  cat("within sample range and economically plausible\n")
  
} else if (beta2 > 0) {
  cat("\nU-shape detected (beta2 > 0) — emissions accelerating with growth\n")
  cat("EKC hypothesis NOT confirmed\n")
} else {
  cat("\nNo significant nonlinearity detected\n")
}

# Compare M4 vs M5 side by side
m4 <- feols(
  ln_co2_pc ~ ln_wealth_top1 + ln_gdp_pc + preinc_gini | iso3c + year,
  data = panel, cluster = ~iso3c
)

cat("\n--- M4 vs M5 Coefficient Comparison ---\n")
modelsummary(
  list("M4 Linear GDP" = m4,
       "M5 EKC"        = m5),
  stars    = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  coef_map = c(
    "ln_wealth_top1" = "Wealth Top 1% (log)",
    "ln_gdp_pc"      = "GDP pc (log)",
    "ln_gdp_pc_sq"   = "GDP pc squared (log)",
    "preinc_gini"   = "Pre-tax Gini"
  ),
  gof_map = c("nobs", "r.squared", "adj.r.squared")
)


# 7.2 VIF on final specification

vif_final <- lm(
  ln_co2_pc ~ ln_wealth_top1 + ln_gdp_pc + ln_gdp_pc_sq + preinc_gini,
  data = panel, na.action = na.omit
)
print(vif(vif_final))
# Not appropiate to check multico in squared specs

# ============================================================
# STAGE 8 — FALSIFICATION TESTS
# ============================================================

# Define baseline model for comparison throughout
baseline <- feols(
  ln_co2_pc ~ ln_wealth_top1 + ln_gdp_pc + ln_gdp_pc_sq + preinc_gini  |
    iso3c + year,
  data = panel, cluster = ~iso3c
)

# ------------------------------------------------------------
# 8.1 Alternative dependent variable — WID consumption emissions
# ------------------------------------------------------------

alt_dv <- feols(
  ln_wid_co2_pc ~ ln_wealth_top1 + ln_gdp_pc + ln_gdp_pc_sq + preinc_gini |
    iso3c + year,
  data = panel, cluster = ~iso3c
)

modelsummary(
  list("Production CO2" = baseline,
       "Consumption CO2 (WID)" = alt_dv),
  stars   = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  coef_map = c(
    "ln_wealth_top1" = "Wealth Top 1% (log)",
    "ln_gdp_pc"      = "GDP per capita (log)",
    "preinc_gini"    = "Pre-tax Gini"
  ),
  gof_map = c("nobs", "r.squared")
)

# 8.2 Alternative wealth measure — top 10%


alt_w10 <- feols(
  ln_co2_pc ~ ln_wealth_top10 + ln_gdp_pc + ln_gdp_pc_sq + preinc_gini |
    iso3c + year,
  data = panel, cluster = ~iso3c
)

modelsummary(
  list("Top 1% Wealth" = baseline,
       "Top 10% Wealth" = alt_w10),
  stars   = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  coef_map = c(
    "ln_wealth_top1"  = "Wealth Top 1% (log)",
    "ln_wealth_top10" = "Wealth Top 10% (log)"
  ),
  gof_map = c("nobs", "r.squared")
)
### COMPARE REST 9% LATER AND PRESENT FINDING ###
# 8.3 Sample period exclusions

m_post2000 <- feols(
  ln_co2_pc ~ ln_wealth_top1 + ln_gdp_pc + ln_gdp_pc_sq + preinc_gini |
    iso3c + year,
  data = panel %>% filter(year >= 2000),
  cluster = ~iso3c
)

m_no_gfc <- feols(
  ln_co2_pc ~ ln_wealth_top1 + ln_gdp_pc + ln_gdp_pc_sq + preinc_gini |
    iso3c + year,
  data = panel %>% filter(!year %in% c(2008, 2009)),
  cluster = ~iso3c
)

# COVID not an issue — panel ends 2020 only one year affected
m_no2020 <- feols(
  ln_co2_pc ~ ln_wealth_top1 + ln_gdp_pc + ln_gdp_pc_sq + preinc_gini |
    iso3c + year,
  data = panel %>% filter(year <= 2019),
  cluster = ~iso3c
)

modelsummary(
  list("Full 1991-2020"  = baseline,
       "Post-2000"       = m_post2000,
       "No GFC 08-09"    = m_no_gfc,
       "Pre-COVID"       = m_no2020),
  stars   = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  coef_map = c("ln_wealth_top1" = "Wealth Top 1% (log)"),
  gof_map  = c("nobs", "r.squared")
)

# 8.4 OECD vs non-OECD heterogeneity

m_oecd <- feols(
  ln_co2_pc ~ ln_wealth_top1 + ln_gdp_pc + ln_gdp_pc_sq + preinc_gini |
    iso3c + year,
  data = panel %>% filter(oecd == 1),
  cluster = ~iso3c
)

m_nonoecd <- feols(
  ln_co2_pc ~ ln_wealth_top1 + ln_gdp_pc + ln_gdp_pc_sq + preinc_gini |
    iso3c + year,
  data = panel %>% filter(oecd == 0),
  cluster = ~iso3c
)

modelsummary(
  list("Full sample" = baseline,
       "OECD"        = m_oecd,
       "Non-OECD"    = m_nonoecd),
  stars   = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  coef_map = c(
    "ln_wealth_top1" = "Wealth Top 1% (log)",
    "ln_gdp_pc"      = "GDP per capita (log)",
    "ln_gdp_pc_sq"      = "GDP per capita SQ (log)"
  ),
  gof_map = c("nobs", "r.squared")
)


#============================================================
# WORKFLOW SUMMARY

cat(sprintf("CSD present          : %s\n",
            ifelse(csd_present, "YES — 2nd gen unit roots needed", "NO")))
cat(sprintf("Slopes heterogeneous : %s\n",
            ifelse(slope_het,   "YES — FE = average effect only", "NO")))
cat(sprintf("Variables I(1)       : %s\n",
            ifelse(all_I1,      "YES — cointegration needed", "NO")))
cat(sprintf("Endogeneity detected : %s\n",
            ifelse(endogenous,  "YES — GMM required", "NO")))

cat("\n--- Recommended Estimator Sequence ---\n")
cat("1. feols() two-way FE + cluster ~iso3c       [baseline]\n")
cat("2. Driscoll-Kraay SE via sandwich            [CSD-robust baseline]\n")
cat("3. Prais-Winsten PCSE                        [Knight et al. comparability]\n")
cat("4. System-GMM via pgmm()                     [endogeneity]\n")
cat("5. FMOLS if cointegration confirmed          [long-run]\n")

cat("\n--- Stata Commands Needed ---\n")
cat("xtcips   : CIPS unit root (CSD-robust)\n")
cat("xtwest   : Westerlund cointegration\n")
cat("xtgcause : Dumitrescu-Hurlin causality\n")
cat("xtslope  : Pesaran-Yamagata slope heterogeneity\n")

