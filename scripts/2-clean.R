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
    co2pc_territorial_wid = kntcar,
    co2pc_consumption_wid   = knfcar,
    ghgpc_territorial_wid = kntghg,
    ghgpc_consumption_wid   = knfghg
  )
### SAVE ###
saveRDS(emis_wid, here("data", "emis_wid.rds"))




#################
### EMISSIONS ###
#################
emis <-  read.csv(here("rawdata","owid-co2-data.csv"))#Read
#Select columns, year range, and drop regions
emis <- emis %>% rename(iso3c = iso_code, co2pc_territorial = co2_per_capita, co2pc_territorial_incluc = co2_including_luc_per_capita, co2pc_consumption = consumption_co2_per_capita, ghgpc_territorial_excluc = ghg_excluding_lucf_per_capita, ghgpc_territorial = ghg_per_capita) %>%
  select(country, iso3c, year, co2pc_territorial, co2pc_territorial_incluc, ghgpc_territorial_excluc, ghgpc_territorial,  co2pc_consumption) %>%
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
ctrl <- ctrl %>% #Drop irrelevant columns
  select(-status, -lastupdated, -longitude, -latitude, -lending, -capital) %>%
  #Drop indicators not (yet) needed for the analysis
  select(country, iso2c, iso3c, year, gdppc, age_dep, pop_growth, urban, internet, trade,
         region, income) %>%
  mutate(region = case_when(
    iso2c == "PR" ~ "Latin America & Caribbean",
    iso2c == "SO" ~ "Sub-Saharan Africa",
    TRUE ~ region
  )) %>%
  filter(region != "Aggregates") %>%
  filter(!is.na(region))
### SAVE ###
saveRDS(ctrl, here("data", "ctrl.rds"))

####################
### CLIMATE LAWS ###
####################
# Climate Change Laws of the World (Climate Policy Radar + Grantham source)
# Build country-year cumulative stocks of laws/policies in force
# Amendments and previous versions are dropped
# Repealed/Replaced families are dropped from the stock from that year onward.
# Two stocks:
# claw_legi  - legislative-category families (Acts, Laws, EU Regulations)
# claw_total - legislative + executive (also plans, strategies, decrees)
# EU instruments are added to the stock of each EU member state
# EU Directives are excluded as they have to be passed in national legislation

# Helper to extract earliest year of any event matching a target type list
law_year <- function(types_str, dates_str, targets) {
  if (is.na(types_str) || is.na(dates_str)) return(NA_integer_)
  types <- trimws(strsplit(types_str, ";")[[1]])
  dates <- trimws(strsplit(dates_str, ";")[[1]])
  if (length(types) != length(dates)) return(NA_integer_)
  hits <- types %in% targets
  if (!any(hits)) return(NA_integer_)
  yrs <- suppressWarnings(as.integer(format(as.Date(dates[hits]), "%Y")))
  yrs <- yrs[!is.na(yrs) & yrs >= 1900 & yrs <= 2030]
  if (length(yrs) == 0) return(NA_integer_)
  min(yrs)
}

claw_raw <- read.csv(
  here("rawdata", "Document_Data_Download-2026-04-27.csv"),
  stringsAsFactors = FALSE
) %>%
  rename(
    family_id      = Family.ID,
    iso3c          = Geography.ISOs,
    role           = Document.Role,
    category       = Category,
    doc_type       = Document.Type,
    timeline_types = Full.timeline.of.events..types.,
    timeline_dates = Full.timeline.of.events..dates.
  ) %>%
  # Keep only main rows (drop amendments, annexes, previous versions, etc.;
  # families are reconstructed below)
  filter(role == "Main") %>%
  # Drop documents with no country (XAA); EU (EUR) is kept and split out below
  filter(!iso3c %in% c("", "XAA")) %>%
  # Keep national legislative and executive instruments (drop UNFCCC)
  filter(category %in% c("Legislative", "Executive"))

# Aggregate to family level: earliest in-force year, earliest repeal year.
# mapply is used instead of rowwise() for speed
claw_fam <- claw_raw %>%
  mutate(
    in_force_year = mapply(law_year, timeline_types, timeline_dates,
                           MoreArgs = list(targets = c("Passed/Approved",
                                                       "Entered Into Force")),
                           USE.NAMES = FALSE),
    repeal_year   = mapply(law_year, timeline_types, timeline_dates,
                           MoreArgs = list(targets = c("Repealed/Replaced")),
                           USE.NAMES = FALSE)
  ) %>%
  group_by(family_id) %>%
  summarise(
    iso3c         = first(iso3c),
    category      = first(category),
    doc_type      = first(doc_type),
    in_force_year = suppressWarnings(min(in_force_year, na.rm = TRUE)),
    repeal_year   = suppressWarnings(min(repeal_year,   na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    in_force_year = ifelse(is.infinite(in_force_year), NA_integer_, in_force_year),
    repeal_year   = ifelse(is.infinite(repeal_year),   NA_integer_, repeal_year)
  ) %>%
  filter(!is.na(in_force_year))

# Split national vs EU. EU subset drops Directives (transposed into
# national law and so already counted under each member state's own ISO)
claw_fam_nat <- claw_fam %>% filter(iso3c != "EUR")
claw_fam_eu  <- claw_fam %>% filter(iso3c == "EUR", doc_type != "Eu Directive")

# Build the national country-year stock as the cumulative sum of (in-force) -1 (repealed)
country_year_grid <- expand.grid(
  iso3c = sort(unique(claw_fam_nat$iso3c)),
  year  = min(claw_fam_nat$in_force_year):2023,
  stringsAsFactors = FALSE
)

claw_total_panel <- bind_rows(
  claw_fam_nat %>% transmute(iso3c, year = in_force_year, delta = 1L),
  claw_fam_nat %>% filter(!is.na(repeal_year)) %>%
    transmute(iso3c, year = repeal_year, delta = -1L)
) %>%
  group_by(iso3c, year) %>%
  summarise(delta = sum(delta), .groups = "drop") %>%
  right_join(country_year_grid, by = c("iso3c", "year")) %>%
  mutate(delta = replace_na(delta, 0L)) %>%
  arrange(iso3c, year) %>%
  group_by(iso3c) %>%
  mutate(claw_total = cumsum(delta)) %>%
  ungroup() %>%
  select(iso3c, year, claw_total)

claw_legi_fam <- claw_fam_nat %>% filter(category == "Legislative")
claw_legi_panel <- bind_rows(
  claw_legi_fam %>% transmute(iso3c, year = in_force_year, delta = 1L),
  claw_legi_fam %>% filter(!is.na(repeal_year)) %>%
    transmute(iso3c, year = repeal_year, delta = -1L)
) %>%
  group_by(iso3c, year) %>%
  summarise(delta = sum(delta), .groups = "drop") %>%
  right_join(country_year_grid, by = c("iso3c", "year")) %>%
  mutate(delta = replace_na(delta, 0L)) %>%
  arrange(iso3c, year) %>%
  group_by(iso3c) %>%
  mutate(claw_legi = cumsum(delta)) %>%
  ungroup() %>%
  select(iso3c, year, claw_legi)

# Build EU's own cumulative stock (one time series, not country-year),
eu_year_grid <- data.frame(year = min(claw_fam_eu$in_force_year):2023)

eu_total_stock <- bind_rows(
  claw_fam_eu %>% transmute(year = in_force_year, delta = 1L),
  claw_fam_eu %>% filter(!is.na(repeal_year)) %>%
    transmute(year = repeal_year, delta = -1L)
) %>%
  group_by(year) %>%
  summarise(delta = sum(delta), .groups = "drop") %>%
  right_join(eu_year_grid, by = "year") %>%
  mutate(delta = replace_na(delta, 0L)) %>%
  arrange(year) %>%
  mutate(eu_total = cumsum(delta)) %>%
  select(year, eu_total)

eu_legi_fam <- claw_fam_eu %>%
  filter(category == "Legislative", doc_type == "Eu Regulation")
eu_legi_stock <- bind_rows(
  eu_legi_fam %>% transmute(year = in_force_year, delta = 1L),
  eu_legi_fam %>% filter(!is.na(repeal_year)) %>%
    transmute(year = repeal_year, delta = -1L)
) %>%
  group_by(year) %>%
  summarise(delta = sum(delta), .groups = "drop") %>%
  right_join(eu_year_grid, by = "year") %>%
  mutate(delta = replace_na(delta, 0L)) %>%
  arrange(year) %>%
  mutate(eu_legi = cumsum(delta)) %>%
  select(year, eu_legi)

# EU member status by country-year, new members enter from their accession year
eu_members <- data.frame(
  iso3c = c("BEL","DEU","FRA","ITA","LUX","NLD","DNK","IRL","GRC","PRT","ESP","GBR",
            "AUT","FIN","SWE",
            "CYP","CZE","EST","HUN","LVA","LTU","MLT","POL","SVK","SVN",
            "BGR","ROU",
            "HRV"),
  join_year = c(rep(1991L, 12),
                rep(1995L, 3),
                rep(2004L, 10),
                rep(2007L, 2),
                2013L),
  exit_year = NA_integer_, #Brexit ignored for simplification
  stringsAsFactors = FALSE
)
eu_member_year <- crossing(eu_members, year = 1991:2023) %>%
  filter(year >= join_year & (is.na(exit_year) | year < exit_year)) %>%
  select(iso3c, year)
# Build laws panel
claw <- claw_total_panel %>%
  left_join(claw_legi_panel, by = c("iso3c", "year")) %>%
  filter(year >= 1991 & year <= 2023) %>%
  left_join(
    eu_member_year %>%
      left_join(eu_total_stock, by = "year") %>%
      left_join(eu_legi_stock,  by = "year"),
    by = c("iso3c", "year")
  ) %>%
  mutate(
    claw_total = claw_total + replace_na(eu_total, 0L),
    claw_legi  = claw_legi  + replace_na(eu_legi,  0L)
  ) %>%
  select(-eu_total, -eu_legi) %>%
  mutate(
    country = countrycode(iso3c, origin = "iso3c", destination = "country.name",
                          custom_match = c("XKX" = "Kosovo")),
    iso2c   = countrycode(iso3c, origin = "iso3c", destination = "iso2c",
                          custom_match = c("XKX" = "XK"))
  ) %>%
  filter(!is.na(country)) %>%
  select(country, iso2c, iso3c, year, claw_legi, claw_total)
### SAVE ###
saveRDS(claw, here("data", "claw.rds"))
#If csv needed#write.csv(claw, here("data", "clawcsv.csv"))