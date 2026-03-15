#Install and load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here, wid, WDI)

################
### DOWNLOAD ###
################
                ##################
                ### INEQUALITY ###
                ##################
#Download from World Inequalit Database (WID) API (package)
ineq_raw <- download_wid(
  indicators = c("shweal", "ghweal", "sptinc", "gptinc", "sdiinc", "gdiinc"), #Indicators, e.g. "s" = Share, "hweal" = Net personal wealth
  perc = c("p99p100", "p90p99", "p90p100", "p0p100", "p0p50"), #quantiles
  ages = 992, #Adults aged 20 and over
  pop = "j", #Equal-split adults
  areas = "all", #All available countries/regions
  years = 1991:2023
)
saveRDS(ineq_raw, here("rawdata","ineq_raw.rds"))
rm(ineq_raw)
### Download emissions from WID
emis_wid_raw <- download_wid(
  indicators = c("kntcar", "knfcar", "kntghg", "knfghg"),
  perc = "p0p100",
  ages = 999,
  pop = "i",
  areas = "all",
  years = 1991:2023
)
saveRDS(emis_wid_raw, here("rawdata","emis_wid_raw.rds"))

                #################
                ### EMISSIONS ###
                #################
#Download emissions csv from github.com/owid/co2-data (OWID)
#Put it in project's "rawdata" folder. Continue in 2-clean

                ##################
                ### WORLD BANK ###
                ##################
#Download controls from World Development Indicators API (package)
ctrl_raw <- WDI(
  indicator = c(
    # Core controls
    gdp_pc         = "NY.GDP.PCAP.KD",
    gdp_pc_ppp     = "NY.GDP.PCAP.PP.KD",
    trade          = "NE.TRD.GNFS.ZS",
    # Energy & environment
    elec_access    = "EG.ELC.ACCS.ZS",
    energy_alt     = "EG.USE.COMM.CL.ZS",
    energy_crnw    = "EG.USE.CRNW.ZS",
    energy_use_pc  = "EG.USE.PCAP.KG.OE",
    elec_fossil    = "EG.ELC.FOSL.ZS",
    elec_hydro     = "EG.ELC.HYRO.ZS",
    elec_ngas      = "EG.ELC.NGAS.ZS",
    elec_nuclear   = "EG.ELC.NUCL.ZS",
    elec_renew     = "EG.ELC.RNWX.ZS",
    fossil_fuel    = "EG.USE.COMM.FO.ZS",
    forest         = "AG.LND.FRST.ZS",
    nat_rents      = "NY.GDP.TOTL.RT.ZS",
    # Economic structure
    agri_land      = "AG.LND.AGRI.ZS",
    agri_va        = "NV.AGR.TOTL.ZS",
    manuf_va       = "NV.IND.MANF.ZS",
    empl_agri      = "SL.AGR.EMPL.ZS",
    empl_industry  = "SL.IND.EMPL.ZS",
    capital_form   = "NE.GDI.TOTL.ZS",
    mkt_cap        = "CM.MKT.LCAP.GD.ZS",
    # Financial & fiscal
    credit_priv    = "FS.AST.PRVT.GD.ZS",
    credit_fin     = "FS.AST.DOMS.GD.ZS",
    fdi_in         = "BX.KLT.DINV.WD.GD.ZS",
    fdi_out        = "BM.KLT.DINV.WD.GD.ZS",
    cur_account    = "BN.CAB.XOKA.GD.ZS",
    expense        = "GC.XPN.TOTL.GD.ZS",
    tax_rev        = "GC.TAX.TOTL.GD.ZS",
    real_int       = "FR.INR.RINR",
    oda            = "DT.ODA.ODAT.GN.ZS",
    # Demographics
    age_dep        = "SP.POP.DPND",
    pop            = "SP.POP.TOTL",
    pop_density    = "EN.POP.DNST",
    migrants       = "SM.POP.TOTL",
    urban          = "SP.URB.TOTL.IN.ZS",
    rural          = "SP.RUR.TOTL.ZS",
    # Social & institutional
    internet       = "IT.NET.USER.ZS",
    health_exp     = "SH.XPD.GHED.GD.ZS",
    edu_exp        = "SE.XPD.TOTL.GD.ZS",
    rd_exp         = "GB.XPD.RSDV.GD.ZS",
    # Governance (WGI) (multicollinear)
    corruption_est = "CC.EST",
    corruption_rnk = "CC.PER.RNK",
    reg_qual_est   = "RQ.EST",
    reg_qual_rnk   = "RQ.PER.RNK",
    rule_law_est   = "RL.EST",
    rule_law_rnk   = "RL.PER.RNK",
    voice_est      = "VA.EST",
    voice_rnk      = "VA.PER.RNK"
  ),
  country = "all",
  start = 1991,
  end = 2023,
  extra = TRUE
)

saveRDS(ctrl_raw, here("rawdata", "ctrl_raw.rds"))
rm(ctrl_raw)                           
         