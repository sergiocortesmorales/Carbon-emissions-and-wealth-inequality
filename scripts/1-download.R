#Install and load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here, wid)

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
saveRDS(ineq_base, here("rawdata","ineq_raw.rds"))

rm(ineq_base)