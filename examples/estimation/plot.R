# Load package
require(TagGrowth)

# Load data
load("../../data/ATR_mod.RData")

# Directory to save output files to
folder <- "v0/"

# Load report
load(paste(folder, "Report.RData", sep = ""))

# Append model outputs to ATR_mod
ATR_mod$Length1_hat <- Report$value[names(Report$value) %in% "Length1_hat"]
ATR_mod$Length2_hat <- Report$value[names(Report$value) %in% "Length2_hat"]

# Do some plots
plot_obs_pred(ATR_mod$Sex, ATR_mod$Length1, ATR_mod$Length1_hat, ATR_mod$Length2, ATR_mod$Length2_hat)
plot_indiv_growth(ATR_mod$Sex, ATR_mod$Age1, ATR_mod$Length1, ATR_mod$Length1_hat, ATR_mod$Age2, ATR_mod$Length2, ATR_mod$Length2_hat)
plot_linf(Report)
plot_histogram_b(ATR_mod, Report)
plot_histogram_z(ATR_mod, Report)
