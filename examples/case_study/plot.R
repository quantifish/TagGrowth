# Load package
require(TagGrowth)

# Load data
load("../../data/ATR_mod.RData")

# Versions
# 0. none
# 1. k
# 2. z
# 3. y
# 4. k, z
# 5. k, y
# 6. z, y
# 7. k, z, y
scenarios <- c("v0/","v1/","v2/","v4/")
#scenarios <- c("v0/","v1/","v2/","v3/","v4/","v5/","v6/","v7/")
LL <- rep(NA, length(scenarios))
npar <- rep(NA, length(scenarios))
aic <- rep(NA, length(scenarios))

for (Iscenario in scenarios)
{
    # Directory to save output files to
    folder <- Iscenario

    # Load report
    load(paste(folder, "Report.RData", sep = ""))
    
    load(paste(folder, "opt.RData", sep = ""))
    LL[which(scenarios == Iscenario)] <- -1 * opt$objective
    npar[which(scenarios == Iscenario)] <- length(opt$par)
    aic[which(scenarios == Iscenario)] <- (2 * opt$objective) + (2 * length(opt$par))

    # Append model outputs to ATR_mod
    ATR_mod$Length1_hat <- Report$value[names(Report$value) %in% "Length1_hat"]
    ATR_mod$Length2_hat <- Report$value[names(Report$value) %in% "Length2_hat"]

    # Do some plots
    plot_obs_pred(ATR_mod$Sex, ATR_mod$Length1, ATR_mod$Length1_hat, ATR_mod$Length2, ATR_mod$Length2_hat, file_name = paste0(folder, "ObsVsPred"))
    plot_indiv_growth(ATR_mod$Sex, ATR_mod$Age1, ATR_mod$Length1, ATR_mod$Length1_hat, ATR_mod$Age2, ATR_mod$Length2, ATR_mod$Length2_hat, file_name = paste0(folder, "IndivGrowth"))
    if (Iscenario %in% c("v1/","v4/","v5/","v7/"))
        plot_histogram_k(ATR_mod, Report, file_name = paste0(folder, "REs_b"))
    if (Iscenario %in% c("v2/","v4/","v6/","v7/"))
        plot_histogram_z(ATR_mod, Report, file_name = paste0(folder, "REs_z"))
    if (Iscenario %in% c("v3/","v5/","v6/","v7/"))
        plot_annual_devs(Report, file_name = paste0(folder, "REs_y"))
    #plot_linf(Report, file_name = paste0(folder, "LinfPrior"))
}

LL
npar
aic

# END
