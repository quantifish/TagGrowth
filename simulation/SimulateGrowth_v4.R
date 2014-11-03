#=================================================================================
# SIMULATION
#=================================================================================

rm(list=ls())
require(tagGrowth)
source("SimGrowth.R")


#=================================================================================
# SIMULATE DATA
#=================================================================================
Ndesign <- 100 # The number of simulations we will do
Nindiv <- 315  # The same as our estimation model
Nareas <- 1


#=================================================================================
# SPECIFY PARAMETERS FOR SIMULATION
#=================================================================================
L0 <- c(0.0,6.9)
bmean <- c(0.003,0.003)
sd_b <- c(0.106,0.112)
gamma <- c(0.4,0.4)
psi <- c(0.001,0.001)
sd_obs <- c(0.099,0.099)
sd_z <- c(0.0,0.0)
sd_y <- c(0,0)

Pars <- rbind(L0, bmean, sd_b, gamma, psi, sd_obs, sd_z, sd_y)
colnames(Pars) <- c("female","male")


#=================================================================================
# RUN THE SIMULATION MODEL
#=================================================================================
set.seed(15)
for (ss in 1:Ndesign)
{
    #ATR_sim <- SimGrowth(ln_xdev=NULL, ln_ydev=ln_ydev, obs_err=TRUE, tvi_err=TRUE)$ATR_sim
    ATR_sim <- SimGrowth(ln_xdev = NULL, ln_ydev = NULL,
                         obs_err = TRUE, tvi_err = FALSE, Pars, Nindiv)
    #ATR_sim <- SimGrowth(ln_xdev=NULL, ln_ydev=NULL, obs_err=FALSE, tvi_err=TRUE)$ATR_sim
    #ATR_sim <- SimGrowth(ln_xdev=NULL, ln_ydev=NULL, obs_err=TRUE, tvi_err=FALSE)$ATR_sim
    #ATR_sim <- SimGrowth(ln_xdev=NULL, ln_ydev=NULL, obs_err=FALSE, tvi_err=FALSE, Input[[1]]$Parameters, Input[[1]]$Data)$ATR_sim
    sim <- list(Sim = ATR_sim, Parameters = Pars)
    save(sim, file = paste("sims/sim", ss, ".RData", sep = ""))
    plot_growth(ATR_sim, ss)
}
