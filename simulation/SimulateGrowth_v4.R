#=================================================================================
# SIMULATION
#=================================================================================

rm(list=ls())

source("../src/plot.growth.R")
source("SimGrowth.R")
source("../src/time-step.R")


#=================================================================================
# SIMULATE DATA
#=================================================================================
Ndesign <- 100 # The number of simulations we will do
Nindiv <- 315  # The same as our estimation model
Nareas <- 1


#=================================================================================
# SPECIFY PARAMETERS FOR SIMULATION
#=================================================================================
L0 <- c(43,50)
bmean <- c(0.00100,0.00106)
sd_b <- c(0.21,0.19)
gamma <- c(0.19,0.19)
psi <- c(0.0000000002,0.0000000002)
sd_obs <- c(0.083,0.083)
sd_z <- c(0.05,0.05)
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
                         obs_err = TRUE, tvi_err = TRUE, Pars, Nindiv)
    #ATR_sim <- SimGrowth(ln_xdev=NULL, ln_ydev=NULL, obs_err=FALSE, tvi_err=TRUE)$ATR_sim
    #ATR_sim <- SimGrowth(ln_xdev=NULL, ln_ydev=NULL, obs_err=TRUE, tvi_err=FALSE)$ATR_sim
    #ATR_sim <- SimGrowth(ln_xdev=NULL, ln_ydev=NULL, obs_err=FALSE, tvi_err=FALSE, Input[[1]]$Parameters, Input[[1]]$Data)$ATR_sim
    sim <- list(Sim = ATR_sim, Parameters = Pars)
    save(sim, file = paste("sims/sim", ss, ".RData", sep = ""))
    plot.growth(ATR_sim, ss)
}
