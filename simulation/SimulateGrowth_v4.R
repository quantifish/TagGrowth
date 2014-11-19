#=================================================================================
# SIMULATION
#=================================================================================

rm(list=ls())         # Make sure R is clean
require(tagGrowth)    # Import our library
source("SimGrowth.R") # Source the growth model function


#=================================================================================
# SIMULATION SETUP
#=================================================================================
directory <- "sims/sim" # Directory to save simulations to (save as .RData files)
Ndesign <- 100          # The number of simulations we will do
Nindiv <- 315           # The same as our estimation model
Nareas <- 1             # The number of areas in our simulation
set.seed(15)            # A random number seed


#=================================================================================
# SPECIFY PARAMETERS FOR SIMULATION
#=================================================================================
#           females, males
L0 <-     c(0.0,     10.0)
bmean <-  c(0.003,   0.003)
sd_b <-   c(0,       0)
gamma <-  c(0.4,     0.4)
psi <-    c(0.001,   0.001)
sd_obs <- c(0.01,    0.01)
sd_z <-   c(0,       0)
sd_y <-   c(0,       0)

Pars <- rbind(L0, bmean, sd_b, gamma, psi, sd_obs, sd_z, sd_y)
colnames(Pars) <- c("female", "male")


#=================================================================================
# RUN THE SIMULATION MODEL
#=================================================================================
for (Isim in 1:Ndesign)
{
    ATR_sim <- SimGrowth(ln_xdev = NULL, ln_ydev = NULL, obs_err = FALSE, tvi_err = FALSE, Pars, Nindiv)
    sim <- list(Sim = ATR_sim, Parameters = Pars)
    save(sim, file = paste(directory, Isim, ".RData", sep = ""))
    plot_growth(ATR_sim, Isim)
}
