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

# Annual
t0 = 0.021
k = 0.090
Linf = 180.20
age = 1:20
plot(age, Linf*(1 - exp(-k*(age - t0))))

# Weekly
t0 = -0.256*52
k = 0.093/52
Linf = 169.07
age = 1:20*52
plot(age, Linf*(1 - exp(-k*(age - t0))))

#=================================================================================
# SPECIFY PARAMETERS FOR SIMULATION
#=================================================================================
#           females, males
L0 <-     c(0.0,            4.0)   # L0 = Linf*(1 - exp(k*t0))
bmean <-  c(0.090/52,       0.093/52) # i.e. b = k
sd_b <-   c(0,              0)
gamma <-  c(0.090/52*180.2, 0.093/52*169.07) # gamma = (b * Linf) / (b^psi)
psi <-    c(0,              0)
sd_obs <- c(0.102,          0.102)
sd_z <-   c(0,              0)
sd_y <-   c(0,              0)

Pars <- rbind(L0, bmean, sd_b, gamma, psi, sd_obs, sd_z, sd_y)
colnames(Pars) <- c("female", "male")


#=================================================================================
# RUN THE SIMULATION MODEL
#=================================================================================
for (Isim in 1:Ndesign)
{
    ATR_sim <- SimGrowth(obs_err = TRUE, tvi_err = FALSE, Pars = Pars, Nindiv = Nindiv)
    sim <- list(Sim = ATR_sim, Parameters = Pars)
    save(sim, file = paste(directory, Isim, ".RData", sep = ""))
    plot_growth(ATR_sim, Isim)
}
