#=================================================================================
# SIMULATION
#=================================================================================

rm(list=ls())         # Make sure R is clean
require(tagGrowth)    # Import our library
source("SimGrowth.R") # Source the growth model function

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

# These are the par values from the Dunn et al. paper
t0   <- c(0.021,  -0.256)
k    <- c(0.090,  0.093)
Linf <- c(180.20, 169.07)
cv   <- 0.102

psi   <- 0
L0    <- Linf * (1 - exp(k * t0))
b     <- k/52
gamma <- (b * Linf) / (b^psi)


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
L0 <-     c(L0[1],    L0[2])    # L0 = Linf*(1 - exp(k*t0))
bmean <-  c(b[1],     b[2])
sd_b <-   c(0.0,      0.0)
gamma <-  c(gamma[1], gamma[2]) # gamma = (b * Linf) / (k^psi)
psi <-    c(0,        0)
sd_obs <- c(0.05,     0.05)       # this is actually a cv
sd_z <-   c(0.2,      0.4)
sd_y <-   c(0,        0)

Pars <- rbind(L0, bmean, sd_b, gamma, psi, sd_obs, sd_z, sd_y)
colnames(Pars) <- c("female", "male")
Pars


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

# END
