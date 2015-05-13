#=================================================================================
# SIMULATION DESIGN
#=================================================================================

# Make sure R is clean
rm(list=ls())

#=================================================================================
# USER SPECIFICATIONS
# =================================================================================
# Directory to save simulations to (save as .RData files), these are also the
# scenarios
#scenarios <- c("v0/","v1/","v2/","v3/")
scenarios <- c("none_none/","k_k/","z_z/","kz_kz/")
Ndesign <- 200                # The number of simulations we will do
set.seed(15)                  # A random number seed
power <- c(50, 100, 250, 500) # Power analysis
Nareas <- 1                   # The number of areas in our simulation

#=================================================================================
# SET UP SIMULATION
#=================================================================================
# Import library and load data (for resampling)
require(TagGrowth)
load("../../data/ATR_mod.RData")

# Annual
t0 = 0.021
k = 0.090
Linf = 180.20
age = 1:20
#plot(age, Linf*(1 - exp(-k*(age - t0))))

# Weekly
t0 = -0.256*52
k = 0.093/52
Linf = 169.07
age = 1:20*52
#plot(age, Linf*(1 - exp(-k*(age - t0))))

#=================================================================================
# SPECIFY PARAMETERS FOR SIMULATION
#=================================================================================
# These are the par values from the Dunn et al. 2008 paper
t0   <- c(0.021,  -0.256)
k    <- c(0.090,  0.093)
Linf <- c(180.20, 169.07)
cv   <- 0.102

psi   <- 0
L0    <- Linf * (1 - exp(k/52 * t0*52))
b     <- k/52
gamma <- (b * Linf) / (b^psi)
# sd_obs is actually a cv

#           females, males
L0 <-     c(L0[1],    L0[2])    # L0 = Linf*(1 - exp(k*t0))
bmean <-  c(b[1],     b[2])
gamma <-  c(gamma[1], gamma[2]) # gamma = (b * Linf) / (k^psi)
psi <-    c(0.0,      0.0)
sd_y <-   c(0.0,      0.0)


#=================================================================================
# RUN THE SIMULATION MODEL
#=================================================================================
for (Iscenario in scenarios)
{
    # Folder structure
    directory <- Iscenario
    dir.create(directory)
    lapply(file.path(directory, power), dir.create)

    # Parameters specific to each scenario
    if (directory == "v0/")
    {
        sd_b <-   c(0.0, 0.0)
        sd_z <-   c(0.0, 0.0)
        sd_obs <- c(0.102, 0.102)
    }
    if (directory == "v1/")
    {
        sd_b <-   c(0.1, 0.2)
        sd_z <-   c(0.0, 0.0)
        sd_obs <- c(0.05, 0.05)
    }
    if (directory == "v2/")
    {
        sd_b <-   c(0.0, 0.0)
        sd_z <-   c(0.3, 0.3)
        sd_obs <- c(0.05, 0.05)
    }
    if (directory == "v3/")
    {
        sd_b <-   c(0.1, 0.2)
        sd_z <-   c(0.3, 0.3)
        sd_obs <- c(0.05, 0.05)
    }
    Pars <- rbind(L0, bmean, sd_b, gamma, psi, sd_obs, sd_z, sd_y)
    colnames(Pars) <- c("female", "male")

    for (Ipow in power)
    {
        Nindiv <- Ipow
        for (Isim in 1:Ndesign)
        {
            ATR_sim <- GrowthModel(obs_err = TRUE, tvi_err = FALSE, Pars = Pars, Nindiv = Nindiv, ATR_mod = ATR_mod)
            sim <- list(Sim = ATR_sim, Parameters = Pars)
            save(sim, file = paste0(directory, Ipow, "/sim", Isim, ".RData"))
            #plot_growth(ATR_sim, Isim)
        }
    }
}

# END
