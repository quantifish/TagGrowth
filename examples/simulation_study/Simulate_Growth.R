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
#xx <- expand.grid(simulator = c("none","k","z","kz"), estimator = c("none","k","z","kz"))
#scenarios <- paste0(xx[,1], "_", xx[,2], "/")
scenarios <- c("sim_none/","sim_k/","sim_z/","sim_kz/")
Ndesign <- 200                # The number of simulations we will do
set.seed(15)                  # A random number seed
power <- c(50, 100, 250, 500) # Power analysis
Nareas <- 1                   # The number of areas in our simulation

#=================================================================================
# SET UP SIMULATION
#=================================================================================
# Import library and load data (for resampling)
require(TagGrowth)
source("Growth_Model.R")
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
    # Identify which simulator/estimator we are using
    #xx <- unlist(strsplit(Iscenario, split = "_"))
    #csim <- xx[1]
    #cest <- gsub("/", "", xx[2])
    
    # Parameters specific to each scenario
    if (Iscenario == "sim_none/")
    {
        sd_b <-   c(0.0, 0.0)
        sd_z <-   c(0.0, 0.0)
        sd_obs <- c(0.102, 0.102)        
    }
    if (Iscenario == "sim_k/")
    {
        sd_b <-   c(0.1, 0.2)
        sd_z <-   c(0.0, 0.0)
        sd_obs <- c(0.05, 0.05)
    }
    if (Iscenario == "sim_z/")
    {
        sd_b <-   c(0.0, 0.0)
        sd_z <-   c(0.3, 0.3)
        sd_obs <- c(0.05, 0.05)
    }
    if (Iscenario == "sim_kz/")
    {
        sd_b <-   c(0.1, 0.2)
        sd_z <-   c(0.3, 0.3)
        sd_obs <- c(0.05, 0.05)
    }

    # Folder structure
    directory <- gsub("/", "", Iscenario)
    dir.create(directory)

    # Collect up the parameters
    Pars <- rbind(L0, bmean, sd_b, gamma, psi, sd_obs, sd_z, sd_y)
    colnames(Pars) <- c("female", "male")

    # Do the simulation
    Nindiv <- 315
    for (Isim in 1:Ndesign)
    {
        ATR_sim <- GrowthModel(obs_err = TRUE, tvi_err = FALSE, Pars = Pars, Nindiv = Nindiv, ATR_mod = ATR_mod)
        sim <- list(Sim = ATR_sim, Parameters = Pars)
        save(sim, file = paste0(Iscenario, "sim", Isim, ".RData"))
    }
    
}

Iscenario <- "sim_kz/"
lapply(file.path(Iscenario, power), dir.create)
    for (Ipow in power)
    {
        Nindiv <- Ipow
        for (Isim in 1:Ndesign)
        {
            ATR_sim <- GrowthModel(obs_err = TRUE, tvi_err = FALSE, Pars = Pars, Nindiv = Nindiv, ATR_mod = ATR_mod)
            sim <- list(Sim = ATR_sim, Parameters = Pars)
            save(sim, file = paste0(Iscenario, Ipow, "/sim", Isim, ".RData"))
            #plot_growth(ATR_sim, Isim)
        }
    }


# END
