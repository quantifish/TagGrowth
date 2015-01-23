#=================================================================================
# SIMULATION
#=================================================================================

# This verison uses a more complex latin hypercube design

rm(list=ls())

require(BACCO) # Using the BACCO library for its latin hypercube design
source("../src/time-step.R")
source("../src/normalise.R")
source("../src/plot.growth.R")
source("../src/plot.parameters.R")
source("SimGrowth.R")


#=================================================================================
# SIMULATE DATA
#=================================================================================
Ndesign <- 20 # How many experiments would you like to do?
Nindiv <- 315 # Define the number of individuals in each simulation
Nsex <- 2
Nareas <- 1

# Simulate sex, ages at tagging and time at liberty for each individual
Sex <- rbinom(Nindiv, 1, 0.5) + 1 # (1=female, 2=male)
Age1 <- round(rnorm(n = Nindiv, 500, 150), digits = 0)
Liberty <- as.integer(runif(n = Nindiv, min = 0, max = 466))
Age2 <- Age1 + Liberty
Time0 <- rep(1, Nindiv)
Time1 <- rep(1, Nindiv)
Time2 <- rep(1, Nindiv)
Year0 <- rep(1, Nindiv)
Year1 <- rep(2, Nindiv)
Year2 <- rep(3, Nindiv)
Area1 <- rep(1, Nindiv)
Data <- data.frame(Sex, Age1, Age2, Liberty, Time0, Time1,
                   Year0, Year1, Year2, Area1)


#=================================================================================
# SIMULATE PARAMETERS
#=================================================================================
# Specify the bounds for each of the model parameters - don't forget some of
# these need to be sex-specific (this has not been done yet)
names <- c("L0","bmean","sd_b","gamma","psi","sd_obs","sd_z","sd_y")
Npar <- length(names)
bounds <- matrix(NA, nrow = Npar, ncol = 2)
rownames(bounds) <- names
colnames(bounds) <- c("lower", "upper")
bounds[1,] <- c(30, 60)           # L0
bounds[2,] <- c(0.0005, 0.005)    # bmean
bounds[3,] <- c(0.1, 0.21)        # sd_b
bounds[4,] <- c(0.1, 0.25)        # gamma
bounds[5,] <- c(1.6e-10, 1.7e-10) # psi
bounds[6,] <- c(0.01, 0.1)        # sd_obs
bounds[7,] <- c(6e-06, 6e-01)     # sd_z
bounds[8,] <- c(0.1, 0.5)         # sd_y

# Use the latin hypercube design to create grid of input parameters given the
# specified bounds
Input <- latin.hypercube(n = Ndesign, d = length(names), names = names,
                         normalize = TRUE)
Pars <- array(data = NA, dim = c(Ndesign, Npar, Nsex))
dimnames(Pars) <- list(Simulation = 1:Ndesign, Parameter = names,
                       Sex = c("female", "male"))

# Normalise the cube to between the parameter bounds
for ( II in 1:Npar )
    for ( JJ in 1:Nsex )
        Pars[, II, JJ] <- normalise(v = Input[, II], a = bounds[II, 1],
                                    b = bounds[II, 2])

Input <- vector("list", Ndesign)
for ( I in 1:Ndesign )
{
    Input[[I]]$Parameters <- Pars[I,,]
    Input[[I]]$Data <- Data
}

plot.parameters(Pars[,,1], names)


#=================================================================================
# RUN THE SIMULATION MODEL
#=================================================================================
set.seed(15)
for (ss in 1:Ndesign)
{
    #ATR_sim <- SimGrowth(ln_xdev=NULL, ln_ydev=ln_ydev, obs_err=TRUE, tvi_err=TRUE)$ATR_sim
    ATR_sim <- SimGrowth(ln_xdev = NULL, ln_ydev = NULL,
                         obs_err = TRUE, tvi_err = TRUE,
                         Input[[ss]]$Parameters, Input[[ss]]$Data)$ATR_sim
    #ATR_sim <- SimGrowth(ln_xdev=NULL, ln_ydev=NULL, obs_err=FALSE, tvi_err=TRUE)$ATR_sim
    #ATR_sim <- SimGrowth(ln_xdev=NULL, ln_ydev=NULL, obs_err=TRUE, tvi_err=FALSE)$ATR_sim
    #ATR_sim <- SimGrowth(ln_xdev=NULL, ln_ydev=NULL, obs_err=FALSE, tvi_err=FALSE, Input[[1]]$Parameters, Input[[1]]$Data)$ATR_sim
    sim <- list(Sim = ATR_sim, Parameters = Input[[ss]]$Parameters)
    save(sim, file = paste("sims/sim", ss, ".RData", sep = ""))
    plot.growth(ATR_sim, ss)
}
