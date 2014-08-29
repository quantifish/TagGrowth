######################################################################################################
# SIMULATION TESTING
######################################################################################################

#################
# CHANGES
# 1. Fixed bug: Not necessary to add z1+z2 in simulator for second length 
# 2. Change to recursive simulator for lengths
# 3. Fixed bug: Length2 should be based on true Length1, not observed Length1 
#################

rm(list=ls())

######################################################################################################
# DATA
######################################################################################################

# Load the actual data
load("../estimation/ATR.RData")
load("../estimation/ATR_mod.RData")

# Change to daily/weekly estimates
source("../time-step.R")
#ATR_mod <- time.step(ATR_mod, units = "days")
ATR_mod <- time.step(ATR_mod, units = "weeks")
head(ATR_mod)


######################################################################################################
# PARAMETERS (using estimation model pars)
######################################################################################################
# Define the number of individuals in the simultion
Nindiv <- nrow(ATR_mod) # We begin with the same number of individuals we have
                        # in our actual data set

# Here I am reading in the parameters estimated in our model, we can just swap
# out the parameters for whatever model we are interested in
pars <- read.csv("../estimation/Pars.csv")
load("../estimation/Report.RData") # These two data sets are the same.
names(Report)
Report$par.fixed
unique(names(Report$par.random))

# devs
fit_ln_bdev <- Report$par.random[which(names(Report$par.random) %in% "ln_bdev")]
Length1_hat <- pars[which(pars[,2] %in% "Length1_hat"),3]
Length2_hat <- pars[which(pars[,2] %in% "Length2_hat"),3]

# Sex-specific parameters c("female", "male")
L0 <- pars[which(pars[,2] %in% "L0"),3]
bmean <- exp(Report$par.fixed[which(names(Report$par.fixed) %in% "ln_bmean")])
sd_b <- pars[which(pars[,2] %in% "sd_bdev"),3]
b_indiv <- pars[which(pars[,2] %in% "b_indiv"),3]

# Population-level parameters
gamma <- pars[which(pars[,2] %in% "gamma"),3]
psi <- 1/(1+exp(-Report$par.fixed[which(names(Report$par.fixed) %in% "logit_psi")]))
sd_obs <- pars[which(pars[,2] %in% "sd_obs"),3]
sd_z <- pars[which(pars[,2] %in% "sd_z"),3]
sd_y <- pars[which(pars[,2] %in% "sd_ydev"),3]
Linf <- (gamma*bmean^psi) / bmean
k <- bmean
a <- gamma * bmean^psi

# Simulate sex, ages at tagging and time at liberty
Sex <- ATR_mod$Sex         # Use the same sex as the actual data set
Age1 <- ATR_mod$iAge1      # Use the same Age1 as in actual data set
Liberty <- ATR_mod$iLiberty # Use the same time at liberty as in actual data set
#Sex <- rbinom(n=Nindiv, size=1, prob=0.5) + 1 # (1=female, 2=male)
#Age1 <- as.integer(runif(n=Nindiv, min=4, max=25))
#Liberty <- as.integer(runif(n=Nindiv, min=1, max=8))
Age2 <- ATR_mod$Age2
#Year1 <- as.integer(runif(n=Nindiv, min=2001, max=2008))
#Year2 <- Year1+Liberty
Year0 <- ATR_mod$Year0
Year1 <- ATR_mod$Year1
Year2 <- ATR_mod$Year2
Area <- ATR_mod$Area1
Time0 <- ATR_mod$Time0
Time1 <- ATR_mod$Time1
Time2 <- ATR_mod$Time2

# Annual effects
yrs <- range(Year0, Year1, Year2)
#ln_ydev <- rnorm(n=length(yrs[1]:yrs[2]), mean=0, sd=sd_y)
ln_ydev <- Report$par.random[which(names(Report$par.random) %in% "ln_ydev")]
time_step <- 52

# Area effects
Nareas <- length(unique(ATR_mod$Area1))
#ln_xdev <- rnorm(n=Nareas, mean=0, sd=0)


######################################################################################################
# THE SIMULATION MODEL
######################################################################################################

SimGrowth <- function(ln_xdev = NULL, ln_ydev = NULL, obs_err = TRUE, tvi_err = TRUE)
{
    # ln_xdev are the deviations for each area
    # ln_ydev are the deviations for each year
    # obs_err is the observation error
    # tvi_err is the time-variying individual error
    # Individual vectors
    ln_bdev <- rep(NA, Nindiv)
    Length1 <- Length1_true <- rep(NA, Nindiv)
    Length2 <- Length2_true <- rep(NA, Nindiv)
    sd_z1 <- rep(NA, Nindiv)
    sd_z2 <- rep(NA, Nindiv)
    z1 <- rep(NA, Nindiv)
    z2 <- rep(NA, Nindiv)
    # Check for spatially-explicit or annual random effects
    if (is.null(ln_xdev)) ln_xdev <- rep(0, Nareas)
    if (is.null(ln_ydev)) ln_ydev <- rep(0, length(yrs[1]:yrs[2]))
    # Cycle through each individual and simulate a growth schedule
    for (i in 1:Nindiv)
    {
        s <- Sex[i]
        a <- Area[i]
        ln_bdev[i] <- rnorm(n=1, mean=0, sd=sd_b[s])
        b <- bmean[s] * exp(ln_bdev[i])
        #b <- b_indiv[i] # Can use the actual b's estimated in model
        # Time-variation error from birth to first capture
        # First length measurement
        year <- Year0[i] # The (index) year that the individual was born
        #for (j in 0:(Age1[i]-1)) sumj <- sumj + gamma * exp(ln_xdev[a] + ln_ydev[Year0[i]+j+1] - bmean[s]*exp(ln_bdev[i])*j)
        Length1_true[i] = L0[s]
        for ( j in 0:(Age1[i]-1) )
        {
            if (!all(ln_ydev == 0))
            {
                time <- Time0[i] + j
                if ( time %% time_step == 0. ) { year <- year + 1 }
            } else {
                year <- 1
            }
            b_tmp <- b
            z_increment <- rnorm(1, 0, sd_z)
            Length1_true[i] = Length1_true[i] * exp(-b_tmp) + gamma*exp(ln_ydev[year+1])*(b_tmp)^(psi-1) * (1-exp(-b_tmp)) + z_increment
        }
        #Length1[i] <- Length1_hat[i] # Can use the actual first length estimated in the model
        # Add observation error?
        if (obs_err) Length1[i] <- Length1_true[i] + rnorm(n=1, mean=0, sd=sd_obs * Length1_true[i])
        # Time-variation error from first capture to second capture
        # Second length measurement
        year <- Year1[i]; # The (index) year that the individual was born
        sumj <- 0
        #for (j in seq(0,Liberty[i]-1,length=Liberty[i]) ) sumj <- sumj + gamma * exp(ln_xdev[a] + ln_ydev[Year1[i]+j+1] - bmean[s]*exp(ln_bdev[i])*j)
        Length2_true[i] = Length1_true[i]
        for ( j in seq(0, Liberty[i]-1, length = Liberty[i]) )
        {
            if (!all(ln_ydev == 0))
            {
                time <- Time1[i] + j
                if ( time %% time_step == 0. ) { year <- year + 1 }
            } else {
                year <- 1
            }
            b_tmp <- b
            z_increment <- rnorm(1, 0, sd_z)
            Length2_true[i] = Length2_true[i] * exp(-b_tmp) + gamma*exp(ln_ydev[year+1])*(b_tmp)^(psi-1) * (1-exp(-b_tmp)) + z_increment
        }
        #Length2[i] <- Length2_hat[i]
        # Add observation error?
        if (obs_err) Length2[i] <- Length2_true[i] + rnorm(n=1, mean=0, sd=sd_obs * Length2_true[i])
    }
    ATR_sim <- data.frame(Sex, Age1, Age2, Liberty, Length1, Length2, ln_bdev, sd_z1, sd_z2, z1, z2,
                          Year0, Year1, Year2, Time0, Time1, Time2, Area, Length1_true, Length2_true)
    out <- list(ATR_sim = ATR_sim)
    return(out)
}

set.seed(15)
#ATR_sim <- SimGrowth(ln_xdev=NULL, ln_ydev=ln_ydev, obs_err=TRUE, tvi_err=TRUE)$ATR_sim
ATR_sim <- SimGrowth(ln_xdev=NULL, ln_ydev=NULL, obs_err=TRUE, tvi_err=TRUE)$ATR_sim
#ATR_sim <- SimGrowth(ln_xdev=NULL, ln_ydev=NULL, obs_err=FALSE, tvi_err=TRUE)$ATR_sim
#ATR_sim <- SimGrowth(ln_xdev=NULL, ln_ydev=NULL, obs_err=TRUE, tvi_err=FALSE)$ATR_sim
#ATR_sim <- SimGrowth(ln_xdev=NULL, ln_ydev=NULL, obs_err=FALSE, tvi_err=FALSE)$ATR_sim
#save(ATR_sim, file="ATR_sim.RData")
#load("ATR_sim.RData")

par(mfrow=c(1,1))
xlim <- c(0, max(ATR_mod$iAge2, ATR_sim$Age2))
ylim <- c(0, 200)
plot(1, type="n", xlim=xlim, ylim=ylim, xlab="Age", ylab="Length (cm)", las=1)
legend("bottomright", legend=c("Females","Males"), col=c("pink","blue"), lwd=1, bty="n")
for (II in 1:100)
{
    set.seed(15 + (II-1))
    ATR_sim <- SimGrowth(ln_xdev=NULL, ln_ydev=ln_ydev, obs_err=TRUE, tvi_err=TRUE)$ATR_sim
    segments(x0=ATR_sim$Age1[ATR_sim$Sex==1], x1=ATR_sim$Age2[ATR_sim$Sex==1], y0=ATR_sim$Length1[ATR_sim$Sex==1], y1=ATR_sim$Length2[ATR_sim$Sex==1], col="pink")
    segments(x0=ATR_sim$Age1[ATR_sim$Sex==2], x1=ATR_sim$Age2[ATR_sim$Sex==2], y0=ATR_sim$Length1[ATR_sim$Sex==2], y1=ATR_sim$Length2[ATR_sim$Sex==2], col="blue")
}

# Plot the growth schedules
par(mfrow=c(1,2))
xlim <- c(0, max(ATR_mod$iAge2, ATR_sim$Age2))
ylim <- c(0, max(ATR_mod$Length2, ATR_sim$Length2))
plot(1, type="n", xlim=xlim, ylim=ylim, xlab="Age", ylab="Length (cm)", las=1)
segments(x0=ATR_mod$iAge1[ATR_mod$Sex==1], x1=ATR_mod$Age2[ATR_mod$Sex==1], y0=ATR_mod$Length1[ATR_mod$Sex==1], y1=ATR_mod$Length2[ATR_mod$Sex==1])
segments(x0=Age1[Sex==1], x1=Age2[Sex==1], y0=Length1_hat[Sex==1], y1=Length2_hat[Sex==1], col=3)
segments(x0=ATR_sim$Age1[ATR_sim$Sex==1], x1=ATR_sim$Age2[ATR_sim$Sex==1], y0=ATR_sim$Length1[ATR_sim$Sex==1], y1=ATR_sim$Length2[ATR_sim$Sex==1], col=2)
title("Females")
legend("bottomright", legend=c("Observed","Model fit","Simulated"), col=c(1,3,2), lwd=1, bty="n")
plot(1, type="n", xlim=xlim, ylim=ylim, xlab="Age", ylab="Length (cm)", las=1)
segments(x0=ATR_mod$iAge1[ATR_mod$Sex==2], x1=ATR_mod$Age2[ATR_mod$Sex==2], y0=ATR_mod$Length1[ATR_mod$Sex==2], y1=ATR_mod$Length2[ATR_mod$Sex==2])
segments(x0=ATR_sim$Age1[ATR_sim$Sex==2], x1=ATR_sim$Age2[ATR_sim$Sex==2], y0=Length1_hat[ATR_sim$Sex==2], y1=Length2_hat[ATR_sim$Sex==2], col=3)
segments(x0=ATR_sim$Age1[ATR_sim$Sex==2], x1=ATR_sim$Age2[ATR_sim$Sex==2], y0=ATR_sim$Length1[ATR_sim$Sex==2], y1=ATR_sim$Length2[ATR_sim$Sex==2], col=2)
title("Males")
legend("bottomright", legend=c("Observed","Model fit","Simulated"), col=c(1,3,2), lwd=1, bty="n")

par(mfrow=c(1,1))
ylim <- c(0, 200)
plot(1, type="n", xlim=xlim, ylim=ylim, xlab="Age", ylab="Length (cm)", las=1)
segments(x0=ATR_sim$Age1[ATR_sim$Sex==1], x1=ATR_sim$Age2[ATR_sim$Sex==1], y0=ATR_sim$Length1[ATR_sim$Sex==1], y1=ATR_sim$Length2[ATR_sim$Sex==1], col="pink")
segments(x0=ATR_sim$Age1[ATR_sim$Sex==2], x1=ATR_sim$Age2[ATR_sim$Sex==2], y0=ATR_sim$Length1[ATR_sim$Sex==2], y1=ATR_sim$Length2[ATR_sim$Sex==2], col="blue")
legend("bottomright", legend=c("Females","Males"), col=c("pink","blue"), lwd=1, bty="n")


# Plot observed vs. predicted length at age
#png("ObsVsPred.png", width=5, height=5, units="in", res=300)
#par(mfrow=c(1,2))
#plot( x=ATR_sim$Length1[1:Nindiv], y=ATR_mod$Length1[1:Nindiv], xlab="Simulated length", ylab="Observed length")
#abline(0,1, col=2)
#plot( x=ATR_sim$Length2[1:Nindiv], y=ATR_mod$Length2[1:Nindiv], xlab="Simulated length", ylab="Observed length")
#abline(0,1, col=2)
#dev.off()


# Plot histogram of b REs
par(mfrow=c(2,1))
hist(exp(ATR_sim$ln_bdev[ATR_sim$Sex==1]), xlab="b", freq=FALSE, las=1, ylim=c(0,2.6), main="Females")
lines(density(exp(ATR_sim$ln_bdev[ATR_sim$Sex==1])), col=2, lwd=2)
lines(density(exp(fit_ln_bdev[ATR_mod$Sex==1])), col=3, lwd=2)
legend("topright", legend=c("Model fit","Simulated"), col=c(3,2), lwd=1, bty="n")
box()
hist(exp(ATR_sim$ln_bdev[ATR_sim$Sex==2]), xlab="b", freq=FALSE, las=1, ylim=c(0,2.6), main="Males")
lines(density(exp(ATR_sim$ln_bdev[ATR_sim$Sex==2])), col=2, lwd=2)
lines(density(exp(fit_ln_bdev[ATR_mod$Sex==2])), col=3, lwd=2)
legend("topright", legend=c("Model fit","Simulated"), col=c(3,2), lwd=1, bty="n")
box()


# Plot histograms of z REs
par(mfrow=c(2,2))
hist(ATR_sim$sd_z1, xlab="sd_z1", freq=FALSE)
lines(density(ATR_sim$sd_z1), col=2, lwd=2)
lines(density(ATR_$sd_z1), col=2, lwd=2)

hist(ATR_sim$sd_z2, xlab="sd_z2", freq=FALSE)
lines(density(ATR_sim$sd_z2), col=2, lwd=2)

hist(ATR_sim$z1, xlab="z1", freq=FALSE)
lines(density(ATR_sim$z1), col=2, lwd=2)

hist(ATR_sim$z2, xlab="z2", freq=FALSE)
lines(density(ATR_sim$z2), col=2, lwd=2)


###################
# RANDOM THOUGHTS #
###################

# 1. L0 seems to be very high when estimated?  Should look into this.  Could
# put a prior penalty on it.
# 5. Plot distributions of stuff to see how they compare with the real data (i.e. b, z)
# 6. Check it out when I don't include obs_error in the simulation -- this is consistent
# with the poor model fits to smaller indivs in the analysis of the real data.  Why are we
# these small indivs so poorly???  Perhaps a prior on L0 would tidy this up?






######################################################################################################
# PARAMETERS (made up)
######################################################################################################
# Define the number of individuals in the simultion
Nindiv <- 10

# Sex-specific parameters c("female", "male")
L0 <- c(50, 52)
bmean <- c(0.00074, 0.00077)
sd_b <- c(0.24, 0.22)
#b_indiv <- pars[which(pars[,2] %in% "b_indiv"),3]

# Population-level parameters
gamma <- 0.14
psi <- 2.07e-10
sd_obs <- 0.077
sd_z <- 5.3e-05
sd_y <- 0.4
Linf <- (gamma*bmean^psi) / bmean

# Simulate sex, ages at tagging and time at liberty
Sex <- rbinom(n=Nindiv, size=1, prob=0.5) + 1 # (1=female, 2=male)
#Age1 <- as.integer(runif(n=Nindiv, min=4, max=25))
#Liberty <- as.integer(runif(n=Nindiv, min=1, max=8))
Age1 <- sample(ATR_mod$iAge1, size = Nindiv, replace = TRUE)
Liberty <- sample(ATR_mod$iLiberty, size = Nindiv, replace = TRUE)
Age2 <- Age1 + Liberty
#Year1 <- as.integer(runif(n=Nindiv, min=2001, max=2008))
#Year2 <- Year1+Liberty
Year0 <- sample(ATR_mod$Year0, size = Nindiv, replace = TRUE)
Year1 <- Year0 + sample(ATR_mod$Year1 - ATR_mod$Year0, size = Nindiv, replace = TRUE)
Year2 <- Year1 + sample(ATR_mod$Year2 - ATR_mod$Year1, size = Nindiv, replace = TRUE)
Area <- sample(ATR_mod$Area1, size = Nindiv, replace = TRUE)
Time0 <- sample(ATR_mod$Time0, size = Nindiv, replace = TRUE)
Time1 <- sample(ATR_mod$Time1, size = Nindiv, replace = TRUE) # WRONG
Time2 <- sample(ATR_mod$Time2, size = Nindiv, replace = TRUE) # WRONG

# Annual effects
yrs <- range(Year0, Year1, Year2)
#ln_ydev <- rnorm(n=length(yrs[1]:yrs[2]), mean=0, sd=sd_y)
ln_ydev <- Report$par.random[which(names(Report$par.random) %in% "ln_ydev")]
time_step <- 52

# Area effects
Nareas <- length(unique(ATR_mod$Area1))
#ln_xdev <- rnorm(n=Nareas, mean=0, sd=0)
