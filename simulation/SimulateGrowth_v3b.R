######################################################################################################
# SIMULATION TESTING
######################################################################################################

#################
# CHANGES
# 1. Fixed bug: Not necessary to add z1+z2 in simulator for second length 
# 3. Fixed bug: Length2 should be based on true Length1, not observed Length1
# 4. Fixed bug: Found an instance of k = bmean / 3, changed to k = bmean
#################
rm(list=ls())

######################################################################################################
# DATA
######################################################################################################

time.step <- function(ATR_mod, units = "weeks")
{
    if (units == "days") { mult <- 365.25 }
    if (units == "weeks") { mult <- 52.15 }
    ATR_mod$Age2 <- ATR_mod$Age2 * mult
    ATR_mod$Age1 <- as.numeric(ATR_mod$Age2 - difftime(ATR_mod$Date2, ATR_mod$Date1, units = units))
    ATR_mod$iAge1 <- round(ATR_mod$Age1)
    ATR_mod$Date0 <- ATR_mod$Date2 - as.difftime(ATR_mod$Age2, unit = units)
    ATR_mod$iLiberty <- round(as.numeric(difftime(ATR_mod$Date2, ATR_mod$Date1, units = units)))
    # We define a year as 1 Nov. to 31 Oct.
    ATR_mod$Year0 <- rep(NA, nrow(ATR_mod))
    ATR_mod$Year1 <- rep(NA, nrow(ATR_mod))
    ATR_mod$Year2 <- rep(NA, nrow(ATR_mod))
    year0 <- 1972
    for (i in 0:100)
    {
        yr <- year0 + i
        y1 <- strptime(paste(yr, "-11-01", sep = ""), format = "%Y-%m-%d")
        y2 <- strptime(paste(yr+1, "-10-31", sep = ""), format = "%Y-%m-%d")
        ind <- which(ATR_mod$Date0 >= y1 & ATR_mod$Date0 < y2)
        ATR_mod$Year0[ind] <- i
        ind <- which(ATR_mod$Date1 >= y1 & ATR_mod$Date1 < y2)
        ATR_mod$Year1[ind] <- i
        ind <- which(ATR_mod$Date2 >= y1 & ATR_mod$Date2 < y2)
        ATR_mod$Year2[ind] <- i
    }
    # Here we identify the number of days since day0 until the birth of each
    # individual (Time0), the capture of each individual (Time1), and the
    # recapture of each individual (Time2).  day0 is defined as the start of
    # the year in which the first individual was born.
    min(ATR_mod$Date0) # = "1973-01-09 23:00:00 NZST", therefore day 0 is to be
                       # 1972-08-01.  This means our first year is 1 Aug 1972
                       # to 31 July 1973.    
    max(ATR_mod$Date2) # = "2012-01-28 NZDT", therefore our final year is 1 Aug
                       # 2011 to 31 July 2012.
    day0 <- strptime("1972-08-01", format = "%Y-%m-%d")
    ATR_mod$Time0 <- round(as.numeric(difftime(ATR_mod$Date0, day0, units = units)))
    ATR_mod$Time1 <- ATR_mod$Time0 + ATR_mod$iAge1
    ATR_mod$Time2 <- ATR_mod$Time1 + ATR_mod$iLiberty
    return(ATR_mod)
}


# Load the actual data
load("../estimation/ATR.RData")
load("../estimation/ATR_mod.RData")

# Change to daily/weekly estimates
#ATR_mod <- time.step(ATR_mod, units = "days")
ATR_mod <- time.step(ATR_mod, units = "weeks")
head(ATR_mod)


######################################################################################################
# PARAMETERS
######################################################################################################
# Define the number of individuals in the simultion
Nindiv <- nrow(ATR_mod) # We begin with the same number of individuals we have
                        # in our actual data set

# Here I am reading in the parameters estimated in our model, we can just swap
# out the parameters for whatever model we are interested in
pars <- read.csv("Pars.csv")
load("Report.RData") # These two data sets are the same.

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
        if (tvi_err)
        {
            sumj <- 0.0001
            for (j in 0:(Age1[i]-1)) sumj <- sumj + exp(2.0*-b*j)
            sd_z1[i] <- sd_z * (b^(psi-1) * (1-exp(-b))) * sumj^0.5
            z1[i] <- rnorm(n=1, mean=0, sd=sd_z1[i])
        }
        # First length measurement
        year <- Year0[i]; # The (index) year that the individual was born
        sumj <- 0
        #for (j in 0:(Age1[i]-1)) sumj <- sumj + gamma * exp(ln_xdev[a] + ln_ydev[Year0[i]+j+1] - bmean[s]*exp(ln_bdev[i])*j)
        for ( j in 0:(Age1[i]-1) )
        {
            time <- Time0[i] + j
            if ( time %% time_step == 0. ) { year = year + 1 }
            sumj <- sumj + (gamma * exp(ln_ydev[year+1] - b*j))
        }
        Length1_true[i] <- L0[s] * exp(-b * Age1[i]) + (b)^(psi-1) * (1-exp(-b)) * sumj
        #Length1[i] <- Length1_hat[i] # Can use the actual first length estimated in the model
        # Add time-varying individual error?
        if (tvi_err) Length1_true[i] <- Length1_true[i] + z1[i]
        # Add observation error?
        if (obs_err) Length1[i] <- Length1_true[i] + rnorm(n=1, mean=0, sd=sd_obs * Length1_true[i])
        # Time-variation error from first capture to second capture
        if (tvi_err)
        {
            sumj <- 0.0001
            for (j in seq(0,Liberty[i]-1,length=Liberty[i]) ) sumj <- sumj + exp(2.0*-b*j)
            sd_z2[i] <- sd_z * ((b)^(psi-1) * (1-exp(-b))) * sumj^0.5
            z2[i] <- rnorm(n=1, mean=0, sd=sd_z2[i])
        }
        # Second length measurement
        year <- Year1[i]; # The (index) year that the individual was born
        sumj <- 0
        #for (j in seq(0,Liberty[i]-1,length=Liberty[i]) ) sumj <- sumj + gamma * exp(ln_xdev[a] + ln_ydev[Year1[i]+j+1] - bmean[s]*exp(ln_bdev[i])*j)
        for ( j in seq(0, Liberty[i]-1, length = Liberty[i]) )
        {
            time <- Time1[i] + j
            if ( time %% time_step == 0. ) { year = year + 1 }
            sumj <- sumj + (gamma * exp(ln_ydev[year+1] - b*j))
        }
        Length2_true[i] <- ( Length1_true[i] * exp(-b * Liberty[i]) ) + ( (b^(psi-1) * (1-exp(-b))) * sumj )
        #Length2[i] <- Length2_hat[i]
        # Add time-varying individual error?
        if (tvi_err) Length2_true[i] <- Length2_true[i] + z2[i] # NOT NECESSARY TO ADD z1 and z2
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
ylim <- c(0, 200)
plot(1, type="n", xlim=xlim, ylim=ylim, xlab="Age", ylab="Length (cm)", las=1)
legend("bottomright", legend=c("Females","Males"), col=c("pink","blue"), lwd=1, bty="n")
for (II in 1:100)
{
    set.seed(15 + (II-1))
    ATR_sim <- SimGrowth(ln_xdev=NULL, ln_ydev=ln_ydev, obs_err=TRUE, tvi_err=TRUE)$ATR_sim
    #segments(x0=ATR_sim$Age1[ATR_sim$Sex==1], x1=ATR_sim$Age2[ATR_sim$Sex==1], y0=ATR_sim$Length1[ATR_sim$Sex==1], y1=ATR_sim$Length2[ATR_sim$Sex==1], col="pink")
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

dyn.load(dynlib(file))
Nindiv <- nrow(ATR_mod)
Data <- list(iAge1 = ATR_sim[1:Nindiv,'Age1'], iLiberty = ATR_sim[1:Nindiv,'Liberty'],
             Length1 = ATR_sim[1:Nindiv,'Length1'], Length2 = ATR_sim[1:Nindiv,'Length2'],
             Sex = ATR_sim[1:Nindiv,'Sex'],
             Time0 = ATR_sim[1:Nindiv,'Time0'], Time1 = ATR_sim[1:Nindiv,'Time1'],
             Year0 = ATR_sim[1:Nindiv,'Year0'],
             Year1 = ATR_sim[1:Nindiv,'Year1'],
             Area1 = ATR_sim[1:Nindiv,'Area'] )

# No year-effects or area-effects
Params <- list(ln_gamma=log(10000), logit_psi=qlogis(0.2), ln_L0=rep(log(1),2),
               ln_bmean=rep(log(0.2),2), ln_bdev=rep(0,Nindiv), ln_sd_bdev=c(log(0.01),log(0.01)),
               ln_sd_obs=log(20),
               z1=rep(0,Nindiv), z2=rep(0,Nindiv), ln_sd_z=log(0.1))
obj <- MakeADFun(data = Data, parameters = Params, random = c("ln_bdev", "z1", "z2"))

# With year-effects
Nyears <- 40
Params <- list(ln_gamma=log(10000), logit_psi=qlogis(0.2), ln_L0=rep(log(1),2),
               ln_bmean=rep(log(0.2),2), ln_bdev=rep(0,Nindiv), ln_sd_bdev=c(log(0.01),log(0.01)),
               ln_sd_obs=log(20),
               z1=rep(0,Nindiv), z2=rep(0,Nindiv), ln_sd_z=log(0.1),
               ln_ydev=rep(0,Nyears), ln_sd_ydev=log(0.01))
obj <- MakeADFun(data = Data, parameters = Params, random = c("ln_bdev", "z1", "z2", "ln_ydev"))

# With area-effects
Nareas <- length(unique(ATR_mod$Area1))
Params <- list(ln_gamma=log(10000), logit_psi=qlogis(0.2), ln_L0=rep(log(1),2),
               ln_bmean=rep(log(0.2),2), ln_bdev=rep(0,Nindiv), ln_sd_bdev=c(log(0.01),log(0.01)),
               ln_sd_obs=log(20),
               z1=rep(0,Nindiv), z2=rep(0,Nindiv), ln_sd_z=log(0.1),
               ln_xdev=rep(0,Nareas), ln_sd_xdev=log(0.01))
obj <- MakeADFun(data = Data, parameters = Params, random = c("ln_bdev", "z1", "z2", "ln_xdev"))


######################################################################################################
# Run model
######################################################################################################
newtonOption(smartsearch=TRUE)
obj$fn(obj$par)
obj$gr(obj$par)
obj$control <- list(trace=10)
obj$hessian <- TRUE

ptm <- proc.time()
opt <- nlminb(start = obj$par, objective = obj$fn, control = list(eval.max = 1e4, iter.max = 1e4))
summary(obj)
Report <- sdreport(obj)
proc.time() - ptm
