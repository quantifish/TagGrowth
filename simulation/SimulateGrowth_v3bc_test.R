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
load("../data/ATR.RData")
load("../data/ATR_mod.RData")

# Change to daily/weekly estimates
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


######################################################################################################
# TEST THE MODEL
######################################################################################################

ln_xdev = NULL; ln_ydev = NULL; obs_err = TRUE; tvi_err = TRUE
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


# Lets take a look at just one individual
i <- 1

        s <- Sex[i]
        a <- Area[i]
        ln_bdev[i] <- rnorm(n=1, mean=0, sd=sd_b[s])
        b <- bmean[s] * exp(ln_bdev[i])
        # Time-variation error from birth to first capture
        # First length measurement
        year <- Year0[i] # The (index) year that the individual was born
        
NN <- 1000 # Do 1000 runs with this individual
mod_new_1 <- rep(NA, NN)
mod_new_2 <- rep(NA, NN)
mod_old <- rep(NA, NN)

# Here I only take a look at a single individual and only look at the growth from the birth to the first measurement
for (II in 1:NN)
{
        #################### New sim model v3c ####################
        # This is the new version that you proposed Jim - this version has white-noise variation in b
        Length1_true[i] = L0[s]
        for ( j in 0:(Age1[i]-1) )
        {
            b_tmp <- b + rnorm(n=1, mean=0, sd=sd_z) # HERE YOU SPECIFY THE SD OF THE WHITE-NOISE FOR b AS sd_z. IS THIS CORRECT?
            Length1_true[i] = Length1_true[i] * exp(-b_tmp) + gamma*(b_tmp)^(psi-1) * (1-exp(-b_tmp))
        }
        mod_new_1[II] <- Length1_true[i]

        #################### New sim model v3d ####################
        # This is something I tried - the equivalent of eqn 6a-6b but modified to a recursive version (like eqn 4)
        Length1_true[i] = L0[s]
        for ( j in 0:(Age1[i]-1) )
        {
            b_tmp <- b # Notice no white-noise variation on b
            sd_z1[i] <- sd_z * (b^(psi-1) * (1-exp(-b)))
            z1[i] <- rnorm(n=1, mean=0, sd=sd_z1[i]) # These two equations have been modified to a recursive version
            Length1_true[i] = Length1_true[i] * exp(-b_tmp) + gamma*(b_tmp)^(psi-1) * (1-exp(-b_tmp)) + z1[i]
        }
        mod_new_2[II] <- Length1_true[i]

        #################### Old sim model v3b ####################
        # This is the old version that we had before (eqn 6a-6b)
            sumj <- 0.0001
            for (j in 0:(Age1[i]-1)) sumj <- sumj + exp(2.0*-b*j)
            sd_z1[i] <- sd_z * (b^(psi-1) * (1-exp(-b))) * sumj^0.5
            z1[i] <- rnorm(n=1, mean=0, sd=sd_z1[i])
        # First length measurement
        sumj <- 0
        for ( j in 0:(Age1[i]-1) )
        {
            sumj <- sumj + (gamma * exp(-b*j))
        }
        Length1_true[i] <- L0[s] * exp(-b * Age1[i]) + (b)^(psi-1) * (1-exp(-b)) * sumj
        # Add time-varying individual error?
        Length1_true[i] <- Length1_true[i] + z1[i]
        mod_old[II] <- Length1_true[i]
}

plot(density(mod_new_1), type = "l")
lines(density(mod_new_2), col = 2)
lines(density(mod_old), col = 3)

plot(density(mod_new_2), col = 2)
lines(density(mod_old), col = 3)

# END

