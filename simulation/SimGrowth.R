#' Growth simulation model
#'
#' @param ln_xdev are the deviations for each area
#' @param ln_ydev are the deviations for each year
#' @param obs_err is the observation error
#' @param tvi_err is the time-variying individual error
#' @param Pars the parameters for the model
#' 
SimGrowth <- function(ln_xdev = NULL, ln_ydev = NULL,
                      obs_err = TRUE, tvi_err = TRUE,
                      Pars)
{
    #ln_xdev=NULL; ln_ydev=NULL; obs_err=TRUE; tvi_err=TRUE
    #Pars=Input[[1]]$Parameters
    #Data=Input[[1]]$Data
    # Simulate sex, ages at tagging and time at liberty for each individual
    Sex <- rbinom(Nindiv, 1, 0.5) + 1 # (1=female, 2=male)
    Age1 <- round(rnorm(n = Nindiv, 500, 150), digits = 0)
    Liberty <- round(runif(n = Nindiv, min = 0, max = 466), digits = 0)
    Age2 <- Age1 + Liberty
    Time0 <- rep(1, Nindiv)
    Time1 <- rep(1, Nindiv)
    Time2 <- rep(1, Nindiv)
    Year0 <- rep(1, Nindiv)
    Year1 <- rep(2, Nindiv)
    Year2 <- rep(3, Nindiv)
    Area1 <- rep(1, Nindiv)
    # Parameters
    L0 <- Pars['L0',]
    bmean <- Pars['bmean',]
    sd_b <- Pars['sd_b',]
    gamma <- Pars['gamma',]
    psi <- Pars['psi',]
    sd_obs <- Pars['sd_obs',]
    sd_z <- Pars['sd_z',]
    sd_y <- Pars['sd_y',]
    # Individual vectors
    ln_bdev <- rep(NA, Nindiv)
    Length1 <- Length1_true <- rep(NA, Nindiv)
    Length2 <- Length2_true <- rep(NA, Nindiv)
    sd_z1 <- rep(NA, Nindiv)
    sd_z2 <- rep(NA, Nindiv)
    z1 <- rep(NA, Nindiv)
    z2 <- rep(NA, Nindiv)
    # Check for spatially-explicit or annual random effects
    yrs <- range(Year0, Year1, Year2)
    if ( is.null(ln_xdev) ) ln_xdev <- rep(0, Nareas)
    if ( is.null(ln_ydev) ) ln_ydev <- rep(0, length(yrs[1]:yrs[2]))
    # Cycle through each individual and simulate a growth schedule
    for ( i in 1:Nindiv )
    {
        s <- Sex[i]
        a <- Area1[i]
        ln_bdev[i] <- rnorm(1, 0, sd_b[s])
        b <- bmean[s] * exp(ln_bdev[i])
        #b <- b_indiv[i] # Can use the actual b's estimated in model
        # Time-variation error from birth to first capture
        # First length measurement
        year <- Year0[i] # The (index) year that the individual was born
        #for (j in 0:(Age1[i]-1)) sumj <- sumj + gamma * exp(ln_xdev[a] + ln_ydev[Year0[i]+j+1] - bmean[s]*exp(ln_bdev[i])*j)
        Length1_true[i] <- L0[s]
        for ( j in 0:(Age1[i]-1) )
        {
            if ( !all(ln_ydev == 0) )
            {
                time <- Time0[i] + j
                if ( time %% time_step == 0. ) { year <- year + 1 }
            } else {
                year <- 1
            }
            z_increment <- rnorm(1, 0, sd_z[s])
            Length1_true[i] <- Length1_true[i] * exp(-b) + gamma[s] * exp(ln_ydev[year+1]) * b^(psi[s] - 1) * (1 - exp(-b)) + z_increment
        }
        #Length1[i] <- Length1_hat[i] # Can use the actual first length estimated in the model
        # Add observation error
        if ( obs_err )
        {
            Length1[i] <- Length1_true[i] + rnorm(1, 0, sd_obs[s] * Length1_true[i])
        } else {
            Length1[i] <- Length1_true[i]
        }
        # Time-variation error from first capture to second capture
        # Second length measurement
        year <- Year1[i]; # The (index) year that the individual was born
        sumj <- 0
        #for (j in seq(0,Liberty[i]-1,length=Liberty[i]) ) sumj <- sumj + gamma * exp(ln_xdev[a] + ln_ydev[Year1[i]+j+1] - bmean[s]*exp(ln_bdev[i])*j)
        Length2_true[i] <- Length1_true[i]
        for ( j in seq(0, Liberty[i]-1, length = Liberty[i]) )
        {
            if ( !all(ln_ydev == 0) )
            {
                time <- Time1[i] + j
                if ( time %% time_step == 0. ) { year <- year + 1 }
            } else {
                year <- 1
            }
            z_increment <- rnorm(1, 0, sd_z[s])
            Length2_true[i] <- Length2_true[i] * exp(-b) + gamma[s] * exp(ln_ydev[year+1]) * b^(psi[s] - 1) * (1 - exp(-b)) + z_increment
        }
        #Length2[i] <- Length2_hat[i]
        # Add observation error
        if ( obs_err )
        {
            Length2[i] <- Length2_true[i] + rnorm(1, 0, sd_obs[s] * Length2_true[i])
        } else {
            Length2[i] <- Length2_true[i]
        }
    }
    ATR_sim <- data.frame(Sex, Age1, Age2, Liberty, Length1, Length2, ln_bdev,
                          sd_z1, sd_z2, z1, z2, Year0, Year1, Year2, Time0,
                          Time1, Time2, Area1, Length1_true, Length2_true)
    out <- ATR_sim
    return( out )
}
