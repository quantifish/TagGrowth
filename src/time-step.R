#' Function for converting data.frame from annual to daily/weekly time steps.
#'
#' @author Darcy Webber, Jim Thorson
#' @param units choose between weeks or days
#' 
time.step <- function(ATR_mod, units = "weeks", year0 = 1972)
{
    # There are 365.25 days in a year or 52.15 weeks in a year
    if (units == "days") mult <- 365.25
    if (units == "weeks") mult <- 52.15
    ATR_mod$Age2 <- ATR_mod$Age2 * mult
    ATR_mod$Age1 <- as.numeric(ATR_mod$Age2 - difftime(ATR_mod$Date2, ATR_mod$Date1, units = units))
    ATR_mod$iAge1 <- round(ATR_mod$Age1)
    ATR_mod$Date0 <- ATR_mod$Date2 - as.difftime(ATR_mod$Age2, unit = units)
    ATR_mod$Liberty <- as.numeric(difftime(ATR_mod$Date2, ATR_mod$Date1, units = units))
    ATR_mod$iLiberty <- round(as.numeric(difftime(ATR_mod$Date2, ATR_mod$Date1, units = units)))
    # We define a year as 1 Nov. to 31 Oct.
    ATR_mod$Year0 <- rep(NA, nrow(ATR_mod))
    ATR_mod$Year1 <- rep(NA, nrow(ATR_mod))
    ATR_mod$Year2 <- rep(NA, nrow(ATR_mod))
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
