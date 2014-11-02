difftime <- function (time1, time2, tz, units = c("auto", "secs", "mins", 
                      "hours", "days", "weeks", "months")) 
{
    if (missing(tz)) {
        time1 <- as.POSIXct(time1)
        time2 <- as.POSIXct(time2)
    }
    else {
        time1 <- as.POSIXct(time1, tz = tz)
        time2 <- as.POSIXct(time2, tz = tz)
    }
    z <- unclass(time1) - unclass(time2)
    attr(z, "tzone") <- NULL
    units <- match.arg(units)
    if (units == "auto") {
        if (all(is.na(z))) 
            units <- "secs"
        else {
            zz <- min(abs(z), na.rm = TRUE)
            if (is.na(zz) || zz < 60) 
                units <- "secs"
            else if (zz < 3600) 
                units <- "mins"
            else if (zz < 86400) 
                units <- "hours"
            else units <- "days"
        }
    }
    switch(units, secs = .difftime(z, units = "secs"), mins = .difftime(z/60, 
        units = "mins"), hours = .difftime(z/3600, units = "hours"), 
        days = .difftime(z/86400, units = "days"), weeks = .difftime(z/(7 * 
            86400), units = "weeks"), months = .difftime(z/(52.15 * 7 * 86400), units = "months"))
}
