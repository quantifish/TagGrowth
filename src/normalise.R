# Function to normalize a vector v between two values [a,b] given the maximum
# and minimum possible values of v.  This is used to convert to/from the
# parameter space used in the emulator to the parameter space used in the
# model/simulator.
normalise <- function (v, a = -1, b = 1, vmin = NULL, vmax = NULL)
{
    if (is.null(vmin)) vmin <- min(v)
    if (is.null(vmax)) vmax <- max(v)
    v <- c(vmin, vmax, v)
    r1 <- max(v) - min(v)
    if (r1 > 0) # if v is all 0 then return the same
    {
        v <- (v - min(v)) / r1 # normalize between [0,1]
        r2 <- b - a
        v <- (v * r2) + a # scale to [a,b]
    }
    v <- v[3:length(v)] # remove vmax and vmin
    if (vmin == 0 && vmax == 0) v <- rep(0, length(v))
    return(v)
}
