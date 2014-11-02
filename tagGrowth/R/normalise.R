#' Function to normalize a vector between two values
#'
#' @param v the vector to be normalised
#' @param a the lower bound to normalise the vector to
#' @param a the upper bound to normalise the vector to
#' @return a vector
#' @export
#' 
normalise <- function (v, a, b)
{
    r1 <- max(v) - min(v)
    # if v is all 0 then return the same
    if (r1 > 0.)
    {
        # normalize between 0,1
        v <- (v - min(v)) / r1
        r2 <- b - a
        # scale to a,b
        v <- (v * r2) + a
    }
    return(v)
}
