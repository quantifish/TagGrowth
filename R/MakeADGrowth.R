#' Function to create the AD object
#'
#' @param data the data
#' @param Options the options
#' @return an AD object
#' @export
#' 
MakeADGrowth <- function(data, Options)
{
    Nindiv <- nrow(data)
    # Make AD object
    Data <- list(Options = Options,
                 iAge1 = round(data[1:Nindiv,'Age1']), iLiberty = round(data[1:Nindiv,'Liberty']),
                 Length1 = data[1:Nindiv,'Length1'], Length2 = data[1:Nindiv,'Length2'],
                 Sex = data[1:Nindiv,'Sex'],
                 Time0 = data[1:Nindiv,'Time0'], Time1 = data[1:Nindiv,'Time1'],
                 Year0 = data[1:Nindiv,'Year0'], Year1 = data[1:Nindiv,'Year1'],
                 Area1 = data[1:Nindiv,'Area1'])
    Nyears <- 40
    Nareas <- length(unique(data$Area1))
    Params <- list(ln_gamma = c(log(0.3), log(0.3)),
                   logit_psi = qlogis(0.000001), L0 = c(0.0, 0.0),
                   ln_bmean = c(log(0.002), log(0.002)), ln_bdev = rep(0, Nindiv), ln_sd_bdev = c(log(0.001), log(0.001)),
                   ln_sd_obs = log(0.102),
                   z1 = rep(0, Nindiv), z2 = rep(0, Nindiv), ln_sd_z = log(0.001),
                   ln_ydev = rep(0, Nyears), ln_sd_ydev = log(0.001),
                   ln_xdev = rep(0, Nareas), ln_sd_xdev = log(0.001))
    Random <- NULL
    Map <- list()
    # We have fixed psi at 0 (or close enough)
    Map[["logit_psi"]] <- factor(NA)
    if (Options[1] == 0)
    {
        Map[["ln_ydev"]]    = factor(rep(NA, Nyears))
        Map[["ln_sd_ydev"]] = factor(NA)
    } else {
        Random = c(Random, "ln_ydev")
    } 
    if (Options[2] == 0)
    {
        Map[["ln_sd_xdev"]] = factor(NA)
        Map[["ln_xdev"]]    = factor(rep(NA,length(Params$ln_xdev))) 
    } else {
        Random = c(Random, "ln_xdev")
    }
    if (Options[3] == 0)
    {
        Map[["ln_bdev"]]    = factor(rep(NA,length(Params$ln_bdev)))
        Map[["ln_sd_bdev"]] = factor(rep(NA,2)) 
    } else {
        Random = c(Random, "ln_bdev")
    }
    if (Options[4] == 0)
    {
        Map[["z2"]]      = factor(rep(NA,length(Params$ln_bdev)))
        Map[["z1"]]      = factor(rep(NA,length(Params$z1)))
        Map[["ln_sd_z"]] = factor(NA)  
    } else {
        Random = c(Random, "z1", "z2")
    }
    obj <- MakeADFun(data = Data, parameters = Params, map = Map, random = Random, inner.control=list(maxit=50))
    # Run model
    newtonOption(smartsearch = TRUE)
    obj$fn(obj$par)
    obj$gr(obj$par)
    obj$env$tracemgc            <- FALSE
    obj$env$inner.control$trace <- FALSE
    obj$env$silent              <- TRUE
    obj$hessian                 <- TRUE
    #ConvergeTol                 <- 1 # 1:Normal; 2:Strong
    #Upr <- rep( Inf, length(obj$par))
    #Lwr <- rep(-Inf, length(obj$par))
    #Upr[match("logit_psi",  names(obj$par))] = qlogis(0.999999)
    #Lwr[match("logit_psi",  names(obj$par))] = qlogis(0.000001)
    #Lwr[match("ln_sd_z",    names(obj$par))] = log(0.001)
    #Lwr[match("ln_sd_xdev", names(obj$par))] = log(0.001)
    #Lwr[match("ln_sd_bdev", names(obj$par))] = log(0.001)
    return(obj)
}
