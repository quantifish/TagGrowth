######################################################################################################
# RANDOM EFFECTS MODELLING OF GROWTH USING TAG RECAPTURE DATA
######################################################################################################

# Make sure R is clean
rm(list = ls())

# Load package
require(TagGrowth)

# Versions
# 0. none
# 1. k
# 2. z
# 3. y
# 4. k, z
# 5. k, y
# 6. z, y
# 7. k, z, y
#scenarios <- c("v0/","v1/","v2/","v3/","v4/","v5/","v6/","v7/")
scenarios <- c("v5/")

# Compile the model
compile("../../inst/executables/ATR.cpp")

# Load data
load("../../data/ATR_mod.RData")

# Change to daily/weekly estimates
ATR_mod <- time_step(ATR_mod, units = "weeks")
data <- ATR_mod


# Specify the random-effects we want to try to estimate
for (Iscenario in scenarios)
{
    # Load the model
    dyn.load(dynlib("../../inst/executables/ATR"))

    folder <- Iscenario
    if (Iscenario == "v0/") # none
        Options <- c("YearTF" = 0, "AreaTF" = 0, "IndivTF" = 0, "IndivTimeTF" = 0)
    if (Iscenario == "v1/") # k
        Options <- c("YearTF" = 0, "AreaTF" = 0, "IndivTF" = 1, "IndivTimeTF" = 0)
    if (Iscenario == "v2/") # z
        Options <- c("YearTF" = 0, "AreaTF" = 0, "IndivTF" = 0, "IndivTimeTF" = 1)
    if (Iscenario == "v3/") # y
        Options <- c("YearTF" = 1, "AreaTF" = 0, "IndivTF" = 0, "IndivTimeTF" = 0)
    if (Iscenario == "v4/") # k, z
        Options <- c("YearTF" = 0, "AreaTF" = 0, "IndivTF" = 1, "IndivTimeTF" = 1)
    if (Iscenario == "v5/") # k, y
        Options <- c("YearTF" = 1, "AreaTF" = 0, "IndivTF" = 1, "IndivTimeTF" = 0)
    if (Iscenario == "v6/") # z, y
        Options <- c("YearTF" = 1, "AreaTF" = 0, "IndivTF" = 0, "IndivTimeTF" = 1)
    if (Iscenario == "v7/") # k, z, y
        Options <- c("YearTF" = 1, "AreaTF" = 0, "IndivTF" = 1, "IndivTimeTF" = 1)

    # Dimensions
    Nindiv <- nrow(data)
    Nyears <- 40
    Nareas <- length(unique(data$Area1))

    # Create lists of data and parameters
    Data <- list(Options = Options,
                 iAge1 = data[1:Nindiv,'Age1'], iLiberty = data[1:Nindiv,'Liberty'],
                 Length1 = data[1:Nindiv,'Length1'], Length2 = data[1:Nindiv,'Length2'],
                 Sex = data[1:Nindiv,'Sex'],
                 Time0 = data[1:Nindiv,'Time0'], Time1 = data[1:Nindiv,'Time1'],
                 Year0 = data[1:Nindiv,'Year0'], Year1 = data[1:Nindiv,'Year1'],
                 Area1 = data[1:Nindiv,'Area1'])
    Params <- list(ln_gamma = c(log(0.3), log(0.3)), logit_psi = qlogis(0.000001), L0 = c(0.0, 0.0),
                   ln_bmean = c(log(0.002), log(0.002)), ln_bdev = rep(0, Nindiv), ln_sd_bdev = c(log(0.001), log(0.001)),
                   ln_sd_obs = log(0.102),
                   z1 = rep(0, Nindiv), z2 = rep(0, Nindiv), ln_sd_z = log(0.001),
                   ln_ydev = rep(0, Nyears), ln_sd_ydev = log(0.001),
                   ln_xdev = rep(0, Nareas), ln_sd_xdev = log(0.001))

    # Use TMB's Map option to turn parameters on/off
    Random <- NULL
    Map <- list()
    Map[["logit_psi"]] <- factor(NA)
    if (Options[1] == 0)
    {
        Map[["ln_ydev"]] = factor(rep(NA, Nyears))
        Map[["ln_sd_ydev"]] = factor(NA)
    } else {
        Random = c(Random, "ln_ydev")
    } 
    if (Options[2]==0)
    {
        Map[["ln_sd_xdev"]] = factor(NA)
        Map[["ln_xdev"]] = factor(rep(NA,length(Params$ln_xdev))) 
    } else {
        Random = c(Random, "ln_xdev")
    }
    if (Options[3]==0)
    {
        Map[["ln_bdev"]] = factor(rep(NA,length(Params$ln_bdev)))
        Map[["ln_sd_bdev"]] = factor(rep(NA,2)) 
    } else {
        Random = c(Random, "ln_bdev")
    }
    if (Options[4]==0)
    {
        Map[["z2"]] = factor(rep(NA,length(Params$ln_bdev)))
        Map[["z1"]] = factor(rep(NA,length(Params$z1)))
        Map[["ln_sd_z"]] = factor(NA)  
    } else {
        Random = c(Random, "z1", "z2")
    }

    # Create the AD object
    obj <- MakeADFun(data = Data, parameters = Params, map = Map, random = Random, inner.control=list(maxit=50))

    # List of parameters that are "on"
    names(obj$par)

    # Set up estimation
    newtonOption(smartsearch = TRUE)
    obj$fn(obj$par)
    obj$gr(obj$par)
    obj$hessian <- TRUE
    obj$control <- list(trace=100)
    ConvergeTol <- 1 # 1:Normal; 2:Strong
    #obj$env$inner.control$step.tol <- c(1e-12,1e-15)[ConvergeTol] # Default : 1e-8  # Change in parameters limit inner optimization
    #obj$env$inner.control$tol10 <- c(1e-8,1e-12)[ConvergeTol]  # Default : 1e-3     # Change in pen.like limit inner optimization
    #obj$env$inner.control$grad.tol <- c(1e-12,1e-15)[ConvergeTol] # # Default : 1e-8  # Maximum gradient limit inner optimization
    summary(obj)

    Upr <- rep(Inf, length(obj$par))
    Upr[match("logit_psi",names(obj$par))] = qlogis(0.999)
    Lwr <- rep(-Inf, length(obj$par))
    Lwr[match("logit_psi",names(obj$par))] = qlogis(0.001)
    Lwr[match("ln_sd_z",names(obj$par))] = log(0.001)
    Lwr[match("ln_sd_xdev",names(obj$par))] = log(0.001)
    Lwr[match("ln_sd_bdev",names(obj$par))] = log(0.001)

    # Optimize!
    opt <- nlminb(start = obj$par, objective = obj$fn, upper = Upr, lower = Lwr, control = list(eval.max = 1e4, iter.max = 1e4, rel.tol = c(1e-10, 1e-8)[ConvergeTol], trace = 1))
    opt[["final_gradient"]] <- obj$gr(opt$par)
    Diag <- obj$report()
    Report <- sdreport(obj)

    # Do we need this bit?
    #Hess <- optimHess(par = opt$par, fn = obj$fn)
    #opt <- nlminb(start = opt$par, objective = obj$fn, upper = Upr, lower = Lwr, control = list(eval.max = 1e4, iter.max = 1e4, rel.tol = c(1e-10, 1e-8)[ConvergeTol], trace = 1))

    # Dynamically unload the model
    dyn.unload(dynlib("../../inst/executables/ATR"))

    # Save outputs
    capture.output(Report, file = paste(folder, "Report.txt", sep = ""))
    save(obj, file = paste(folder, "obj.RData", sep = ""))
    save(opt, file = paste(folder, "opt.RData", sep = ""))
    save(Report, file = paste(folder, "Report.RData", sep = ""))
    write.csv(data.frame(names(Report$value), Report$value), file = paste(folder, "Pars.csv", sep = ""), row.names = TRUE)

    # Is the fit positive definite Hessian?
    print(Report$pdHess)
}


######################################################################################################
# Inspect results
######################################################################################################

Delta <- rep(0,length(opt$par))
Delta[2] = 1e-5
(obj$fn(opt$par - Delta/2) - obj$fn(opt$par + Delta/2))  / abs(max(Delta))

# Check none of the parameters are up against the bounds
cbind(Lwr, opt$par, Upr)

REs_b <- Report$par.random[names(Report$par.random) %in% "ln_bdev"]
REs_z1 <- Report$par.random[names(Report$par.random) %in% "z1"]
REs_z2 <- Report$par.random[names(Report$par.random) %in% "z2"]
REs_y <- Report$par.random[names(Report$par.random) %in% "ln_ydev"]
head(Report$value, 15)
t(t(tapply(X = Report$value, INDEX = names(Report$value), FUN = length)))

# END
