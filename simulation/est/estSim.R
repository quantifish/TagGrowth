#=================================================================================
# RANDOM EFFECTS MODELLING OF GROWTH USING TAG RECAPTURE DATA
#=================================================================================

# Make sure R is clean
rm(list = ls())

# Load TMB
require(TMB)

compile("../../estimation/ATR.cpp")

for (Isim in 1:100)
{
    cat("\nStarting simulation", Isim, "...\n")
    dyn.load(dynlib("../../estimation/ATR"))
    # Data
    fname <- paste("../sims/sim", Isim, ".RData", sep = "")
    load(fname)
    sim$obj <- NULL
    sim$opt <- NULL
    sim$Report <- NULL
    save(sim, file = fname)
    ATR_mod <- sim$Sim
    Options <- c("YearTF"=0, "AreaTF"=0, "IndivTF"=0, "IndivTimeTF"=0) #1st slot: 
    Nindiv <- nrow(ATR_mod)
    # Make AD object
    Data <- list(Options=Options, iAge1 = ATR_mod[1:Nindiv,'Age1'], iLiberty = ATR_mod[1:Nindiv,'Liberty'],
             Length1 = ATR_mod[1:Nindiv,'Length1'], Length2 = ATR_mod[1:Nindiv,'Length2'],
             Sex = ATR_mod[1:Nindiv,'Sex'],
             Time0 = ATR_mod[1:Nindiv,'Time0'], Time1 = ATR_mod[1:Nindiv,'Time1'],
             Year0 = ATR_mod[1:Nindiv,'Year0'], Year1 = ATR_mod[1:Nindiv,'Year1'],
             Area1 = ATR_mod[1:Nindiv,'Area1'])
    Nyears <- 40
    Nareas <- length(unique(ATR_mod$Area1))
    Params <- list(ln_gamma = log(10000), logit_psi = qlogis(0.2), ln_L0 = rep(log(1), 2),
               ln_bmean = rep(log(0.2), 2), ln_bdev = rep(0, Nindiv), ln_sd_bdev = c(log(0.01), log(0.01)),
               ln_sd_obs = log(0.099), z1 = rep(0, Nindiv), z2 = rep(0, Nindiv), ln_sd_z = log(0.1),
               ln_ydev = rep(0, Nyears), ln_sd_ydev = log(0.01),
               ln_xdev = rep(0, Nareas), ln_sd_xdev = log(0.01))
    Random <- NULL
    Map <- list()
    Map[["ln_sd_obs"]] <- factor(NA)
    if (Options[1]==0)
    {
        Map[["ln_ydev"]] = factor(rep(NA, Nyears))
        Map[["ln_sd_ydev"]] = factor(NA)
    } else {
        Random = c(Random, "ln_ydev")
    } 
    if(Options[2]==0)
    {
        Map[["ln_sd_xdev"]]=factor(NA)
        Map[["ln_xdev"]]=factor(rep(NA,length(Params$ln_xdev))) 
    } else {
        Random = c(Random, "ln_xdev")
    }
    if(Options[3]==0)
    {
        Map[["ln_bdev"]]=factor(rep(NA,length(Params$ln_bdev)))
        Map[["ln_sd_bdev"]]=factor(rep(NA,2)) 
    } else {
        Random = c(Random, "ln_bdev")
    }
    if(Options[4]==0)
    {
        Map[["z2"]]=factor(rep(NA,length(Params$ln_bdev)))
        Map[["z1"]]=factor(rep(NA,length(Params$z1)))
        Map[["ln_sd_z"]]=factor(NA)  
    } else {
        Random = c(Random, "z1", "z2")
    }
    obj <- MakeADFun(data = Data, parameters = Params, map = Map, random = Random, inner.control=list(maxit=50))
    # Run model
    newtonOption(smartsearch = TRUE)
    obj$fn(obj$par)
    obj$gr(obj$par)
    obj$env$tracemgc <- FALSE
    obj$env$inner.control$trace <- FALSE
    obj$env$silent <- TRUE
    obj$hessian <- TRUE
    ConvergeTol <- 1 # 1:Normal; 2:Strong
    Upr = rep(Inf, length(obj$par))
    Upr[match("logit_psi",names(obj$par))] = qlogis(0.999)
    Lwr = rep(-Inf, length(obj$par))
    Lwr[match("logit_psi",names(obj$par))] = qlogis(0.001)
    Lwr[match("ln_sd_z",names(obj$par))] = log(0.001)
    Lwr[match("ln_sd_xdev",names(obj$par))] = log(0.001)
    Lwr[match("ln_sd_bdev",names(obj$par))] = log(0.001)

    doFit <- function()
    {
        opt <- nlminb(start = obj$par, objective = obj$fn, control = list(eval.max = 1e4, iter.max = 1e4))
        opt[["final_gradient"]] = obj$gr(opt$par)
        Report <- sdreport(obj)
        # Append Report to sim and save as .RData file
        sim$obj <- obj
        sim$opt <- opt
        Report$diag.cov.random <- NULL
        Report$cov <- NULL
        Report$sd <- NULL
        Report$gradient.fixed <- NULL
        Report$cov.fixed <- NULL
        sim$Report <- Report
        save(sim, file = fname)
    }
    tryCatch(doFit(), error = function(e) cat("Error in simulation", Isim, "\n"), finally = cat("Simulation", Isim, "done\n"))
    dyn.unload(dynlib("../../estimation/ATR"))
}

