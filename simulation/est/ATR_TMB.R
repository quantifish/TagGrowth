#=================================================================================
# RANDOM EFFECTS MODELLING OF GROWTH USING TAG RECAPTURE DATA
#=================================================================================

# Make sure R is clean
rm(list = ls())

# Load TMB
require(TMB)

compile("ATR.cpp")

for (Isim in 48:100)
{
    cat("\nStarting simulation", Isim, "...\n")
    dyn.load(dynlib("ATR"))
    # Data
    fname <- paste("../sims/sim", Isim, ".RData", sep = "")
    load(fname)
    ATR_mod <- sim$Sim
    Nindiv <- nrow(ATR_mod)
    # Make AD object
    Data <- list(iAge1 = ATR_mod[1:Nindiv,'Age1'], iLiberty = ATR_mod[1:Nindiv,'Liberty'],
                 Length1 = ATR_mod[1:Nindiv,'Length1'], Length2 = ATR_mod[1:Nindiv,'Length2'],
                 Sex = ATR_mod[1:Nindiv,'Sex'],
                 Time0 = ATR_mod[1:Nindiv,'Time0'], Time1 = ATR_mod[1:Nindiv,'Time1'],
                 Year0 = ATR_mod[1:Nindiv,'Year0'], Year1 = ATR_mod[1:Nindiv,'Year1'],
                 Area1 = ATR_mod[1:Nindiv,'Area1'])
    Nyears <- 40
    Nareas <- length(unique(ATR_mod$Area1))
    Params <- list(ln_gamma = log(10000), logit_psi = qlogis(0.2), ln_L0 = rep(log(1), 2),
                   ln_bmean = rep(log(0.2), 2), ln_bdev = rep(0, Nindiv), ln_sd_bdev = c(log(0.01), log(0.01)),
                   ln_sd_obs = log(20),
                   z1 = rep(0, Nindiv), z2 = rep(0, Nindiv), ln_sd_z = log(0.1),
                   ln_ydev = rep(0, Nyears), ln_sd_ydev = log(0.01),
                   ln_xdev = rep(0, Nareas), ln_sd_xdev = log(0.01))
    obj <- MakeADFun(data = Data, parameters = Params,
                     map = list(ln_ydev = factor(rep(NA, Nyears)), ln_sd_ydev = factor(NA), ln_xdev = factor(rep(NA, Nareas)), ln_sd_xdev = factor(NA)),
                     random = c("ln_bdev", "z1", "z2"))
    # Run model
    newtonOption(smartsearch = TRUE)
    obj$fn(obj$par)
    obj$gr(obj$par)
    obj$env$tracemgc <- FALSE
    obj$env$inner.control$trace <- FALSE
    obj$env$silent <- TRUE
    obj$hessian <- TRUE
    #obj$env$inner.control$step.tol <- 1e-12 # Default : 1e-8 # Change in parameters limit inner optimization
    #obj$env$inner.control$tol10 <- 1e-8     # Default : 1e-3 # Change in pen.like limit inner optimization
    #obj$env$inner.control$grad.tol <- 1e-12 # Default : 1e-8 # Maximum gradient limit inner optimization
    doFit <- function()
    {
        opt <- nlminb(start = obj$par, objective = obj$fn, control = list(eval.max = 1e4, iter.max = 1e4))
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
    dyn.unload(dynlib("ATR"))
}
