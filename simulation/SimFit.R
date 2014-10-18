SimFit <- function(Isim)
{
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
    obj$control <- list(trace = 10)
    obj$hessian <- TRUE
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
    cat("Simulation", Isim, "done\n")
}

#format(object.size(obj), units = "Kb")
#format(object.size(opt), units = "Kb")
#format(object.size(Report), units = "Kb")
