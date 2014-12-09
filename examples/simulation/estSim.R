#=================================================================================
# RANDOM EFFECTS MODELLING OF GROWTH USING TAG RECAPTURE DATA
#=================================================================================

# Make sure R is clean
rm(list = ls())

# Load tagGrowth
require(TagGrowth)

# File structure
directory <- "v0/"
TmbFile <- paste0(system.file("executables", package = "TagGrowth"), "/")
Version <- "ATR"


for (Isim in 1:100)
{
    cat("\nStarting simulation", Isim, "...\n")
    dyn.load(paste0(TmbFile, dynlib("ATR")))
    
    # Data
    fname <- paste(directory, "sim", Isim, ".RData", sep = "")
    load(fname)
    sim$obj <- NULL
    sim$opt <- NULL
    sim$Report <- NULL
    save(sim, file = fname)

    # Create the AD object
    Options <- c("YearTF" = 0, "AreaTF" = 0, "IndivTF" = 1, "IndivTimeTF" = 1)
    obj <- MakeADGrowth(data = sim$Sim, Options = Options)
    
    doFit <- function()
    {
        opt <- nlminb(start = obj$par, objective = obj$fn, control = list(eval.max = 1e4, iter.max = 1e4))
        opt[["final_gradient"]] <- obj$gr(opt$par)
        Report                 <- sdreport(obj)
        Report$diag.cov.random <- NULL
        Report$cov             <- NULL
        Report$sd              <- NULL
        Report$gradient.fixed  <- NULL
        Report$cov.fixed       <- NULL
        # Append Report to sim and save as .RData file
        sim$obj    <- obj
        sim$opt    <- opt
        sim$Report <- Report
        save(sim, file = fname)
    }
    tryCatch(doFit(), error = function(e) cat("Error in simulation", Isim, "\n"), finally = cat("Simulation", Isim, "done\n"))
    dyn.unload(paste0(TmbFile, dynlib("ATR")))
}

plot_simulations(directory)

# Plot up a specific simulation
Isim = 3
fname <- paste(directory, "sim", Isim, ".RData", sep = "")
load(fname)
load("../../data/ATR_mod.RData")
plot_histogram_b(data = sim$Sim, report = sim$Report, file_name = paste(directory, "results/REs_b_", Isim, sep = ""))
plot_obs_pred(sim$Sim$Sex, sim$Sim$Length1_true, sim$Sim$Length1, sim$Sim$Length2_true, sim$Sim$Length2,
              file_name = paste(directory, "results/ObsVsPred_", Isim, sep = ""))
plot_indiv_growth(sim$Sim$Sex,
                  sim$Sim$Age1, sim$Sim$Length1, sim$Sim$Length1_true,
                  sim$Sim$Age2, sim$Sim$Length2, sim$Sim$Length2_true,
                  file_name = paste(directory, "results/IndivGrowth_", Isim, sep = ""))
