#=================================================================================
# RANDOM EFFECTS MODELLING OF GROWTH USING TAG RECAPTURE DATA
#=================================================================================

# Make sure R is clean
rm(list = ls())

#=================================================================================
# USER SPECIFICATIONS
# =================================================================================
#scenarios <- c("v0/","v1/","v2/","v3/")
scenarios <- c("v3/")
#power <- c(50, 100, 250, 500) # Power analysis
power <- c(500) # Power analysis
Ndesign <- 200

#=================================================================================
# SET UP MODEL FITS
# =================================================================================
# Load tagGrowth
require(TagGrowth)

# Add results folder if required
lapply(file.path(scenarios, power, "results"), dir.create)

# The model
compile("../../inst/executables/ATR.cpp")
TmbFile <- paste0(system.file("executables", package = "TagGrowth"), "/")
Version <- "ATR"

# Come back to 55

# Warning - this takes a looooong time
for (Iscenario in scenarios)
{
    for (Ipow in power)
    {
        #for (Isim in 1:Ndesign)
        for (Isim in 183:Ndesign)
        {
            cat("\nStarting simulation", Isim, "...\n")
            dyn.load(paste0(TmbFile, dynlib("ATR")))
    
            # Data
            fname <- paste(Iscenario, Ipow, "/sim", Isim, ".RData", sep = "")
            load(fname)
            sim$obj <- NULL
            sim$opt <- NULL
            sim$Report <- NULL
            save(sim, file = fname)

            # Create the AD object
            if (Iscenario == "v0/")
                Options <- c("YearTF" = 0, "AreaTF" = 0, "IndivTF" = 0, "IndivTimeTF" = 0)
            if (Iscenario == "v1/")
                Options <- c("YearTF" = 0, "AreaTF" = 0, "IndivTF" = 1, "IndivTimeTF" = 0)
            if (Iscenario == "v2/")
                Options <- c("YearTF" = 0, "AreaTF" = 0, "IndivTF" = 0, "IndivTimeTF" = 1)
            if (Iscenario == "v3/")
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

        # Plot the estimated parameter values from all of the simulations
        plot_simulations(paste0(Iscenario, Ipow, "/"))

    }
}




    # Plot up an example of a specific simulation to see what the fit looks like
    Isim <- 3
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

