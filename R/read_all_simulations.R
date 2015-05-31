#' Function to read simulations
#'
#' @param directory the dir
#' 
#' @export
#' 
read_all_simulations <- function(directory = "", Nsims = 200)
{
    #scenarios <- c("v0","v1","v2","v3")
    scenarios <- c("sim_none","sim_k","sim_z","sim_kz")
    mods <- c("est_none","est_k","est_z","est_kz")
    powers <- c(50,100,250,500)
    fixed.pars <- c("gamma","psi","L0","bmean","sd_bdev","sd_obs","sd_z")
    sex <- c("Female","Male","Both","Female","Male","Female","Male","Female","Male","Both","Both")

    par.fixed <- NULL

    for (Isim in 1:Nsims)
    {
        for (Iscenario in scenarios)
        {
            for (Ipow in mods)
            {
                fname <- paste0(directory, Iscenario, "/", Ipow, "/sim", Isim, ".RData")
                if (file.exists(fname))
                {
                    load(fname)
                    if ( !is.null(sim$Report$pdHess) )
                    {
                        if ( sim$Report$pdHess )
                        {
                            pf <- sim$Report$value[names(sim$Report$value) %in% fixed.pars]
                            pf <- data.frame(Simulation = Iscenario, Estimation = Ipow, Parameter = names(pf), Sex = sex, Estimate = pf, Truth = NA)
                            pf$Truth[pf$Parameter == "sd_z"]                         <- sim$Parameters['sd_z',1]
                            pf$Truth[pf$Parameter == "psi"]                          <- sim$Parameters['psi',1]
                            pf$Truth[pf$Parameter == "sd_obs"]                       <- sim$Parameters['sd_obs',1]
                            pf$Truth[pf$Parameter == "gamma"   & pf$Sex == "Female"] <- sim$Parameters['gamma',1]
                            pf$Truth[pf$Parameter == "gamma"   & pf$Sex == "Male"]   <- sim$Parameters['gamma',2]
                            pf$Truth[pf$Parameter == "L0"      & pf$Sex == "Female"] <- sim$Parameters['L0',1]
                            pf$Truth[pf$Parameter == "L0"      & pf$Sex == "Male"]   <- sim$Parameters['L0',2]
                            pf$Truth[pf$Parameter == "bmean"   & pf$Sex == "Female"] <- sim$Parameters['bmean',1]
                            pf$Truth[pf$Parameter == "bmean"   & pf$Sex == "Male"]   <- sim$Parameters['bmean',2]
                            pf$Truth[pf$Parameter == "sd_bdev" & pf$Sex == "Female"] <- sim$Parameters['sd_b',1]
                            pf$Truth[pf$Parameter == "sd_bdev" & pf$Sex == "Male"]   <- sim$Parameters['sd_b',2]
                            par.fixed <- rbind(pf, par.fixed)
                        }
                    }
                }
            }
        }
    }

    ord <- fixed.pars[fixed.pars %in% unique(par.fixed$Parameter)]
    par.fixed$Parameter <- factor(par.fixed$Parameter, levels = ord)
    #par.fixed$Simulation <- factor(par.fixed$Simulation, levels = mods)
    
    return(par.fixed)
}
