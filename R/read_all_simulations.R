#' Function to read simulations
#'
#' @param directory the dir
#' 
#' @export
#' 
read_all_simulations <- function(directory = "../examples/simulation/", Nsims = 200)
{
    scenarios <- c("v0","v1","v2","v3")
    powers <- c(50,100,250,500)
    fixed.pars <- c("gamma","psi","L0","bmean","sd_bdev","sd_obs","sd_z")
    sex <- c("Female","Male","Both","Female","Male","Female","Male","Female","Male","Both","Both")

    par.fixed <- NULL

    for (Isim in 1:Nsims)
    {
        for (Iscenario in scenarios)
        {
            for (Ipow in powers)
            {
                fname <- paste(directory, Iscenario, "/", Ipow, "/sim", Isim, ".RData", sep = "")
                load(fname)
                if ( !is.null(sim$Report$pdHess) )
                {
                    if ( sim$Report$pdHess )
                    {
                        pf <- sim$Report$value[names(sim$Report$value) %in% fixed.pars]
                        pf2 <- data.frame(Iscenario, Ipow, names(pf), sex, pf)
                        par.fixed <- rbind(pf2, par.fixed)
                    }
                }
            }
        }
    }
    names(par.fixed) <- c("Scenario","Power","Parameter","Sex","Estimate")
    
    par.fixed$truth <- NA
    par.fixed$truth[par.fixed$Parameter == "sd_z"]                                 <- sim$Parameters['sd_z',1]
    par.fixed$truth[par.fixed$Parameter == "psi"]                                  <- sim$Parameters['psi',1]
    par.fixed$truth[par.fixed$Parameter == "sd_obs"]                               <- sim$Parameters['sd_obs',1]
    par.fixed$truth[par.fixed$Parameter == "gamma"   & par.fixed$Sex == "Female"] <- sim$Parameters['gamma',1]
    par.fixed$truth[par.fixed$Parameter == "gamma"   & par.fixed$Sex == "Male"]   <- sim$Parameters['gamma',2]
    par.fixed$truth[par.fixed$Parameter == "L0"      & par.fixed$Sex == "Female"] <- sim$Parameters['L0',1]
    par.fixed$truth[par.fixed$Parameter == "L0"      & par.fixed$Sex == "Male"]   <- sim$Parameters['L0',2]
    par.fixed$truth[par.fixed$Parameter == "bmean"   & par.fixed$Sex == "Female"] <- sim$Parameters['bmean',1]
    par.fixed$truth[par.fixed$Parameter == "bmean"   & par.fixed$Sex == "Male"]   <- sim$Parameters['bmean',2]
    par.fixed$truth[par.fixed$Parameter == "sd_bdev" & par.fixed$Sex == "Female"] <- sim$Parameters['sd_b',1]
    par.fixed$truth[par.fixed$Parameter == "sd_bdev" & par.fixed$Sex == "Male"]   <- sim$Parameters['sd_b',2]

    ord <- fixed.pars[fixed.pars %in% unique(par.fixed$Parameter)]
    par.fixed$Parameter <- factor(par.fixed$Parameter, levels = ord)
    
    names(par.fixed) <- c("Scenario","Power","Parameter","Sex","Estimate","Truth")
    return(par.fixed)
}
