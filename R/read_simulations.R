#' Function to read simulations
#'
#' @param directory the dir
#' 
#' @export
#' 
read_simulations <- function(directory)
{
    par.fixed <- NULL
    pdH <- NULL
    fixed.pars <- c("gamma","psi","L0","bmean","sd_bdev","sd_obs","sd_z")
    for (Isim in 1:100)
    {
        fname <- paste(directory, "sim", Isim, ".RData", sep = "")
        load(fname)
        if ( !is.null(sim$Report$pdHess) )
        {
            if ( sim$Report$pdHess )
            {
                par.fixed <- rbind(sim$Report$value[names(sim$Report$value) %in% fixed.pars], par.fixed)
            }
            pdH <- c(sim$Report$pdHess, pdH)
        } else {
            pdH <- c(FALSE, pdH)
        }
    }

    # Identify those parameters (columns) in which the parameter is static for removal
    flag <- NULL
    for (I in 1:ncol(par.fixed))
    {
        if (all(par.fixed[,I] == par.fixed[1,I]))
        {
            flag <- c(flag, I)
        }
    }
    ind_remove <- colnames(par.fixed)[flag]

    # Identify those parameters that are not sex-specific, we will plonk these in with females
    ind <- which(colnames(par.fixed) %in% c("psi","sd_obs", "sd_z"))
    par.fixed1 <- data.frame(melt(par.fixed[,ind]), Sex = "Females")

    # Identify those parameters that are sex-specific
    ind_L0      <- which(colnames(par.fixed) %in% "L0")
    ind_bmean   <- which(colnames(par.fixed) %in% "bmean")
    ind_gamma   <- which(colnames(par.fixed) %in% "gamma")
    ind_sd_bdev <- which(colnames(par.fixed) %in% "sd_bdev")
    par.fixed2 <- data.frame(melt(par.fixed[,c(ind_L0[1], ind_bmean[1], ind_gamma[1],
                                               ind_sd_bdev[1])]), Sex = "Females")
    par.fixed3 <- data.frame(melt(par.fixed[,c(ind_L0[2], ind_bmean[2], ind_gamma[2],
                                               ind_sd_bdev[2])]), Sex = "Males")
    names(par.fixed2) <- names(par.fixed1)
    names(par.fixed3) <- names(par.fixed1)
    
    par.fixed4 <- rbind(par.fixed1, par.fixed2, par.fixed3)
    par.fixed <- par.fixed4
    par.fixed$truth <- NA
    par.fixed$truth[par.fixed$Var2 == "sd_z"]                                 <- sim$Parameters['sd_z',1]
    par.fixed$truth[par.fixed$Var2 == "psi"]                                  <- sim$Parameters['psi',1]
    par.fixed$truth[par.fixed$Var2 == "sd_obs"]                               <- sim$Parameters['sd_obs',1]
    par.fixed$truth[par.fixed$Var2 == "gamma"   & par.fixed$Sex == "Females"] <- sim$Parameters['gamma',1]
    par.fixed$truth[par.fixed$Var2 == "gamma"   & par.fixed$Sex == "Males"]   <- sim$Parameters['gamma',2]
    par.fixed$truth[par.fixed$Var2 == "L0"      & par.fixed$Sex == "Females"] <- sim$Parameters['L0',1]
    par.fixed$truth[par.fixed$Var2 == "L0"      & par.fixed$Sex == "Males"]   <- sim$Parameters['L0',2]
    par.fixed$truth[par.fixed$Var2 == "bmean"   & par.fixed$Sex == "Females"] <- sim$Parameters['bmean',1]
    par.fixed$truth[par.fixed$Var2 == "bmean"   & par.fixed$Sex == "Males"]   <- sim$Parameters['bmean',2]
    par.fixed$truth[par.fixed$Var2 == "sd_bdev" & par.fixed$Sex == "Females"] <- sim$Parameters['sd_b',1]
    par.fixed$truth[par.fixed$Var2 == "sd_bdev" & par.fixed$Sex == "Males"]   <- sim$Parameters['sd_b',2]
    par.fixed <- par.fixed[!par.fixed$Var2 %in% ind_remove,]

    ord <- fixed.pars[fixed.pars %in% unique(par.fixed$Var2)]
    par.fixed$Var2 <- factor(par.fixed$Var2, levels = ord)
    
    # Print the number of fits that were pdH to screen
    cat("\nNumber of pdH fits =", length(which(pdH)), "\n\n")

    # Print the true parameter values to screen
    print(round(sim$Parameters, 5))
    cat("\n")

    names(par.fixed) <- c("Simulation","Parameter","Value","Sex","Truth")
    return(par.fixed)
}
