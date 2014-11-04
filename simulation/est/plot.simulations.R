
require(tagGrowth)

# Plot up a specific simulation
Isim=1
directory <- "../sims"
fname <- paste(directory, "/sim", Isim, ".RData", sep = "")
load(fname)
load("../../data/ATR_mod.RData")
plot_histogram_b(data = sim$Sim, report = sim$Report)
plot_obs_pred(sim$Sim$Sex, sim$Sim$Length1_true, sim$Sim$Length1, sim$Sim$Length2_true, sim$Sim$Length2)
plot_indiv_growth(sim$Sim$Sex,
                  sim$Sim$Age1, sim$Sim$Length1, sim$Sim$Length1_true,
                  sim$Sim$Age2, sim$Sim$Length2, sim$Sim$Length2_true)

plot.simulations <- function(directory = ".")
{
    require(tagGrowth)
    directory <- "../sims"
    par.fixed <- NULL
    pdH <- NULL    
    fixed.pars <- c("gamma","psi","L0","bmean","sd_bdev","sd_obs")
    for (Isim in 1:100)
    {
        fname <- paste(directory, "/sim", Isim, ".RData", sep = "")
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
    par.fixed1 <- data.frame(melt(par.fixed[,c(1,2,9)]), Sex = "Both")
    par.fixed2 <- data.frame(melt(par.fixed[,c(3,5,7)]), Sex = "Females")
    par.fixed3 <- data.frame(melt(par.fixed[,c(4,6,8)]), Sex = "Males")
    par.fixed4 <- rbind(par.fixed1, par.fixed2, par.fixed3)
    par.fixed <- par.fixed4
    par.fixed$truth <- NA
    par.fixed$truth[par.fixed$Var2 == "gamma"] <- sim$Parameters['gamma',1]
    par.fixed$truth[par.fixed$Var2 == "psi"] <- sim$Parameters['psi',1]
    par.fixed$truth[par.fixed$Var2 == "sd_obs"] <- sim$Parameters['sd_obs',1]
    par.fixed$truth[par.fixed$Var2 == "sd_z"] <- sim$Parameters['sd_z',1]
    par.fixed$truth[par.fixed$Var2 == "L0" & par.fixed$Sex == "Females"] <- sim$Parameters['L0',1]
    par.fixed$truth[par.fixed$Var2 == "L0" & par.fixed$Sex == "Males"] <- sim$Parameters['L0',2]
    par.fixed$truth[par.fixed$Var2 == "bmean" & par.fixed$Sex == "Females"] <- sim$Parameters['bmean',1]
    par.fixed$truth[par.fixed$Var2 == "bmean" & par.fixed$Sex == "Males"] <- sim$Parameters['bmean',2]
    par.fixed$truth[par.fixed$Var2 == "sd_bdev" & par.fixed$Sex == "Females"] <- sim$Parameters['sd_b',1]
    par.fixed$truth[par.fixed$Var2 == "sd_bdev" & par.fixed$Sex == "Males"] <- sim$Parameters['sd_b',2]

    #============================================================================
    
    p <- ggplot(data = par.fixed, aes(x = value)) +
        geom_histogram(colour = "black", fill = "orange") +
        #geom_density(colour = "black", fill = "orange") +
        facet_grid(Sex ~ Var2, scales = "free") +
        #geom_vline(aes(xintercept = truth), size = 1.5, colour = "red", alpha = 0.6) +
        geom_vline(aes(xintercept = truth), size = 0.75, colour = "red") +
        xlab("") + ylab("Frequency\n") +
        plot_theme()
        #scale_colour_manual(values = plot_palette)
    
    png(paste("SimPars.png", sep = ""), width = 10, height = 6, units = "in", res = 300)
    print(p)
    dev.off()

    #============================================================================
    
    p <- ggplot(data = par.fixed, aes(x = Var1, y = value)) +
        geom_hline(aes(yintercept = truth), size = 0.75, colour = "red") +
        geom_line(colour = "black", fill = "orange") +
        facet_grid(Var2 ~ Sex, scales = "free") +
        xlab("") + ylab("Frequency\n") +
        plot_theme()

    png(paste("TracePars.png", sep = ""), width = 10, height = 6, units = "in", res = 300)
    print(p)
    dev.off()    

    print(length(which(pdH)))
}
