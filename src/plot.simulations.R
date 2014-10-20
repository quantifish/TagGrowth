plot.simulations <- function(directory = ".")
{
    require(ggplot2)
    require(reshape2)
    source("plot_theme.R")
    directory <- "../simulation/sims"
    par.fixed <- NULL
    for (Isim in 1:47)
    {
        fname <- paste(directory, "/sim", Isim, ".RData", sep = "")
        load(fname)
        #par.fixed <- rbind(par.fixed, sim$Report$par.fixed)
        par.fixed <- rbind(par.fixed, sim$Report$value[1:16])
    }
    par.fixed1 <- data.frame(melt(par.fixed[,c(1,2,13,14)]), Sex = "Both")
    par.fixed2 <- data.frame(melt(par.fixed[,c(3,9,11)]), Sex = "Females")
    par.fixed3 <- data.frame(melt(par.fixed[,c(4,10,12)]), Sex = "Males")
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

    p <- ggplot(data = par.fixed, aes(x = value)) +
        geom_histogram(colour = "black", fill = "orange") +
        #geom_density(colour = "black", fill = "orange") +
        facet_grid(Sex ~ Var2, scales = "free_x") +
        #facet_wrap(Var2 ~ Sex, scales = "free_x") +
        geom_vline(aes(xintercept = truth), size = 1.5, colour = "red", alpha = 0.6) +
        xlab("") + ylab("Frequency\n") +
        plot_theme()
        #scale_colour_manual(values = plot_palette)

    
    png(paste("SimPars.png", sep = ""), width = 10, height = 6, units = "in", res = 300)
    print(p)
    dev.off()
}
