plot.simulations <- function(directory = ".")
{
    require(ggplot2)
    require(reshape2)
    source("plot_theme.R")
    directory <- "../simulation/sims"
    par.fixed <- NULL
    for (Isim in 1:9)
    {
        fname <- paste(directory, "/sim", Isim, ".RData", sep = "")
        load(fname)
        #Sim[[Isim]] <- sim
        par.fixed <- rbind(par.fixed, sim$Report$par.fixed)
    }
    par.fixed1 <- data.frame(melt(par.fixed[,c(1,2,9,10)]), Sex = "Both")
    par.fixed2 <- data.frame(melt(par.fixed[,c(3,5,7)]), Sex = "Females")
    par.fixed3 <- data.frame(melt(par.fixed[,c(4,6,8)]), Sex = "Males")
    par.fixed <- rbind(par.fixed1, par.fixed2, par.fixed3)
    par.fixed$value <- exp(par.fixed$value)
    
    sim$Parameters

    ggplot(data = par.fixed, aes(x = value)) +
        geom_histogram(colour = "black", fill = "orange") +
            facet_grid(Sex ~ Var2, scales = "free_x") +
            #facet_wrap(Sex ~ Var2, scales = "free_x") +
        xlab("") + ylab("Frequency\n") +
        plot_theme()
        #scale_colour_manual(values = plot_palette)

    
    png(paste("SimPars.png", sep = ""), width = 6, height = 6, units = "in", res = 300)
    print(p)
    dev.off()
}
