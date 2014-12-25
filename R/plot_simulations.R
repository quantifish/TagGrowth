#' Function to plot simulations
#'
#' @param directory the dir
#' @return plots
#' 
#' @export
#' 
plot_simulations <- function(directory)
{
    par.fixed <- read_simulations(directory)
        
    p <- ggplot(data = par.fixed, aes(x = Value)) +
        geom_histogram(colour = "black", fill = "grey") +
        facet_grid(Sex ~ Parameter, scales = "free") +
        #geom_vline(aes(xintercept = Truth), size = 1.5, colour = "red", alpha = 0.6) +
        geom_vline(aes(xintercept = Truth), size = 0.75, colour = "red") +
        xlab("") + ylab("Frequency\n") +
        plot_theme()
        #scale_colour_manual(values = plot_palette)
    
    png(paste(directory, "results/", "SimPars.png", sep = ""), width = 10, height = 6, units = "in", res = 300)
    print(p)
    dev.off()
    
    p <- ggplot(data = par.fixed, aes(x = Simulation, y = Value)) +
        geom_hline(aes(yintercept = Truth), size = 0.75, colour = "red") +
        geom_line(colour = "black", fill = "orange") +
        facet_grid(Parameter ~ Sex, scales = "free") +
        xlab("\nSimulation") + ylab("") +
        plot_theme()

    png(paste(directory, "results/", "TracePars.png", sep = ""), width = 10, height = 6, units = "in", res = 300)
    print(p)
    dev.off()

    #p <- ggplot(data = subset(par.fixed, Parameter %in% c("gamma","L0" ,"bmean")), aes(x = Value)) +
    #    geom_histogram(colour = "black", fill = "grey") +
    #    facet_wrap(Parameter ~ Sex, scales = "free") +
    #    #facet_grid(Parameter ~ Sex, scales = "free") +
    #    geom_vline(aes(xintercept = Truth), size = 0.75, colour = "red") +
    #    xlab("") + ylab("Frequency\n") +
    #    plot_theme()
    #png(paste(directory, "results/", "SimPars.png", sep = ""), width = 10, height = 6, units = "in", res = 300)
    #print(p)
    #dev.off()

}
