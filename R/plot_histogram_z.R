#' Plot histogram of time-varying individual growth parameter (z)
#'
#' @export
#' 
plot_histogram_z <- function(data, report, file_name = "REs_z")
{
    REs_z1 <- report$par.random[names(report$par.random) %in% "z1"]
    REs_z2 <- report$par.random[names(report$par.random) %in% "z2"]
    Sex <- data$Sex
    d <- data.frame(Sex, REs_z1, REs_z2)
    dat <- melt(d, id.vars = "Sex")
    dat$Sex[dat$Sex == 1] <- "Females"
    dat$Sex[dat$Sex == 2] <- "Males"
    
    p <- ggplot(data = dat, aes(x = value)) +
        geom_histogram(aes(y = ..density..), colour = "black", fill = "grey") +
        facet_grid(variable ~ Sex, scales = "free") +
        xlab("\nz") + ylab("Density\n") +
        plot_theme() +
        scale_colour_manual(values = plot_palette)
    
    png(paste(file_name, ".png", sep = ""), width = 8, height = 4, units = "in", res = 400)
    print(p)
    dev.off()
}
