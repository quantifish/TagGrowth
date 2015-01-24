#' Plot annual deviates
#'
#' @export
#' 
plot_annual_devs <- function(report, year0 = 1972, file_name = "REs_y")
{
    REs_y <- report$par.random[names(report$par.random) %in% "ln_ydev"]
    yr <- year0:(year0 + (length(REs_y) - 1))
    dat <- data.frame(Year = yr, Ydev = REs_y)
    
    p <- ggplot(data = dat, aes(x = Year)) +
        geom_line(aes(y = Ydev), colour = "black") +
        xlab("\nYear") + ylab("Ydev\n") +
        plot_theme() +
        scale_colour_manual(values = plot_palette)
    
    png(paste(file_name, ".png", sep = ""), width = 4, height = 4, units = "in", res = 400)
    print(p)
    dev.off()
}
