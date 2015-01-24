#' Plot histogram of time-varying k
#'
#' @export
#' 
plot_histogram_k <- function(data, report, file_name = "REs_b")
{
    REs_b <- report$par.random[names(report$par.random) %in% "ln_bdev"]
    Sex <- data$Sex
    d <- data.frame(Sex, REs_b)
    dat <- melt(d, id.vars = "Sex")
    dat$Sex[dat$Sex == 1] <- "Females"
    dat$Sex[dat$Sex == 2] <- "Males"
    
    p <- ggplot(data = dat, aes(x = value)) +
        geom_histogram(aes(y = ..density..), colour = "black", fill = "grey") +
        facet_grid(~ Sex) +
        xlab("\nk") + ylab("Density\n") +
        plot_theme() +
        scale_colour_manual(values = plot_palette)
    
    png(paste(file_name, ".png", sep = ""), width = 8, height = 4, units = "in", res = 400)
    print(p)
    dev.off()
}



# Plot histogram of REs
#png("REs.png", width=5, height=5, units="in", res=300)
#par(mfrow=c(2,2))
#xlim <- range(Report$value[names(Report$value) %in% "bmean"][1]*exp(REs_b))
#hist((Report$value[names(Report$value) %in% "bmean"][1]*exp(REs_b))[ATR_mod$Sex==1], xlab="b", main="Females", las = 1, xlim = xlim, col = "grey")
#abline(v=Report$value[names(Report$value) %in% "bmean"][1], col=2, lwd=2, lty = 2)
#box()
#hist((Report$value[names(Report$value) %in% "bmean"][2]*exp(REs_b))[ATR_mod$Sex==2], xlab="b", main="Males", las = 1, xlim = xlim, col = "grey")
#abline(v=Report$value[names(Report$value) %in% "bmean"][2], col=2, lwd=2, lty = 2)
#box()
#xlim <- range(REs_z1, REs_z2)
#hist(REs_z1, xlab="z1", main = "", las = 1, col = "grey", xlim = xlim)
#box()
#hist(REs_z2, xlab="z2", main = "", las = 1, col = "grey", xlim = xlim)
#box()
#dev.off()