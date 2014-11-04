#' Plot L infinity
#'
#' @export
#' 
plot_linf <- function(report, file_name = "LinfPrior")
{
    png(paste(file_name, ".png", sep = ""), width=5, height=5, units="in", res=300)
    par(mfrow = c(1, 1))
    x <- 75:275
    priorF <- dnorm(x = x, mean = 180.20, sd = 0.102*180.20)
    priorM <- dnorm(x = x, mean = 169.07, sd = 0.102*169.07)
    plot(x, priorM, type = "l", col = "blue", lwd = 2, xlab = expression(L[infinity]), ylab = "Density", las = 1)
    lines(x, priorF, col="pink", lwd=2)
    LinfF <- report$value[names(report$value) %in% "Linf"][1]
    LinfM <- report$value[names(report$value) %in% "Linf"][2]
    abline(v=LinfF, lty=2, lwd=2, col="pink")
    abline(v=LinfM, lty=2, lwd=2, col="blue")
    legend("topleft", legend = c("Female prior", "Female estimate", "Male prior", "Male estimate"), lwd = 2, lty = c(1,2,1,2), col=c("pink","pink","blue","blue"), bty = "n")
    dev.off()
}
