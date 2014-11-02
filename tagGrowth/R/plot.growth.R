plot.growth <- function(x, label = "")
{
    png(paste("sims/growth-", label, ".png", sep = ""), width = 6, height = 6, units = "in", res = 300)
    par(mfrow = c(1, 1))
    xlim <- c(0, max(x$Age2))
    ylim <- c(0, 200)
    plot(1, type = "n", xlim = xlim, ylim = ylim, xlab = "Age (weeks)", ylab = "Length (cm)", las = 1)
    segments(x0 = x$Age1[x$Sex==1], x1 = x$Age2[x$Sex==1], y0 = x$Length1[x$Sex==1], y1 = x$Length2[x$Sex==1], col = "pink")
    segments(x0 = x$Age1[x$Sex==2], x1 = x$Age2[x$Sex==2], y0 = x$Length1[x$Sex==2], y1 = x$Length2[x$Sex==2], col = "blue")
    legend("bottomright", legend = c("Females","Males"), col = c("pink","blue"), lwd = 1, bty = "n")
    dev.off()
}
