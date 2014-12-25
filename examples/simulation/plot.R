require(TagGrowth) 

plot_simulations("v0/")
plot_simulations("v1/")
plot_simulations("v2/")
plot_simulations("v3/")


v0 <- read_simulations("v0/")
plot_simulations("v0/")

lab <- c(expression(sigma[obs]), expression(L[0]), expression(k),
         expression(gamma))
sex <- c("Females", "Males")

par(mfrow = c(3,2))
for (i in 2:length(unique(v0$Parameter)))
{
    r <- subset(v0, Parameter %in% unique(v0$Parameter)[i])
    xlim <- range(r$Value) * c(0.95, 1.05)
    for (j in 1:2)
    {
        d <- subset(v0, Parameter %in% unique(v0$Parameter)[i] & Sex %in% sex[j])
        hist(d$Value, main = "", xlab = lab[i], col = "grey", las = 1, xlim = xlim)
        abline(v = d$Truth[1], col = 2, lwd = 2)
    }
}
