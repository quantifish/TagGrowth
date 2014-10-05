plot.parameters <- function(pars, names)
{
    png(paste("sims/parameters.png", sep = ""), width = 6, height = 6, units = "in", res = 300)
    pairs(as.data.frame(pars), las = 1, labels = names, gap = 0.2)
    dev.off()
}
