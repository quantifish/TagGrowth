#' Pairs plot of parameters
#'
#' @param pars a matrix of the parameters to be plotted
#' @param names a vector of the parameter labels
#' @param path the dircetory to save the plot in
#' @return a plot
#' @export
#'
plot_parameters <- function(pars, names, path = ".")
{
    png(paste(path, "/parameters.png", sep = ""), width = 6, height = 6, units = "in", res = 300)
    pairs(as.data.frame(pars), las = 1, labels = names, gap = 0.2)
    dev.off()
}
