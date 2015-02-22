#' Plot standardised residuals
#'
#' @export
#' 
plot_standard_resids <- function(Sex, Length1_obs, Length1_hat, Length2_obs, Length2_hat,
                                 file_name = "StandardResids", file_suffix = "eps")
{
    res1 <- Length1_obs - Length1_hat
    res2 <- Length2_obs - Length2_hat
    res1 <- res1 / sd(res1)
    res2 <- res2 / sd(res2)
    dat <- data.frame(Sex, Residual1 = res1, Residual2 = res2)
    dat$Sex[dat$Sex == 1] <- "Females"
    dat$Sex[dat$Sex == 2] <- "Males"

    lim <- max(res1, res2, abs(res1), abs(res2))
    
    p <- ggplot(data = dat) +
        geom_hline(yintercept = 0) +
        geom_vline(xintercept = 0) +
        aes(shape = Sex) + 
        geom_point(aes(x = Residual1, y = Residual2, color = Sex)) +
        xlab("\nStandardized residual at tagging") + ylab("Standardized residual at recapture\n") +
        scale_x_continuous(limits = c(-lim, lim)) +
        scale_y_continuous(limits = c(-lim, lim)) +
        coord_fixed() +
        plot_theme() + theme(legend.position = c(0.86, 0.13)) +
        scale_colour_manual(values = plot_palette)
    
    if (file_suffix %in% c("eps",".eps"))
        cairo_ps(paste(file_name, ".eps", sep = ""), width = 5, height = 5)
    if (file_suffix %in% c("png",".png"))
        png(paste(file_name, ".png", sep = ""), width = 10, height = 5, units = "in", res = 400)
    print(p)
    dev.off()
}
