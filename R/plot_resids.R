#' Plot residuals
#'
#' @export
#' 
plot_resids <- function(Sex, Length1_obs, Length1_hat, Length2_obs, Length2_hat,
                        file_name = "Resids")
{
    res1 <- Length1_obs - Length1_hat
    res2 <- Length2_obs - Length2_hat
    dat <- data.frame(Sex, Residual1 = res1, Residual2 = res2)
    dat$Sex[dat$Sex == 1] <- "Females"
    dat$Sex[dat$Sex == 2] <- "Males"
    
    p <- ggplot(data = dat) +
        geom_hline(yintercept = 0) +
        geom_vline(xintercept = 0) +
        aes(shape = Sex) + 
        geom_point(aes(x = Residual1, y = Residual2, color = Sex)) +
        xlab("\nResidual at tagging (cm)") + ylab("Residual at recapture (cm)\n") +
        #scale_x_continuous(limits = c(50, 175)) +
        #scale_y_continuous(limits = c(50, 175)) +
        coord_fixed() +
        #guides(shape = guide_legend(title = "Sex")) +
        plot_theme() +
        scale_colour_manual(values = plot_palette)
    
    png(paste(file_name, ".png", sep = ""), width = 5, height = 5, units = "in", res = 400)
    print(p)
    dev.off()
}

# Plot observed vs. predicted length at age
#png("ObsVsPred.png", width=5, height=5, units="in", res=300)
#par(mfrow=c(1,2))
#ylim <- range(ATR_mod$Length1[1:Nindiv], ATR_mod$Length2[1:Nindiv])
#xlim <- range(Report$value[names(Report$value) %in% "Length1_hat"], Report$value[names(Report$value) %in% "Length2_hat"])
#plot( x=Report$value[names(Report$value) %in% "Length1_hat"], y=ATR_mod$Length1[1:Nindiv], xlab="Predicted length", ylab="Observed length", las = 1, xlim = xlim, ylim = ylim)
#abline(0,1, col=2, lwd = 2); box()
#plot( x = Report$value[names(Report$value) %in% "Length2_hat"], y = ATR_mod$Length2[1:Nindiv], xlab="Predicted length", ylab="Observed length", las = 1, xlim = xlim, ylim = ylim)
#abline(0,1, col=2, lwd = 2); box()
#dev.off()
