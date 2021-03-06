#' Plot observed vs. predicted growth paths
#'
#' @export
#' 
plot_obs_pred <- function(Sex, Length1_obs, Length1_hat, Length2_obs, Length2_hat,
                          file_name = "ObsVsPred", file_suffix = "eps")
{
    d1 <- data.frame(Sex, Length1_obs, Length1_hat, Length = "Length at tagging")
    d2 <- data.frame(Sex, Length2_obs, Length2_hat, Length = "Length at recapture")
    names(d2) <- names(d1)
    dat <- rbind(d1, d2)
    dat$Sex[dat$Sex == 1] <- "Females"
    dat$Sex[dat$Sex == 2] <- "Males"
    
    p <- ggplot(data = dat) +
        geom_abline(aes(yintercept = 0, slope = 1)) +
        aes(shape = Sex) + 
        geom_point(aes(x = Length1_hat, y = Length1_obs, color = Sex)) +
        facet_grid(. ~ Length) +
        xlab("\nPredicted length (cm)") + ylab("Observed length (cm)\n") +
        scale_x_continuous(limits = c(50, 175)) +
        scale_y_continuous(limits = c(50, 175)) +
        coord_fixed() +
        #guides(shape = guide_legend(title = "Sex")) +
        plot_theme() +
        scale_colour_manual(values = plot_palette)
    
    if (file_suffix %in% c("eps",".eps"))
        cairo_ps(paste(file_name, ".eps", sep = ""), width = 10, height = 5)
    if (file_suffix %in% c("png",".png"))
        png(paste(file_name, ".png", sep = ""), width = 10, height = 5, units = "in", res = 400)
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
