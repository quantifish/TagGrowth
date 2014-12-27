#' Plot individual growth paths
#'
#' @export
#' 
plot_indiv_growth <- function(Sex,
                              Age1, Length1_obs, Length1_hat,
                              Age2, Length2_obs, Length2_hat,
                              file_name = "IndivGrowth")
{
    d1 <- data.frame(Age1, Age2, Sex, Length1_obs, Length2_obs, Key = "Observed")
    d2 <- data.frame(Age1, Age2, Sex, Length1_hat, Length2_hat, Key = "Expected")
    names(d2) <- names(d1)
    dat <- rbind(d1, d2)
    dat$Sex[dat$Sex == 1] <- "Females"
    dat$Sex[dat$Sex == 2] <- "Males"
    
    p <- ggplot(data = dat) +
        geom_segment(aes(x = Age1, y = Length1_obs, xend = Age2, yend = Length2_obs, group = c(Key), color = Key)) +
        facet_grid(. ~ Sex) +
        xlab("\nAge (weeks)") + ylab("Length (cm)\n") +
        scale_x_continuous(limits = c(0, max(dat$Age1, dat$Age2))) +
        scale_y_continuous(limits = c(0, max(dat$Length1_obs, dat$Length2_obs))) +
        guides(color = guide_legend(title = "Key")) +
        plot_theme() +
        scale_colour_manual(values = plot_palette)
    
    png(paste(file_name, ".png", sep = ""), width = 10, height = 5, units = "in", res = 400)
    print(p)
    dev.off()
}

# Plot observed vs. "true" growth schedules
#png("IndivGrowth.png", width=10, height=5, units="in", res=300)
#par(mfrow=c(1,2))
#plot(1, type="n", xlim=c(0,max(ATR_mod$Age2[1:Nindiv]/365)), ylim=c(0,max(ATR_mod$Length2[1:Nindiv])), xlab="Age", ylab="Length (cm)", las=1)
#segments(x0=ATR_mod$iAge1[ATR_mod$Sex==1]/365, x1=ATR_mod$Age2[ATR_mod$Sex==1]/365, y0=ATR_mod$Length1[ATR_mod$Sex==1], y1=ATR_mod$Length2[ATR_mod$Sex==1])
#segments(x0=ATR_mod$iAge1[ATR_mod$Sex==1]/365, x1=ATR_mod$Age2[ATR_mod$Sex==1]/365, y0=(Report$value[names(Report$value) %in% "Length1_hat"])[ATR_mod$Sex==1], y1=(Report$value[names(Report$value) %in% "Length2_hat"])[ATR_mod$Sex==1], col="red")
#title("Females")
#legend("bottomright", legend=c("Observed","Expected"), col=1:2, lwd=1, bty="n")
#plot(1, type="n", xlim=c(0,max(ATR_mod$Age2[1:Nindiv]/365)), ylim=c(0,max(ATR_mod$Length2[1:Nindiv])), xlab="Age", ylab="Length (cm)", las=1)
#segments(x0=ATR_mod$iAge1[ATR_mod$Sex==2]/365, x1=ATR_mod$Age2[ATR_mod$Sex==2]/365, y0=ATR_mod$Length1[ATR_mod$Sex==2], y1=ATR_mod$Length2[ATR_mod$Sex==2])
#segments(x0=ATR_mod$iAge1[ATR_mod$Sex==2]/365, x1=ATR_mod$Age2[ATR_mod$Sex==2]/365, y0=(Report$value[names(Report$value) %in% "Length1_hat"])[ATR_mod$Sex==2], y1=(Report$value[names(Report$value) %in% "Length2_hat"])[ATR_mod$Sex==2], col="red")
#title("Males")
#legend("bottomright", legend=c("Observed","Expected"), col=1:2, lwd=1, bty="n")
#dev.off()
