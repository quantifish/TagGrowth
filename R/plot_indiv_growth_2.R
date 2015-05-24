#' Plot individual growth paths for two different models
#'
#' @export
#' 
plot_indiv_growth_2 <- function (Sex, Age1, Length1_obs, Length1_hat1, Length1_hat2, Age2, Length2_obs, Length2_hat1, Length2_hat2, Label = c("No random effects", "Transient variation"), file_name = "IndivGrowth", file_suffix = "eps") 
{
    d1 <- data.frame(Age1, Age2, Sex, Length1_obs, Length2_obs, 
        Key = "Observed", Model = Label[1])
    d2 <- data.frame(Age1, Age2, Sex, Length1_obs, Length2_obs, 
        Key = "Observed", Model = Label[2])
    d3 <- data.frame(Age1, Age2, Sex, Length1_hat1, Length2_hat1, 
        Key = "Expected", Model = Label[1])
    d4 <- data.frame(Age1, Age2, Sex, Length1_hat2, Length2_hat2, 
        Key = "Expected", Model = Label[2])
    names(d4) <- names(d3) <- names(d2) <- names(d1)
    dat <- rbind(d1, d2, d3, d4)
    dat$Sex[dat$Sex == 1] <- "Females"
    dat$Sex[dat$Sex == 2] <- "Males"
    p <- ggplot(data = dat) + geom_segment(aes(x = Age1, y = Length1_obs, 
        xend = Age2, yend = Length2_obs, group = c(Key), color = Key)) + 
        facet_grid(Model ~ Sex) + xlab("\nAge (years)") + ylab("Length (cm)\n") + 
        scale_x_continuous(limits = c(0, max(dat$Age1, dat$Age2))) + 
        scale_y_continuous(limits = c(0, max(dat$Length1_obs, dat$Length2_obs))) +
        plot_theme() + scale_colour_manual(values = plot_palette) +
        guides(color = guide_legend(title = "Key")) + theme(legend.position = c(0.91, 0.1))
    if (file_suffix %in% c("eps", ".eps")) 
        cairo_ps(paste0(file_name, ".eps"), width = 10, height = 10)
    if (file_suffix %in% c("png", ".png")) 
        png(paste0(file_name, ".png"), width = 10, height = 10, 
            units = "in", res = 400)
    print(p)
    dev.off()
}
