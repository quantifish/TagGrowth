require(TagGrowth)

d <- read_all_simulations()

psize <- c(12, 8)

# Summary
aggregate(Estimate ~ Sex + Parameter + Estimation + Simulation, data = d, FUN = length)
aggregate(Estimate ~ Sex + Parameter + Estimation + Simulation, data = d, FUN = median)

# SDs
d1 <- subset(d, subset = d$Parameter == "L0")
d2 <- subset(d1, subset = d1$Simulation == "k and z")
aggregate(Estimate ~ Sex + Estimation, data = d2, FUN = sd)


d1 <- subset(d, subset = d$Parameter == "bmean")
d1$Estimate <- d1$Estimate * 52.15 # Convert to years-1
d1$Truth <- d1$Truth * 52.15       # Convert to years-1
d2 <- subset(d1, subset = d$Sex == "Male")
d3 <- subset(d2, subset = d$Estimation == "z")
d3


################################################################################
# FIG1
plot_theme_b <- function (base_size = 12, base_family = "") 
{
    theme_grey(base_size = base_size, base_family = base_family) %+replace% 
        theme(axis.text = element_text(size = rel(1)), axis.title = element_text(size = rel(1.2)), 
            axis.ticks = element_line(colour = "black"), strip.text = element_text(size = rel(1.2)), 
            legend.key = element_rect(colour = "grey80"), panel.background = element_rect(fill = "white", 
                colour = NA), panel.border = element_rect(fill = NA, 
                colour = "grey50"), panel.grid.major = element_line(colour = "grey90", 
                size = 0.2), panel.grid.minor = element_line(colour = "grey98", 
                size = 0.5), strip.background = element_rect(fill = "grey80", 
                colour = "grey50", size = 0.2))
}

# All at once
d1 <- d[!d$Parameter == "psi",]
d1$Estimate[d1$Parameter == "sd_z" & d1$Estimation %in% c("none","k")] <- NA
d1$Estimate[d1$Parameter == "sd_bdev" & d1$Estimation %in% c("none","z")] <- NA
d1$Parameter <- as.character(d1$Parameter)
d1$Simulation <- as.character(d1$Simulation)

# Linf
psi <- 0.000001
d1$Estimate[d1$Parameter == "gamma"] <- (d1$Estimate[d1$Parameter == "gamma"] * d1$Estimate[d1$Parameter == "bmean"]^psi) / d1$Estimate[d1$Parameter == "bmean"]
d1$Truth[d1$Parameter == "gamma" & d1$Sex == "Female"] <- 180.2
d1$Truth[d1$Parameter == "gamma" & d1$Sex == "Male"] <- 169.07
d1 <- d1[!(d1$Parameter == "gamma" & d1$Estimate > 400),]

# mu_k
d1$Estimate[d1$Parameter == "bmean"] <- d1$Estimate[d1$Parameter == "bmean"] * 52.15 # Convert to years-1
d1$Truth[d1$Parameter == "bmean"] <- d1$Truth[d1$Parameter == "bmean"] * 52.15       # Convert to years-1

d1$Simulation[d1$Simulation == "k and z"] <- "k~and~z"

d1$Parameter[d1$Parameter == "gamma"] <- "mu[L[infinity]]"
d1$Parameter[d1$Parameter == "bmean"] <- "mu[k]"
d1$Parameter[d1$Parameter == "L0"] <- "L[0]"
d1$Parameter[d1$Parameter == "sd_bdev"] <- "sigma[k]"
d1$Parameter[d1$Parameter == "sd_z"] <- "sigma[z]"
d1$Parameter[d1$Parameter == "sd_obs"] <- "c[obs]"

dl <- aggregate(Estimate ~ Sex + Parameter + Estimation + Simulation, data = d1, FUN = length)
dl$x <- aggregate(Estimate ~ Sex + Parameter + Estimation + Simulation, data = d1, FUN = mean)$Estimate
dl$x[dl$Parameter == "c[obs]"] <- 0.02
dl$Estimate[dl$Parameter != "c[obs]"] <- ""
bit <- dl[dl$Parameter == "c[obs]",]
bit$Sex <- "Male"
bit$Estimate <- ""
bit$x <- 0.01
dl <- rbind(dl, bit)

d1$Parameter <- factor(d1$Parameter, levels = c("mu[k]", "mu[L[infinity]]", "L[0]", "sigma[k]", "sigma[z]", "c[obs]"))
d1$Simulation <- factor(d1$Simulation, levels = c("none", "k", "z", "k~and~z"))
truth <- aggregate(Truth ~ Sex + Parameter + Estimation + Simulation, data = d1, FUN = max)

dsim <- d1

p <- ggplot(data = d1, aes(x = Estimation, y = Estimate)) +
    geom_hline(data = truth, aes(colour = factor(Sex), yintercept = Truth), size = 1, linetype = 3) +
    facet_grid(Parameter ~ Simulation, scales = "free", labeller = label_parsed) +
    #geom_jitter(aes(color = factor(Sex))) +
    #geom_point(aes(color = factor(Sex))) +
    #geom_boxplot(aes(fill = factor(Sex))) +
    geom_violin(aes(fill = factor(Sex))) +
    geom_text(data = dl, size = 4, aes(y = x, label = Estimate)) + 
    xlab("\nEstimation model") + ylab("Estimate\n") + plot_theme_b() +
    scale_fill_discrete(name = "Sex") + theme(legend.position = "right")

z <- ggplot_gtable(ggplot_build(p)) # get gtable object
dev.off()
# add label for top strip
z <- gtable_add_rows(z, z$heights[[3]], 2)
z <- gtable_add_grob(x = z, 
  grobs = list(rectGrob(gp = gpar(col = NA, fill = "white")),
  textGrob("Simulation model", gp = gpar(fontsize = 14))),
  t=3, l=4, b=3, r=10, name = paste(runif(2)))
z <- gtable_add_cols(z, unit(1/8, "line"), 7) # add margins
z <- gtable_add_rows(z, unit(1/8, "line"), 3)
z$layout$clip[z$layout$name=="panel"] <- "off" # Code to override clipping

cairo_ps("FIG1.eps", width = psize[1], height = psize[2])
grid.draw(z)
dev.off()
################################################################################


# Power analysis
d <- read_all_powers()

################################################################################
# FIG2
# All at once
d1 <- d[!d$Parameter == "psi",]
names(d1) <- c("Estimation", "Simulation", "Parameter", "Sex", "Estimate", "Truth")
d1$Estimate[d1$Parameter == "sd_z" & d1$Estimation %in% c("none","k")] <- NA
d1$Estimate[d1$Parameter == "sd_bdev" & d1$Estimation %in% c("none","z")] <- NA
d1$Estimation <- as.character(d1$Estimation)
d1$Parameter <- as.character(d1$Parameter)
d1$Simulation <- as.character(d1$Simulation)

# Linf
psi <- 0.000001
d1$Estimate[d1$Parameter == "gamma"] <- (d1$Estimate[d1$Parameter == "gamma"] * d1$Estimate[d1$Parameter == "bmean"]^psi) / d1$Estimate[d1$Parameter == "bmean"]
d1$Truth[d1$Parameter == "gamma" & d1$Sex == "Female"] <- 180.2
d1$Truth[d1$Parameter == "gamma" & d1$Sex == "Male"] <- 169.07
d1 <- d1[!(d1$Parameter == "gamma" & d1$Estimate > 400),]
d1 <- d1[!(d1$Parameter == "L0" & d1$Estimate < -200),]

# mu_k
d1$Estimate[d1$Parameter == "bmean"] <- d1$Estimate[d1$Parameter == "bmean"] * 52.15 # Convert to years-1
d1$Truth[d1$Parameter == "bmean"] <- d1$Truth[d1$Parameter == "bmean"] * 52.15       # Convert to years-1

d1$Parameter[d1$Parameter == "gamma"] <- "mu[L[infinity]]"
d1$Parameter[d1$Parameter == "bmean"] <- "mu[k]"
d1$Parameter[d1$Parameter == "L0"] <- "L[0]"
d1$Parameter[d1$Parameter == "sd_bdev"] <- "sigma[k]"
d1$Parameter[d1$Parameter == "sd_z"] <- "sigma[z]"
d1$Parameter[d1$Parameter == "sd_obs"] <- "c[obs]"

dl <- aggregate(Estimate ~ Sex + Parameter + Estimation + Simulation, data = d1, FUN = length)
dl$x <- aggregate(Estimate ~ Sex + Parameter + Estimation + Simulation, data = d1, FUN = mean)$Estimate
dl$x[dl$Parameter == "c[obs]"] <- 0.02
dl$Estimate[dl$Parameter != "c[obs]"] <- ""
bit <- dl[dl$Parameter == "c[obs]",]
bit$Sex <- "Male"
bit$Estimate <- ""
bit$x <- 0.01
dl <- rbind(dl, bit)

d1$Parameter <- factor(d1$Parameter, levels = c("mu[k]", "mu[L[infinity]]", "L[0]", "sigma[k]", "sigma[z]", "c[obs]"))
d1$Simulation <- factor(d1$Simulation, levels = c("50", "100", "250", "500"))
d1$Estimation <- factor(d1$Estimation, levels = c("none", "k", "z", "k and z"))
truth <- aggregate(Truth ~ Sex + Parameter + Estimation + Simulation, data = d1, FUN = max)

dpow <- d1

p <- ggplot(data = d1, aes(x = Estimation, y = Estimate)) +
    geom_hline(data = truth, aes(colour = factor(Sex), yintercept = Truth), size = 1, linetype = 3) +
    facet_grid(Parameter ~ Simulation, scales = "free", labeller = label_parsed) +
    #geom_jitter(aes(color = factor(Sex))) +
    #geom_point(aes(color = factor(Sex))) +
    #geom_boxplot(aes(fill = factor(Sex))) +
    geom_violin(aes(fill = factor(Sex))) +
    geom_text(data = dl, size = 4, aes(y = x, label = Estimate)) + 
    xlab("\nEstimation model") + ylab("Estimate\n") + plot_theme_b() +
    scale_fill_discrete(name = "Sex") + theme(legend.position = "right")
print(p)

z <- ggplot_gtable(ggplot_build(p)) # get gtable object
dev.off()
# add label for top strip
z <- gtable_add_rows(z, z$heights[[3]], 2)
z <- gtable_add_grob(x = z, 
  grobs = list(rectGrob(gp = gpar(col = NA, fill = "white")),
  textGrob("Sample size", gp = gpar(fontsize = 14))),
  t=3, l=4, b=3, r=10, name = paste(runif(2)))
z <- gtable_add_cols(z, unit(1/8, "line"), 7) # add margins
z <- gtable_add_rows(z, unit(1/8, "line"), 3)
z$layout$clip[z$layout$name=="panel"] <- "off" # Code to override clipping

cairo_ps("FIG2.eps", width = psize[1], height = psize[2])
grid.draw(z)
dev.off()
################################################################################







################################################################################
# mu_k - fig. 1
d1 <- subset(d, subset = d$Parameter == "bmean")
d1$Estimate <- d1$Estimate * 52.15 # Convert to years-1
d1$Truth <- d1$Truth * 52.15       # Convert to years-1
dl <- aggregate(Estimate ~ Sex + Parameter + Estimation + Simulation, data = d1, FUN = length)
dl$lab <- dl$Estimate
dl$x <- rep(0.25, nrow(dl))
dl$y <- rep(120, nrow(dl))
p <- ggplot(data = d1, aes(x = Estimate)) +
        geom_text(data = dl, aes(x = x, y = y, label = lab), vjust = 1) +
        geom_histogram(colour = "black", fill = "grey") +
        facet_grid(Estimation + Sex ~ Simulation, scales = "fixed") +
        geom_vline(aes(xintercept = Truth), size = 1.5, colour = "red", alpha = 0.6) +
        scale_y_continuous(limits = c(0, 130)) +
        xlab(expression(mu[k])) + ylab("Frequency\n") +
        plot_theme() + theme(axis.title.x = element_text(vjust = -0.5))
z <- ggplot_gtable(ggplot_build(p)) # get gtable object
dev.off()
# add label for right strip
z <- gtable_add_cols(z, z$widths[[3]])
z <- gtable_add_grob(x = z, 
  list(rectGrob(vjust = 1, gp = gpar(col = NA, fill = "white")),
  textGrob("Estimation model", vjust = 1.5, rot = -90, gp = gpar(fontsize = 16))),
  t = 4, l = 14, b = 18, clip = "off", name = paste(runif(2)))
# add label for top strip
z <- gtable_add_rows(z, z$heights[[3]], 2)
z <- gtable_add_grob(x = z, 
  grobs = list(rectGrob(gp = gpar(col = NA, fill = "white")),
  textGrob("Simulation model", gp = gpar(fontsize = 16))),
  t=3, l=4, b=3, r=10, name = paste(runif(2)))
z <- gtable_add_cols(z, unit(1/8, "line"), 7) # add margins
z <- gtable_add_rows(z, unit(1/8, "line"), 3)
z$layout$clip[z$layout$name=="panel"] <- "off" # Code to override clipping
cairo_ps("FIG1.eps", width = psize[1], height = psize[2])
grid.draw(z)
dev.off()
################################################################################


################################################################################
# Linf - fig. 2
d1 <- subset(d, subset = d$Parameter == "gamma")
d2 <- subset(d, subset = d$Parameter == "bmean")
d1$Estimate <- d1$Estimate * 52.15 # Convert to years-1
d2$Estimate <- d2$Estimate * 52.15 # Convert to years-1
psi <- 0.000001
d3 <- d1
d3$Estimate <- (d1$Estimate * d2$Estimate^psi) / d2$Estimate
d3$Truth[d3$Sex == "Female"] <- 180.2
d3$Truth[d3$Sex == "Male"] <- 169.07

dl <- aggregate(Estimate ~ Sex + Parameter + Estimation + Simulation, data = d3, FUN = length)
dl$lab <- dl$Estimate
dl$x <- rep(290, nrow(dl))
dl$y <- rep(110, nrow(dl))

p <- ggplot(data = d3, aes(x = Estimate)) +
        geom_text(data = dl, aes(x = x, y = y, label = lab), vjust = 1) +
        geom_histogram(colour = "black", fill = "grey") +
        facet_grid(Estimation + Sex ~ Simulation, scales = "fixed") +
        geom_vline(aes(xintercept = Truth), size = 1.5, colour = "red", alpha = 0.6) +
        xlab(expression(mu[L[infinity]])) + ylab("Frequency\n") +
        scale_x_continuous(limits = c(120, 300)) +
        plot_theme() + theme(axis.title.x = element_text(vjust = -0.5))# +
        #labs(title = "Estimation model\n")
z <- ggplot_gtable(ggplot_build(p)) # get gtable object
dev.off()
# add label for right strip
z <- gtable_add_cols(z, z$widths[[3]])
z <- gtable_add_grob(x = z,
  list(rectGrob(vjust = 1, gp = gpar(col = NA, fill = "white")),
  textGrob("Estimation model", vjust = 1.5, rot = -90, gp = gpar(fontsize = 16))),
  t = 4, l = 14, b = 18, clip = "off", name = paste(runif(2)))
# add label for top strip
z <- gtable_add_rows(z, z$heights[[3]], 2)
z <- gtable_add_grob(x = z, 
  #list(rectGrob(gp = gpar(col = NA, fill = gray(0.5))),
  grobs = list(rectGrob(gp = gpar(col = NA, fill = "white")),
  textGrob("Simulation model", gp = gpar(fontsize = 16))),
  t=3, l=4, b=3, r=10, name = paste(runif(2)))
z <- gtable_add_cols(z, unit(1/8, "line"), 7) # add margins
z <- gtable_add_rows(z, unit(1/8, "line"), 3)
z$layout$clip[z$layout$name=="panel"] <- "off" # Code to override clipping

cairo_ps("FIG2.eps", width = psize[1], height = psize[2])
#png("sim_linf.png", width = psize[1], height = psize[2], units = "in", res = 300)
grid.draw(z)
dev.off()
################################################################################


################################################################################
# L0 - fig. 3
d1 <- subset(d, subset = d$Parameter == "L0")
dl <- aggregate(Estimate ~ Sex + Parameter + Estimation + Simulation, data = d1, FUN = length)
dl$lab <- dl$Estimate
dl$x <- rep(43, nrow(dl))
dl$y <- rep(95, nrow(dl))
p <- ggplot(data = d1, aes(x = Estimate)) +
        geom_text(data = dl, aes(x = x, y = y, label = lab), vjust = 1) +
        geom_histogram(colour = "black", fill = "grey") +
        facet_grid(Estimation + Sex ~ Simulation, scales = "fixed") +
        geom_vline(aes(xintercept = Truth), size = 1.5, colour = "red", alpha = 0.6) +
        xlab(expression(L[0])) + ylab("Frequency\n") +
        scale_x_continuous(limits = c(-75, 50)) +
        plot_theme() + theme(axis.title.x = element_text(vjust = -0.5))
z <- ggplot_gtable(ggplot_build(p)) # get gtable object
dev.off()
# add label for right strip
z <- gtable_add_cols(z, z$widths[[3]])
z <- gtable_add_grob(x = z, 
  list(rectGrob(vjust = 1, gp = gpar(col = NA, fill = "white")),
  textGrob("Estimation model", vjust = 1.5, rot = -90, gp = gpar(fontsize = 16))),
  t = 4, l = 14, b = 18, clip = "off", name = paste(runif(2)))
# add label for top strip
z <- gtable_add_rows(z, z$heights[[3]], 2)
z <- gtable_add_grob(x = z, 
  grobs = list(rectGrob(gp = gpar(col = NA, fill = "white")),
  textGrob("Simulation model", gp = gpar(fontsize = 16))),
  t=3, l=4, b=3, r=10, name = paste(runif(2)))
z <- gtable_add_cols(z, unit(1/8, "line"), 7) # add margins
z <- gtable_add_rows(z, unit(1/8, "line"), 3)
z$layout$clip[z$layout$name=="panel"] <- "off" # Code to override clipping
cairo_ps("FIG3.eps", width = psize[1], height = psize[2])
grid.draw(z)
dev.off()
################################################################################


################################################################################
# sd_kdev - fig. 4
d1 <- subset(d, subset = d$Parameter == "sd_bdev")
d1 <- subset(d1, subset = d1$Simulation %in% c("k", "k and z"))
dl <- aggregate(Estimate ~ Sex + Parameter + Estimation + Simulation, data = d1, FUN = length)
dl$lab <- dl$Estimate
dl$x <- rep(0.38, nrow(dl))
dl$y <- rep(105, nrow(dl))
p <- ggplot(data = d1, aes(x = Estimate)) +
        geom_text(data = dl, aes(x = x, y = y, label = lab), vjust = 1) +
        geom_histogram(colour = "black", fill = "grey") +
        facet_grid(Estimation + Sex ~ Simulation, scales = "fixed") +
        scale_x_continuous(limits = c(0, 0.41)) +
        geom_vline(aes(xintercept = Truth), size = 1.5, colour = "red", alpha = 0.6) +
        xlab(expression(sigma[k])) + ylab("Frequency\n") +
        plot_theme() + theme(axis.title.x = element_text(vjust = -0.5))
z <- ggplot_gtable(ggplot_build(p)) # get gtable object
dev.off()
# add label for right strip
z <- gtable_add_cols(z, z$widths[[3]])
z <- gtable_add_grob(x = z, 
  list(rectGrob(vjust = 1, gp = gpar(col = NA, fill = "white")),
  textGrob("Estimation model", vjust = 1.5, rot = -90, gp = gpar(fontsize = 16))),
  t = 4, l = 10, b = 18, clip = "off", name = paste(runif(2)))
# add label for top strip
z <- gtable_add_rows(z, z$heights[[3]], 2)
z <- gtable_add_grob(x = z, 
  grobs = list(rectGrob(gp = gpar(col = NA, fill = "white")),
  textGrob("Simulation model", gp = gpar(fontsize = 16))),
  t=3, l=2, b=3, r=10, name = paste(runif(2)))
z <- gtable_add_cols(z, unit(1/8, "line"), 7) # add margins
z <- gtable_add_rows(z, unit(1/8, "line"), 3)
z$layout$clip[z$layout$name=="panel"] <- "off" # Code to override clipping
cairo_ps("FIG4.eps", width = psize[1]/2, height = psize[2])
grid.draw(z)
dev.off()
################################################################################


################################################################################
# sd_z - fig. 5
d1 <- subset(d, subset = d$Parameter == "sd_z")
d1 <- subset(d1, subset = d1$Simulation %in% c("z", "k and z"))
dl <- aggregate(Estimate ~ Sex + Parameter + Estimation + Simulation, data = d1, FUN = length)
dl$lab <- dl$Estimate
dl$x <- rep(0.48, nrow(dl))
dl$y <- rep(85, nrow(dl))
p <- ggplot(data = d1, aes(x = Estimate)) +
        geom_text(data = dl, aes(x = x, y = y, label = lab), vjust = 1) +
        geom_histogram(colour = "black", fill = "grey") +
        facet_grid(Estimation ~ Simulation, scales = "free_x") +
        geom_vline(aes(xintercept = Truth), size = 1.5, colour = "red", alpha = 0.6) +
        xlab(expression(sigma[z])) + ylab("Frequency\n") +
        scale_x_continuous(limits = c(0, 0.51)) +
        plot_theme() + theme(axis.title.x = element_text(vjust = -0.5))
z <- ggplot_gtable(ggplot_build(p)) # get gtable object
dev.off()
# add label for right strip
z <- gtable_add_cols(z, z$widths[[3]])
z <- gtable_add_grob(x = z, 
  list(rectGrob(vjust = 1, gp = gpar(col = NA, fill = "white")),
  textGrob("Estimation model", vjust = 1.5, rot = -90, gp = gpar(fontsize = 16))),
  t = 10, l = 9, b = 4, clip = "off", name = paste(runif(2)))
# add label for top strip
z <- gtable_add_rows(z, z$heights[[3]], 2)
z <- gtable_add_grob(x = z, 
  grobs = list(rectGrob(gp = gpar(col = NA, fill = "white")),
  textGrob("Simulation model", gp = gpar(fontsize = 16))),
  t=3, l=7, b=3, r=2, name = paste(runif(2)))
z <- gtable_add_cols(z, unit(1/8, "line"), 7) # add margins
z <- gtable_add_rows(z, unit(1/8, "line"), 3)
z$layout$clip[z$layout$name=="panel"] <- "off" # Code to override clipping
cairo_ps("FIG5.eps", width = psize[1]/2, height = psize[2]/1.7)
grid.draw(z)
dev.off()
################################################################################


# gamma
d1 <- subset(d, subset = d$Parameter == "gamma")
d1$Estimate <- d1$Estimate * 52.15 # Convert to years-1
d1$Truth <- d1$Truth * 52.15       # Convert to years-1
dl <- aggregate(Estimate ~ Sex + Parameter + Estimation + Simulation, data = d1, FUN = length)
dl$lab <- dl$Estimate
dl$x <- rep(35, nrow(dl))
dl$y <- rep(135, nrow(dl))
p <- ggplot(data = d1, aes(x = Estimate)) +
        geom_text(data = dl, aes(x = x, y = y, label = lab), vjust = 1) +
        geom_histogram(colour = "black", fill = "grey") +
        facet_grid(Estimation + Sex ~ Simulation, scales = "fixed") +
        geom_vline(aes(xintercept = Truth), size = 1.5, colour = "red", alpha = 0.6) +
        xlab(expression(gamma)) + ylab("Frequency\n") +
        plot_theme() + theme(axis.title.x = element_text(vjust = -0.5))
png("sim_gamma.png", width = psize[1], height = psize[2], units = "in", res = 300)
print(p)
dev.off()


# sd_obs
d1 <- subset(d, subset = d$Parameter == "sd_obs")
dl <- aggregate(Estimate ~ Sex + Parameter + Estimation + Simulation, data = d1, FUN = length)
dl$lab <- dl$Estimate
dl$x <- rep(43, nrow(dl))
dl$y <- rep(95, nrow(dl))
p <- ggplot(data = d1, aes(x = Estimate)) +
        geom_text(data = dl, aes(x = x, y = y, label = lab), vjust = 1) +
        geom_histogram(colour = "black", fill = "grey") +
        facet_grid(Estimation ~ Simulation, scales = "free_x") +
        geom_vline(aes(xintercept = Truth), size = 1.5, colour = "red", alpha = 0.6) +
        xlab(expression(c[obs])) + ylab("Frequency\n") +
        plot_theme() + theme(axis.title.x = element_text(vjust = -0.5)) +
        labs(title = "Estimation model\n")
png("sim_sd_obs.png", width = psize[1], height = psize[2]/2, units = "in", res = 300)
print(p)
dev.off()



# Power analysis
d <- read_all_powers()

################################################################################
# mu_k - fig. 6
d1 <- subset(d, subset = d$Parameter == "bmean")
d1$Estimate <- d1$Estimate * 52.15 # Convert to years-1
d1$Truth <- d1$Truth * 52.15       # Convert to years-1
dl <- aggregate(Estimate ~ Sex + Parameter + Estimation + Simulation, data = d1, FUN = length)
dl$lab <- dl$Estimate
dl$x <- rep(0.25, nrow(dl))
dl$y <- rep(120, nrow(dl))
p <- ggplot(data = d1, aes(x = Estimate)) +
        geom_text(data = dl, aes(x = x, y = y, label = lab), vjust = 1) +
        geom_histogram(colour = "black", fill = "grey") +
        facet_grid(Estimation + Sex ~ Simulation, scales = "fixed") +
        geom_vline(aes(xintercept = Truth), size = 1.5, colour = "red", alpha = 0.6) +
        scale_y_continuous(limits = c(0, 130)) +
        xlab(expression(mu[k])) + ylab("Frequency\n") +
        plot_theme() + theme(axis.title.x = element_text(vjust = -0.5))
z <- ggplot_gtable(ggplot_build(p)) # get gtable object
dev.off()
# add label for right strip
z <- gtable_add_cols(z, z$widths[[3]])
z <- gtable_add_grob(x = z,
  list(rectGrob(vjust = 1, gp = gpar(col = NA, fill = "white")),
  textGrob("Sample size", vjust = 1.5, rot = -90, gp = gpar(fontsize = 16))),
  t = 4, l = 14, b = 18, clip = "off", name = paste(runif(2)))
# add label for top strip
z <- gtable_add_rows(z, z$heights[[3]], 2)
z <- gtable_add_grob(x = z, 
  grobs = list(rectGrob(gp = gpar(col = NA, fill = "white")),
  textGrob("Estimation model", gp = gpar(fontsize = 16))),
  t=3, l=4, b=3, r=10, name = paste(runif(2)))
z <- gtable_add_cols(z, unit(1/8, "line"), 7) # add margins
z <- gtable_add_rows(z, unit(1/8, "line"), 3)
z$layout$clip[z$layout$name=="panel"] <- "off" # Code to override clipping
cairo_ps("FIG6.eps", width = psize[1], height = psize[2])
grid.draw(z)
dev.off()
################################################################################


################################################################################
# Linf - fig. 7
d1 <- subset(d, subset = d$Parameter == "gamma")
d2 <- subset(d, subset = d$Parameter == "bmean")
d1$Estimate <- d1$Estimate * 52.15 # Convert to years-1
d2$Estimate <- d2$Estimate * 52.15 # Convert to years-1
psi <- 0.000001
d3 <- d1
d3$Estimate <- (d1$Estimate * d2$Estimate^psi) / d2$Estimate
d3$Truth[d3$Sex == "Female"] <- 180.2
d3$Truth[d3$Sex == "Male"] <- 169.07

dl <- aggregate(Estimate ~ Sex + Parameter + Estimation + Simulation, data = d3, FUN = length)
dl$lab <- dl$Estimate
dl$x <- rep(290, nrow(dl))
dl$y <- rep(110, nrow(dl))

p <- ggplot(data = d3, aes(x = Estimate)) +
        geom_text(data = dl, aes(x = x, y = y, label = lab), vjust = 1) +
        geom_histogram(colour = "black", fill = "grey") +
        facet_grid(Estimation + Sex ~ Simulation, scales = "fixed") +
        geom_vline(aes(xintercept = Truth), size = 1.5, colour = "red", alpha = 0.6) +
        xlab(expression(mu[L[infinity]])) + ylab("Frequency\n") +
        scale_x_continuous(limits = c(120, 300)) +
        plot_theme() + theme(axis.title.x = element_text(vjust = -0.5))# +
        #labs(title = "Estimation model\n")
z <- ggplot_gtable(ggplot_build(p)) # get gtable object
dev.off()
# add label for right strip
z <- gtable_add_cols(z, z$widths[[3]])
z <- gtable_add_grob(x = z,
  list(rectGrob(vjust = 1, gp = gpar(col = NA, fill = "white")),
  textGrob("Sample size", vjust = 1.5, rot = -90, gp = gpar(fontsize = 16))),
  t = 4, l = 14, b = 18, clip = "off", name = paste(runif(2)))
# add label for top strip
z <- gtable_add_rows(z, z$heights[[3]], 2)
z <- gtable_add_grob(x = z, 
  #list(rectGrob(gp = gpar(col = NA, fill = gray(0.5))),
  grobs = list(rectGrob(gp = gpar(col = NA, fill = "white")),
  textGrob("Estimation model", gp = gpar(fontsize = 16))),
  t=3, l=4, b=3, r=10, name = paste(runif(2)))
z <- gtable_add_cols(z, unit(1/8, "line"), 7) # add margins
z <- gtable_add_rows(z, unit(1/8, "line"), 3)
z$layout$clip[z$layout$name=="panel"] <- "off" # Code to override clipping

cairo_ps("FIG7.eps", width = psize[1], height = psize[2])
grid.draw(z)
dev.off()
################################################################################


################################################################################
# L0 - fig. 8
d1 <- subset(d, subset = d$Parameter == "L0")
dl <- aggregate(Estimate ~ Sex + Parameter + Estimation + Simulation, data = d1, FUN = length)
dl$lab <- dl$Estimate
dl$x <- rep(43, nrow(dl))
dl$y <- rep(95, nrow(dl))
p <- ggplot(data = d1, aes(x = Estimate)) +
        geom_text(data = dl, aes(x = x, y = y, label = lab), vjust = 1) +
        geom_histogram(colour = "black", fill = "grey") +
        facet_grid(Estimation + Sex ~ Simulation, scales = "fixed") +
        geom_vline(aes(xintercept = Truth), size = 1.5, colour = "red", alpha = 0.6) +
        xlab(expression(L[0])) + ylab("Frequency\n") +
        scale_x_continuous(limits = c(-75, 50)) +
        plot_theme() + theme(axis.title.x = element_text(vjust = -0.5))
z <- ggplot_gtable(ggplot_build(p)) # get gtable object
dev.off()
# add label for right strip
z <- gtable_add_cols(z, z$widths[[3]])
z <- gtable_add_grob(x = z, 
  list(rectGrob(vjust = 1, gp = gpar(col = NA, fill = "white")),
  textGrob("Sample size", vjust = 1.5, rot = -90, gp = gpar(fontsize = 16))),
  t = 4, l = 14, b = 18, clip = "off", name = paste(runif(2)))
# add label for top strip
z <- gtable_add_rows(z, z$heights[[3]], 2)
z <- gtable_add_grob(x = z, 
  grobs = list(rectGrob(gp = gpar(col = NA, fill = "white")),
  textGrob("Estimation model", gp = gpar(fontsize = 16))),
  t=3, l=4, b=3, r=10, name = paste(runif(2)))
z <- gtable_add_cols(z, unit(1/8, "line"), 7) # add margins
z <- gtable_add_rows(z, unit(1/8, "line"), 3)
z$layout$clip[z$layout$name=="panel"] <- "off" # Code to override clipping
cairo_ps("FIG8.eps", width = psize[1], height = psize[2])
grid.draw(z)
dev.off()
################################################################################


################################################################################
# sd_kdev - fig. 9
d1 <- subset(d, subset = d$Parameter == "sd_bdev")
d1 <- subset(d1, subset = d1$Simulation %in% c("k", "k and z"))
dl <- aggregate(Estimate ~ Sex + Parameter + Estimation + Simulation, data = d1, FUN = length)
dl$lab <- dl$Estimate
dl$x <- rep(0.38, nrow(dl))
dl$y <- rep(105, nrow(dl))
p <- ggplot(data = d1, aes(x = Estimate)) +
        geom_text(data = dl, aes(x = x, y = y, label = lab), vjust = 1) +
        geom_histogram(colour = "black", fill = "grey") +
        facet_grid(Estimation + Sex ~ Simulation, scales = "fixed") +
        scale_x_continuous(limits = c(0, 0.41)) +
        geom_vline(aes(xintercept = Truth), size = 1.5, colour = "red", alpha = 0.6) +
        xlab(expression(sigma[k])) + ylab("Frequency\n") +
        plot_theme() + theme(axis.title.x = element_text(vjust = -0.5))
z <- ggplot_gtable(ggplot_build(p)) # get gtable object
dev.off()
# add label for right strip
z <- gtable_add_cols(z, z$widths[[3]])
z <- gtable_add_grob(x = z, 
  list(rectGrob(vjust = 1, gp = gpar(col = NA, fill = "white")),
  textGrob("Sample size", vjust = 1.5, rot = -90, gp = gpar(fontsize = 16))),
  t = 4, l = 10, b = 18, clip = "off", name = paste(runif(2)))
# add label for top strip
z <- gtable_add_rows(z, z$heights[[3]], 2)
z <- gtable_add_grob(x = z, 
  grobs = list(rectGrob(gp = gpar(col = NA, fill = "white")),
  textGrob("Estimation model", gp = gpar(fontsize = 16))),
  t=3, l=2, b=3, r=10, name = paste(runif(2)))
z <- gtable_add_cols(z, unit(1/8, "line"), 7) # add margins
z <- gtable_add_rows(z, unit(1/8, "line"), 3)
z$layout$clip[z$layout$name=="panel"] <- "off" # Code to override clipping
cairo_ps("FIG9.eps", width = psize[1]/2, height = psize[2])
grid.draw(z)
dev.off()
################################################################################


################################################################################
# sd_z - fig. 10
d1 <- subset(d, subset = d$Parameter == "sd_z")
d1 <- subset(d1, subset = d1$Simulation %in% c("z", "k and z"))
dl <- aggregate(Estimate ~ Sex + Parameter + Estimation + Simulation, data = d1, FUN = length)
dl$lab <- dl$Estimate
dl$x <- rep(0.48, nrow(dl))
dl$y <- rep(85, nrow(dl))
p <- ggplot(data = d1, aes(x = Estimate)) +
        geom_text(data = dl, aes(x = x, y = y, label = lab), vjust = 1) +
        geom_histogram(colour = "black", fill = "grey") +
        facet_grid(Estimation ~ Simulation, scales = "free_x") +
        geom_vline(aes(xintercept = Truth), size = 1.5, colour = "red", alpha = 0.6) +
        xlab(expression(sigma[z])) + ylab("Frequency\n") +
        scale_x_continuous(limits = c(0, 0.51)) +
        plot_theme() + theme(axis.title.x = element_text(vjust = -0.5))
z <- ggplot_gtable(ggplot_build(p)) # get gtable object
dev.off()
# add label for right strip
z <- gtable_add_cols(z, z$widths[[3]])
z <- gtable_add_grob(x = z, 
  list(rectGrob(vjust = 1, gp = gpar(col = NA, fill = "white")),
  textGrob("Sample size", vjust = 1.5, rot = -90, gp = gpar(fontsize = 16))),
  t = 10, l = 9, b = 4, clip = "off", name = paste(runif(2)))
# add label for top strip
z <- gtable_add_rows(z, z$heights[[3]], 2)
z <- gtable_add_grob(x = z, 
  grobs = list(rectGrob(gp = gpar(col = NA, fill = "white")),
  textGrob("Estimation model", gp = gpar(fontsize = 16))),
  t=3, l=7, b=3, r=2, name = paste(runif(2)))
z <- gtable_add_cols(z, unit(1/8, "line"), 7) # add margins
z <- gtable_add_rows(z, unit(1/8, "line"), 3)
z$layout$clip[z$layout$name=="panel"] <- "off" # Code to override clipping
cairo_ps("FIG10.eps", width = psize[1]/2, height = psize[2]/1.7)
grid.draw(z)
dev.off()
################################################################################
