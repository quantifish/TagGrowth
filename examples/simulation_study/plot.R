require(TagGrowth)
d <- read_all_simulations()

psize <- c(12, 8)

# Summary
aggregate(Estimate ~ Sex + Parameter + Power + Scenario, data = d, FUN = length)
aggregate(Estimate ~ Sex + Parameter + Power + Scenario, data = d, FUN = median)

# SDs
d1 <- subset(d, subset = d$Parameter == "L0")
d2 <- subset(d1, subset = d1$Scenario == "k and z")
aggregate(Estimate ~ Sex + Power, data = d2, FUN = sd)


# TESTING
test<-data.frame(x=1:20, y=21:40, facet.a=rep(c(1,2),10), facet.b=rep(c(1,2), each=20))
p <- qplot(data=test, x=x, y=y, facets=facet.b~facet.a)

# get gtable object
z <- ggplot_gtable(ggplot_build(p))

# add label for right strip
z <- gtable_add_cols(z, z$widths[[7]])
z <- gtable_add_grob(z, 
  list(rectGrob(gp = gpar(col = NA, fill = gray(0.5))),
  textGrob("Variable 1", rot = -90, gp = gpar(col = gray(1)))),
  4, 8, 6, name = paste(runif(2)))

# add label for top strip
z <- gtable_add_rows(z, z$heights[[3]], 2)
z <- gtable_add_grob(z, 
  list(rectGrob(gp = gpar(col = NA, fill = gray(0.5))),
  textGrob("Variable 2", gp = gpar(col = gray(1)))),
  3, 4, 3, 6, name = paste(runif(2)))

# add margins
z <- gtable_add_cols(z, unit(1/8, "line"), 7)
z <- gtable_add_rows(z, unit(1/8, "line"), 3)

# draw it
grid.newpage()
grid.draw(z)



# Linf - fig. 6
d1 <- subset(d, subset = d$Parameter == "gamma")
d2 <- subset(d, subset = d$Parameter == "bmean")
d1$Estimate <- d1$Estimate * 52.15 # Convert to years-1
d2$Estimate <- d2$Estimate * 52.15 # Convert to years-1
psi <- 0.000001
d3 <- d1
d3$Estimate <- (d1$Estimate * d2$Estimate^psi) / d2$Estimate
d3$Truth[d3$Sex == "Female"] <- 180.2
d3$Truth[d3$Sex == "Male"] <- 169.07

dl <- aggregate(Estimate ~ Sex + Parameter + Power + Scenario, data = d3, FUN = length)
dl$lab <- dl$Estimate
dl$x <- rep(290, nrow(dl))
dl$y <- rep(110, nrow(dl))

p <- ggplot(data = d3, aes(x = Estimate)) +
        geom_text(data = dl, aes(x = x, y = y, label = lab), vjust = 1) +
        geom_histogram(colour = "black", fill = "grey") +
        facet_grid(Power + Sex ~ Scenario, scales = "fixed") +
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

cairo_ps("FIG6.eps", width = psize[1], height = psize[2])
#png("sim_linf.png", width = psize[1], height = psize[2], units = "in", res = 300)
grid.draw(z)
dev.off()


# gamma
d1 <- subset(d, subset = d$Parameter == "gamma")
d1$Estimate <- d1$Estimate * 52.15 # Convert to years-1
d1$Truth <- d1$Truth * 52.15       # Convert to years-1
dl <- aggregate(Estimate ~ Sex + Parameter + Power + Scenario, data = d1, FUN = length)
dl$lab <- dl$Estimate
dl$x <- rep(35, nrow(dl))
dl$y <- rep(135, nrow(dl))
p <- ggplot(data = d1, aes(x = Estimate)) +
        geom_text(data = dl, aes(x = x, y = y, label = lab), vjust = 1) +
        geom_histogram(colour = "black", fill = "grey") +
        facet_grid(Power + Sex ~ Scenario, scales = "fixed") +
        geom_vline(aes(xintercept = Truth), size = 1.5, colour = "red", alpha = 0.6) +
        xlab(expression(gamma)) + ylab("Frequency\n") +
        plot_theme() + theme(axis.title.x = element_text(vjust = -0.5))
png("sim_gamma.png", width = psize[1], height = psize[2], units = "in", res = 300)
print(p)
dev.off()


# kmean - fig. 5
d1 <- subset(d, subset = d$Parameter == "bmean")
d1$Estimate <- d1$Estimate * 52.15 # Convert to years-1
d1$Truth <- d1$Truth * 52.15       # Convert to years-1
dl <- aggregate(Estimate ~ Sex + Parameter + Power + Scenario, data = d1, FUN = length)
dl$lab <- dl$Estimate
dl$x <- rep(0.25, nrow(dl))
dl$y <- rep(120, nrow(dl))
p <- ggplot(data = d1, aes(x = Estimate)) +
        geom_text(data = dl, aes(x = x, y = y, label = lab), vjust = 1) +
        geom_histogram(colour = "black", fill = "grey") +
        facet_grid(Power + Sex ~ Scenario, scales = "fixed") +
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
cairo_ps("FIG5.eps", width = psize[1], height = psize[2])
#png("sim_kmean.png", width = psize[1], height = psize[2], units = "in", res = 300)
grid.draw(z)
dev.off()


# L0 - fig. 7
d1 <- subset(d, subset = d$Parameter == "L0")
dl <- aggregate(Estimate ~ Sex + Parameter + Power + Scenario, data = d1, FUN = length)
dl$lab <- dl$Estimate
dl$x <- rep(43, nrow(dl))
dl$y <- rep(95, nrow(dl))
p <- ggplot(data = d1, aes(x = Estimate)) +
        geom_text(data = dl, aes(x = x, y = y, label = lab), vjust = 1) +
        geom_histogram(colour = "black", fill = "grey") +
        facet_grid(Power + Sex ~ Scenario, scales = "fixed") +
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
cairo_ps("FIG7.eps", width = psize[1], height = psize[2])
#png("sim_L0.png", width = psize[1], height = psize[2], units = "in", res = 300)
grid.draw(z)
dev.off()


# sd_kdev - fig. 8
d1 <- subset(d, subset = d$Parameter == "sd_bdev")
d1 <- subset(d1, subset = d1$Scenario %in% c("k", "k and z"))
dl <- aggregate(Estimate ~ Sex + Parameter + Power + Scenario, data = d1, FUN = length)
dl$lab <- dl$Estimate
dl$x <- rep(0.38, nrow(dl))
dl$y <- rep(105, nrow(dl))
p <- ggplot(data = d1, aes(x = Estimate)) +
        geom_text(data = dl, aes(x = x, y = y, label = lab), vjust = 1) +
        geom_histogram(colour = "black", fill = "grey") +
        facet_grid(Power + Sex ~ Scenario, scales = "fixed") +
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
cairo_ps("FIG8.eps", width = psize[1]/2, height = psize[2])
#png("sim_sd_kdev.png", width = psize[1]/2, height = psize[2], units = "in", res = 300)
grid.draw(z)
dev.off()


# sd_z - fig. 9
d1 <- subset(d, subset = d$Parameter == "sd_z")
d1 <- subset(d1, subset = d1$Scenario %in% c("z", "k and z"))
dl <- aggregate(Estimate ~ Sex + Parameter + Power + Scenario, data = d1, FUN = length)
dl$lab <- dl$Estimate
dl$x <- rep(0.48, nrow(dl))
dl$y <- rep(85, nrow(dl))
p <- ggplot(data = d1, aes(x = Estimate)) +
        geom_text(data = dl, aes(x = x, y = y, label = lab), vjust = 1) +
        geom_histogram(colour = "black", fill = "grey") +
        facet_grid(Power ~ Scenario, scales = "free_x") +
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
cairo_ps("FIG9.eps", width = psize[1]/2, height = psize[2]/2)
#png("sim_sd_z.png", width = psize[1]/2, height = psize[2]/2, units = "in", res = 300)
grid.draw(z)
dev.off()


# sd_obs
d1 <- subset(d, subset = d$Parameter == "sd_obs")
dl <- aggregate(Estimate ~ Sex + Parameter + Power + Scenario, data = d1, FUN = length)
dl$lab <- dl$Estimate
dl$x <- rep(43, nrow(dl))
dl$y <- rep(95, nrow(dl))
p <- ggplot(data = d1, aes(x = Estimate)) +
        geom_text(data = dl, aes(x = x, y = y, label = lab), vjust = 1) +
        geom_histogram(colour = "black", fill = "grey") +
        facet_grid(Power ~ Scenario, scales = "free_x") +
        geom_vline(aes(xintercept = Truth), size = 1.5, colour = "red", alpha = 0.6) +
        xlab(expression(c[obs])) + ylab("Frequency\n") +
        plot_theme() + theme(axis.title.x = element_text(vjust = -0.5)) +
        labs(title = "Estimation model\n")
png("sim_sd_obs.png", width = psize[1], height = psize[2]/2, units = "in", res = 300)
print(p)
dev.off()
