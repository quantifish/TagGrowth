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


# Linf
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
        xlab(expression(L[infinity])) + ylab("Frequency\n") +
        scale_x_continuous(limits = c(120, 300)) +
        plot_theme() + theme(axis.title.x = element_text(vjust = -0.5))
png("sim_linf.png", width = psize[1], height = psize[2], units = "in", res = 300)
print(p)
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


# kmean
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
png("sim_kmean.png", width = psize[1], height = psize[2], units = "in", res = 300)
print(p)
dev.off()


# L0
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
png("sim_L0.png", width = psize[1], height = psize[2], units = "in", res = 300)
print(p)
dev.off()


# sd_kdev
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
png("sim_sd_kdev.png", width = psize[1]/2, height = psize[2], units = "in", res = 300)
print(p)
dev.off()

# sd_z
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
png("sim_sd_z.png", width = psize[1]/2, height = psize[2]/2, units = "in", res = 300)
print(p)
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
        plot_theme() + theme(axis.title.x = element_text(vjust = -0.5))
png("sim_sd_obs.png", width = psize[1], height = psize[2]/2, units = "in", res = 300)
print(p)
dev.off()
