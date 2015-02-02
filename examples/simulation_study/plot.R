require(TagGrowth)
d <- read_all_simulations()

psize <- c(12, 8)

d$Linf <- d$gamma * d$bmean^d$psi

# gamma
d1 <- subset(d, subset = d$Parameter == "gamma")
p <- ggplot(data = d1, aes(x = Estimate)) +
        geom_histogram(colour = "black", fill = "grey") +
        facet_grid(Power + Sex ~ Scenario, scales = "free_x") +
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
p <- ggplot(data = d1, aes(x = Estimate)) +
        geom_histogram(colour = "black", fill = "grey") +
        facet_grid(Power + Sex ~ Scenario, scales = "free_x") +
        geom_vline(aes(xintercept = Truth), size = 1.5, colour = "red", alpha = 0.6) +
        xlab(expression(mu[k])) + ylab("Frequency\n") +
        plot_theme() + theme(axis.title.x = element_text(vjust = -0.5))
png("sim_kmean.png", width = psize[1], height = psize[2], units = "in", res = 300)
print(p)
dev.off()

# L0
d1 <- subset(d, subset = d$Parameter == "L0")
p <- ggplot(data = d1, aes(x = Estimate)) +
        geom_histogram(colour = "black", fill = "grey") +
        facet_grid(Power + Sex ~ Scenario, scales = "free_x") +
        geom_vline(aes(xintercept = Truth), size = 1.5, colour = "red", alpha = 0.6) +
        xlab(expression(L[0])) + ylab("Frequency\n") +
        plot_theme() + theme(axis.title.x = element_text(vjust = -0.5))
png("sim_L0.png", width = psize[1], height = psize[2], units = "in", res = 300)
print(p)
dev.off()

# sd_kdev
d1 <- subset(d, subset = d$Parameter == "sd_bdev")
d1 <- subset(d1, subset = d1$Scenario %in% c("k", "k and z"))
p <- ggplot(data = d1, aes(x = Estimate)) +
        geom_histogram(colour = "black", fill = "grey") +
        facet_grid(Power + Sex ~ Scenario, scales = "free_x") +
        geom_vline(aes(xintercept = Truth), size = 1.5, colour = "red", alpha = 0.6) +
        xlab(expression(sigma[k])) + ylab("Frequency\n") +
        plot_theme() + theme(axis.title.x = element_text(vjust = -0.5))
png("sim_sd_kdev.png", width = psize[1]/2, height = psize[2], units = "in", res = 300)
print(p)
dev.off()

# sd_z
d1 <- subset(d, subset = d$Parameter == "sd_z")
d1 <- subset(d1, subset = d1$Scenario %in% c("z", "k and z"))
p <- ggplot(data = d1, aes(x = Estimate)) +
        geom_histogram(colour = "black", fill = "grey") +
        facet_grid(Power ~ Scenario, scales = "free_x") +
        geom_vline(aes(xintercept = Truth), size = 1.5, colour = "red", alpha = 0.6) +
        xlab(expression(sigma[z])) + ylab("Frequency\n") +
        plot_theme() + theme(axis.title.x = element_text(vjust = -0.5))
png("sim_sd_z.png", width = psize[1]/2, height = psize[2]/2, units = "in", res = 300)
print(p)
dev.off()

# sd_obs
d1 <- subset(d, subset = d$Parameter == "sd_obs")
p <- ggplot(data = d1, aes(x = Estimate)) +
        geom_histogram(colour = "black", fill = "grey") +
        facet_grid(Power ~ Scenario, scales = "free_x") +
        geom_vline(aes(xintercept = Truth), size = 1.5, colour = "red", alpha = 0.6) +
        xlab(expression(c[obs])) + ylab("Frequency\n") +
        plot_theme() + theme(axis.title.x = element_text(vjust = -0.5))
png("sim_sd_obs.png", width = psize[1], height = psize[2]/2, units = "in", res = 300)
print(p)
dev.off()

# Linf
d1 <- subset(d, subset = d$Parameter == "gamma")
p <- ggplot(data = d1, aes(x = Estimate)) +
        geom_histogram(colour = "black", fill = "grey") +
        facet_grid(Power + Sex ~ Scenario, scales = "free_x") +
        geom_vline(aes(xintercept = Truth), size = 1.5, colour = "red", alpha = 0.6) +
        xlab(expression(gamma)) + ylab("Frequency\n") +
        plot_theme() + theme(axis.title.x = element_text(vjust = -0.5))
png("sim_gamma.png", width = psize[1], height = psize[2], units = "in", res = 300)
print(p)
dev.off()


# Summary
aggregate(Estimate ~ Sex + Parameter + Power + Scenario, data = d, FUN = length)
aggregate(Estimate ~ Sex + Parameter + Power + Scenario, data = d, FUN = median)
