require(TagGrowth)
d <- read_all_simulations()

psize <- c(12, 8)

# gamma
d1 <- subset(d, subset = d$Parameter == "gamma")
p <- ggplot(data = d1, aes(x = Estimate)) +
        geom_histogram(colour = "black", fill = "grey") +
        facet_grid(Power + Sex ~ Scenario, scales = "free_x") +
        geom_vline(aes(xintercept = Truth), size = 1.5, colour = "red", alpha = 0.6) +
        xlab("") + ylab("Frequency\n") +
        plot_theme()
png("sim_gamma.png", width = psize[1], height = psize[2], units = "in", res = 300)
print(p)
dev.off()

# kmean
d1 <- subset(d, subset = d$Parameter == "bmean")
p <- ggplot(data = d1, aes(x = Estimate)) +
        geom_histogram(colour = "black", fill = "grey") +
        facet_grid(Power + Sex ~ Scenario, scales = "free_x") +
        geom_vline(aes(xintercept = Truth), size = 1.5, colour = "red", alpha = 0.6) +
        xlab("") + ylab("Frequency\n") +
        plot_theme()
png("sim_kmean.png", width = psize[1], height = psize[2], units = "in", res = 300)
print(p)
dev.off()

# L0
d1 <- subset(d, subset = d$Parameter == "L0")
p <- ggplot(data = d1, aes(x = Estimate)) +
        geom_histogram(colour = "black", fill = "grey") +
        facet_grid(Power + Sex ~ Scenario, scales = "free_x") +
        geom_vline(aes(xintercept = Truth), size = 1.5, colour = "red", alpha = 0.6) +
        xlab("") + ylab("Frequency\n") +
        plot_theme()
png("sim_L0.png", width = psize[1], height = psize[2], units = "in", res = 300)
print(p)
dev.off()

# sd_bdev
d1 <- subset(d, subset = d$Parameter == "sd_bdev")
d1 <- subset(d1, subset = d1$Scenario %in% c("k", "k and z"))
p <- ggplot(data = d1, aes(x = Estimate)) +
        geom_histogram(colour = "black", fill = "grey") +
        facet_grid(Power + Sex ~ Scenario, scales = "free_x") +
        geom_vline(aes(xintercept = Truth), size = 1.5, colour = "red", alpha = 0.6) +
        xlab("") + ylab("Frequency\n") +
        plot_theme()
png("sim_sd_kdev.png", width = psize[1], height = psize[2], units = "in", res = 300)
print(p)
dev.off()

# sd_z
d1 <- subset(d, subset = d$Parameter == "sd_z")
d1 <- subset(d1, subset = d1$Scenario %in% c("z", "k and z"))
p <- ggplot(data = d1, aes(x = Estimate)) +
        geom_histogram(colour = "black", fill = "grey") +
        facet_grid(Power ~ Scenario, scales = "free_x") +
        geom_vline(aes(xintercept = Truth), size = 1.5, colour = "red", alpha = 0.6) +
        xlab("") + ylab("Frequency\n") +
        plot_theme()
png("sim_sd_z.png", width = psize[1], height = psize[2], units = "in", res = 300)
print(p)
dev.off()

# sd_obs
d1 <- subset(d, subset = d$Parameter == "sd_obs")
p <- ggplot(data = d1, aes(x = Estimate)) +
        geom_histogram(colour = "black", fill = "grey") +
        facet_grid(Power ~ Scenario, scales = "free_x") +
        geom_vline(aes(xintercept = Truth), size = 1.5, colour = "red", alpha = 0.6) +
        xlab("") + ylab("Frequency\n") +
        plot_theme()
png("sim_sd_obs.png", width = psize[1], height = psize[2], units = "in", res = 300)
print(p)
dev.off()




    p <- ggplot(data = d1, aes(x = Estimate)) +
        geom_histogram(colour = "black", fill = "grey") +
        facet_grid(Power ~ Scenario, scales = "free_x") +
        #geom_vline(aes(xintercept = Truth), size = 1.5, colour = "red", alpha = 0.6) +
        geom_vline(aes(xintercept = Truth), size = 0.75, colour = "red") +
        xlab("") + ylab("Frequency\n") +
        plot_theme()
        #scale_colour_manual(values = plot_palette)




plot_simulations("v0/")
plot_simulations("v1/")
plot_simulations("v2/")
plot_simulations("v3/")


folder <- "v0/"
v0 <- read_simulations(folder)
#plot_simulations(folder)

lab <- c(expression(sigma[obs]), expression(L[0]), expression(k),
         expression(gamma))
sex <- c("Females", "Males")

#png(paste(folder, "results/SimPars_2.png", sep = ""), width = 4.6, height = 4.6, units = "in", res = 300)
pdf(paste(folder, "results/SimPars_2.pdf", sep = ""), width = 5, height = 5)
par(mfrow = c(3,2), oma = c(0,2.5,0,0), mar = c(5.1,2.1,2.1,2.1))
for (i in 2:length(unique(v0$Parameter)))
{
    r <- subset(v0, Parameter %in% unique(v0$Parameter)[i])
    xlim <- range(r$Value) * c(0.95, 1.05)
    for (j in 1:2)
    {
        d <- subset(v0, Parameter %in% unique(v0$Parameter)[i] & Sex %in% sex[j])
        hist(d$Value, main = "", xlab = lab[i], ylab = "", col = "grey", las = 1, xlim = xlim)
        if (i == 2 && j == 1) title("Female")
        if (i == 2 && j == 2) title("Male")
        abline(v = d$Truth[1], col = 2, lwd = 2)
    }
}
mtext("Frequency", side = 2, line = 1, outer = TRUE)
dev.off()








science_theme = theme(panel.grid.major = element_line(size = 0.5, color = "grey"), 
    axis.line = element_line(size = 0.7, color = "black"),
legend.position = c(0.85, 0.7), text = element_text(size = 14))

# The instructions say the figure should be sized to fit in one or two columns
# (2.3 or 4.6 inches), so we want them to look good at that resolution.

pdf(file = "sleepplot.pdf", width= 6, height = 4, #' see how it looks at this size
    useDingbats=F) #I have had trouble when uploading figures with digbats before, so I don't use them
sleepplot2 #print our plot
dev.off() #stop making pdfs
