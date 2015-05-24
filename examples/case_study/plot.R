# Load package
require(TagGrowth)

# Load data
data(toothfish)

table(format(toothfish$Date1, "%b"))
table(format(toothfish$Date2, "%b"))

(range(toothfish$Date1, toothfish$Date2)[2] - range(toothfish$Date1, toothfish$Date2)[1])/365

range(toothfish$Date1)
range(toothfish$Date2)
      
# Versions
# 0. none
# 1. k
# 2. z
# 3. y
# 4. k, z
# 5. k, y
# 6. z, y
# 7. k, z, y
scenarios <- c("v0/","v1/","v2/","v4/")
#scenarios <- c("v0/","v1/","v2/","v3/","v4/","v5/","v6/","v7/")
LL <- rep(NA, length(scenarios))
npar <- rep(NA, length(scenarios))
aic <- rep(NA, length(scenarios))

for (Iscenario in scenarios)
{
    # Directory to save output files to
    folder <- Iscenario

    # Load report
    load(paste0(folder, "Report.RData"))
    
    load(paste0(folder, "opt.RData"))
    LL[which(scenarios == Iscenario)] <- -1 * opt$objective
    npar[which(scenarios == Iscenario)] <- length(opt$par)
    aic[which(scenarios == Iscenario)] <- (2 * opt$objective) + (2 * length(opt$par))

    # Append model outputs to ATR_mod
    toothfish$Length1_hat <- Report$value[names(Report$value) %in% "Length1_hat"]
    toothfish$Length2_hat <- Report$value[names(Report$value) %in% "Length2_hat"]

    # Do some plots
    plot_obs_pred(Sex = toothfish$Sex, Length1_obs = toothfish$Length1, Length1_hat = toothfish$Length1_hat, Length2_obs = toothfish$Length2, Length2_hat = toothfish$Length2_hat, file_name = paste0(folder, "ObsVsPred"))
    #plot_resids(Sex = toothfish$Sex, Length1_obs = toothfish$Length1, Length1_hat = toothfish$Length1_hat, Length2_obs = toothfish$Length2, Length2_hat = toothfish$Length2_hat, file_name = paste0(folder, "Resids"))
    plot_standard_resids(Sex = toothfish$Sex, Length1_obs = toothfish$Length1, Length1_hat = toothfish$Length1_hat, Length2_obs = toothfish$Length2, Length2_hat = toothfish$Length2_hat, file_name = paste0(folder, "StandardResid"))
    plot_indiv_growth(toothfish$Sex, toothfish$Age1, toothfish$Length1, toothfish$Length1_hat, toothfish$Age2, toothfish$Length2, toothfish$Length2_hat, file_name = paste0(folder, "IndivGrowth"))
    if (Iscenario %in% c("v1/","v4/","v5/","v7/"))
        plot_histogram_k(ATR_mod, Report, file_name = paste0(folder, "REs_b"))
    if (Iscenario %in% c("v2/","v4/","v6/","v7/"))
        plot_histogram_z(ATR_mod, Report, file_name = paste0(folder, "REs_z"))
    if (Iscenario %in% c("v3/","v5/","v6/","v7/"))
        plot_annual_devs(Report, file_name = paste0(folder, "REs_y"))
    #plot_linf(Report, file_name = paste0(folder, "LinfPrior"))
}

LL
npar
aic

# Do plot for paper of individual growth trajectories for no random effects
# model and z only model
load("v0/Report.RData")
toothfish$Length1_none <- Report$value[names(Report$value) %in% "Length1_hat"]
toothfish$Length2_none <- Report$value[names(Report$value) %in% "Length2_hat"]
load("v1/Report.RData")
toothfish$Length1_k <- Report$value[names(Report$value) %in% "Length1_hat"]
toothfish$Length2_k <- Report$value[names(Report$value) %in% "Length2_hat"]
load("v2/Report.RData")
toothfish$Length1_z <- Report$value[names(Report$value) %in% "Length1_hat"]
toothfish$Length2_z <- Report$value[names(Report$value) %in% "Length2_hat"]
load("v4/Report.RData")
toothfish$Length1_kz <- Report$value[names(Report$value) %in% "Length1_hat"]
toothfish$Length2_kz <- Report$value[names(Report$value) %in% "Length2_hat"]

#plot_indiv_growth_2(Sex = toothfish$Sex, Age1 = toothfish$Age1, Length1_obs = toothfish$Length1, Length1_hat1 = toothfish$Length1_none, Length1_hat2 = toothfish$Length1_z, Age2 = toothfish$Age2, Length2_obs = toothfish$Length2, Length2_hat1 = toothfish$Length2_none, Length2_hat2 = toothfish$Length2_z, file_name = "FIG1")
Sex = toothfish$Sex
Age1 = toothfish$Age1
Length1_obs = toothfish$Length1
Age2 = toothfish$Age2
Length2_obs = toothfish$Length2
file_name = "FIG1"
Label <- c("None","k","z","k and z")

    d1 <- data.frame(Age1, Age2, Sex, Length1_obs, Length2_obs, 
        Key = "Observed", Model = Label[1])
    d2 <- data.frame(Age1, Age2, Sex, Length1_obs, Length2_obs, 
        Key = "Observed", Model = Label[2])
    d3 <- data.frame(Age1, Age2, Sex, Length1_obs, Length2_obs, 
        Key = "Observed", Model = Label[3])
    d4 <- data.frame(Age1, Age2, Sex, Length1_obs, Length2_obs, 
        Key = "Observed", Model = Label[4])
    d5 <- data.frame(Age1, Age2, Sex, toothfish$Length1_none,
                     toothfish$Length2_none, Key = "Expected", Model = Label[1])
    d6 <- data.frame(Age1, Age2, Sex, toothfish$Length1_k,
                     toothfish$Length2_k, Key = "Expected", Model = Label[2])
    d7 <- data.frame(Age1, Age2, Sex, toothfish$Length1_z,
                     toothfish$Length2_z, Key = "Expected", Model = Label[3])
    d8 <- data.frame(Age1, Age2, Sex, toothfish$Length1_kz,
                     toothfish$Length2_kz, Key = "Expected", Model = Label[4])
    names(d8) <- names(d2) <- names(d3) <- names(d4) <- names(d5) <- names(d6) <- names(d7) <- names(d1)
    dat <- rbind(d1, d2, d3, d4, d5, d6, d7, d8)
    dat$Sex[dat$Sex == 1] <- "Females"
    dat$Sex[dat$Sex == 2] <- "Males"
    p <- ggplot(data = dat) + geom_segment(aes(x = Age1, y = Length1_obs, 
        xend = Age2, yend = Length2_obs, group = c(Key), color = Key)) + 
        facet_grid(Model ~ Sex) + xlab("\nAge (years)") + ylab("Length (cm)\n") + 
        scale_x_continuous(limits = c(0, max(dat$Age1, dat$Age2))) + 
        scale_y_continuous(limits = c(0, max(dat$Length1_obs, dat$Length2_obs))) +
        plot_theme() + scale_colour_manual(values = plot_palette) +
        guides(color = guide_legend(title = "Key")) +
        theme(legend.position = c(0.91, 0.05))
    cairo_ps(paste0(file_name, ".eps"), width = 8.27, height = 11.69)
    print(p)
    dev.off()


# END
