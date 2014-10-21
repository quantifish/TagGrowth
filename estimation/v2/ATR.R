######################################################################################################
# RANDOM EFFECTS MODELLING OF GROWTH USING TAG RECAPTURE DATA
######################################################################################################
# Authors: Jim Thorson, Darcy Webber

# Make sure R is clean
rm(list = ls())

# Load TMB
require(TMB)
require(ggplot2)
require(reshape2)

# Source some R
source("../../src/time-step.R")
source("../../src/plot_theme.R")
source("../../src/plot_palette.R")
source("../../src/plot.obs.pred.R")
source("../../src/plot.histogram.R")
source("../../src/plot.indiv.growth.R")

# Compile the model
compile("../ATR.cpp")


######################################################################################################
# DATA
######################################################################################################
# Load data
load("../../data/ATR_mod.RData")

# Change to daily/weekly estimates
ATR_mod <- time.step(ATR_mod, units = "weeks")


######################################################################################################
# Make AD object
######################################################################################################
dyn.load(dynlib("../ATR"))
Nindiv <- nrow(ATR_mod)
Data <- list(iAge1 = ATR_mod[1:Nindiv,'iAge1'], iLiberty = ATR_mod[1:Nindiv,'iLiberty'],
             Length1 = ATR_mod[1:Nindiv,'Length1'], Length2 = ATR_mod[1:Nindiv,'Length2'],
             Sex = ATR_mod[1:Nindiv,'Sex'],
             Time0 = ATR_mod[1:Nindiv,'Time0'], Time1 = ATR_mod[1:Nindiv,'Time1'],
             Year0 = ATR_mod[1:Nindiv,'Year0'], Year1 = ATR_mod[1:Nindiv,'Year1'],
             Area1 = ATR_mod[1:Nindiv,'Area1'])
Nyears <- 40
Nareas <- length(unique(ATR_mod$Area1))

# No year-effects or area-effects
Params <- list(ln_gamma=log(10000), logit_psi=qlogis(0.2), ln_L0=rep(log(1),2),
               ln_bmean=rep(log(0.2),2), ln_bdev=rep(0,Nindiv), ln_sd_bdev=c(log(0.01),log(0.01)),
               ln_sd_obs=log(20),
               z1=rep(0,Nindiv), z2=rep(0,Nindiv), ln_sd_z=log(0.1),
               ln_ydev=rep(0,Nyears), ln_sd_ydev=log(0.01),
               ln_xdev=rep(0,Nareas), ln_sd_xdev=log(0.01))
obj <- MakeADFun(data = Data, parameters = Params,
                 map = list(ln_ydev=factor(rep(NA,Nyears)), ln_sd_ydev=factor(NA), ln_xdev=factor(rep(NA,Nareas)), ln_sd_xdev=factor(NA)),
                 random = c("ln_bdev", "z1", "z2"))


######################################################################################################
# Run model
######################################################################################################
newtonOption(smartsearch = TRUE)
obj$fn(obj$par)
obj$gr(obj$par)
obj$control <- list(trace = 100)
obj$hessian <- TRUE
ConvergeTol <- 1 # 1:Normal; 2:Strong
#obj$env$inner.control$step.tol <- c(1e-12,1e-15)[ConvergeTol] # Default : 1e-8  # Change in parameters limit inner optimization
#obj$env$inner.control$tol10 <- c(1e-8,1e-12)[ConvergeTol]  # Default : 1e-3     # Change in pen.like limit inner optimization
#obj$env$inner.control$grad.tol <- c(1e-12,1e-15)[ConvergeTol] # # Default : 1e-8  # Maximum gradient limit inner optimization
summary(obj)

opt <- nlminb(start = obj$par, objective = obj$fn, control = list(eval.max = 1e4, iter.max = 1e4, rel.tol = c(1e-10,1e-8)[ConvergeTol]))
Report <- sdreport(obj)

dyn.unload(dynlib("ATR"))
Report$pdHess

######################################################################################################
# Inspect results
######################################################################################################
t(t(tapply(X=Report$value, INDEX=names(Report$value), FUN=length)))

# Save results to file
save(obj, file = "obj.RData")
save(opt, file = "opt.RData")
save(Report, file = "Report.RData")
write.csv(data.frame(names(Report$value), Report$value), file = "Pars.csv", row.names = TRUE)

# Load if not estimating
#load("Report.RData")

######################################################################################################
# Plot results
######################################################################################################
# Append model outputs to ATR_mod
ATR_mod$Length1_hat <- Report$value[names(Report$value) %in% "Length1_hat"]
ATR_mod$Length2_hat <- Report$value[names(Report$value) %in% "Length2_hat"]

plot.obs.pred()
plot.histogram()
plot.indiv.growth()

# Prior on Linf
png("LinfPrior.png", width=5, height=5, units="in", res=300)
par(mfrow=c(1,1))
x <- 75:275
priorF <- dnorm(x=x, mean=180.20, sd=0.102*180.20)
priorM <- dnorm(x=x, mean=169.07, sd=0.102*169.07)
plot(x, priorM, type = "l", col = "blue", lwd = 2, xlab = expression(L[infinity]), ylab = "Density", las = 1)
lines(x, priorF, col="pink", lwd=2)
LinfF <- Report$value[names(Report$value) %in% "Linf"][1]
LinfM <- Report$value[names(Report$value) %in% "Linf"][2]
abline(v=LinfF, lty=2, lwd=2, col="pink")
abline(v=LinfM, lty=2, lwd=2, col="blue")
legend("topleft", legend = c("Female prior", "Female estimate", "Male prior", "Male estimate"), lwd = 2, lty = c(1,2,1,2), col=c("pink","pink","blue","blue"), bty = "n")
dev.off()

# END
