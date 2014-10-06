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

source("../../src/theme_presentation.R")
source("../../src/plot.obs.pred.R")
source("../../src/plot.histogram.R")

compile("ATR.cpp")


######################################################################################################
# DATA
######################################################################################################

load("../sims/sim1.RData")
ATR_mod <- sim$Sim
head(ATR_mod)


######################################################################################################
# Make AD object
######################################################################################################
dyn.load(dynlib("ATR"))
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
obj$control <- list(trace = 10)
obj$hessian <- TRUE

ptm <- proc.time()
opt <- nlminb(start = obj$par, objective = obj$fn, control = list(eval.max = 1e4, iter.max = 1e4))
summary(obj)
Report <- sdreport(obj)
proc.time() - ptm


######################################################################################################
# Inspect results
######################################################################################################
REs_b <- Report$par.random[names(Report$par.random) %in% "ln_bdev"]
REs_z1 <- Report$par.random[names(Report$par.random) %in% "z1"]
REs_z2 <- Report$par.random[names(Report$par.random) %in% "z2"]
REs_y <- Report$par.random[names(Report$par.random) %in% "ln_ydev"]
head(Report$value, 15)
t(t(tapply(X=Report$value, INDEX=names(Report$value), FUN=length)))

# Save results to file
#save(obj, file="obj.RData")
#save(opt, file="opt.RData")
save(Report, file="Report.RData")
#load("Report.RData")
write.csv(data.frame(names(Report$value), Report$value), file="Pars.csv", row.names=TRUE)

# Append model outputs to ATR_mod
ATR_mod$Length1_hat <- Report$value[names(Report$value) %in% "Length1_hat"]
ATR_mod$Length2_hat <- Report$value[names(Report$value) %in% "Length2_hat"]

######################################################################################################
# Plot results
######################################################################################################
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


plot.obs.pred()
plot.histogram()


# Plot observed vs. "true" growth schedules
png("IndivGrowth.png", width=10, height=5, units="in", res=300)
par(mfrow=c(1,2))
plot(1, type="n", xlim=c(0,max(ATR_mod$Age2[1:Nindiv]/365)), ylim=c(0,max(ATR_mod$Length2[1:Nindiv])), xlab="Age", ylab="Length (cm)", las=1)
segments(x0=ATR_mod$iAge1[ATR_mod$Sex==1]/365, x1=ATR_mod$Age2[ATR_mod$Sex==1]/365, y0=ATR_mod$Length1[ATR_mod$Sex==1], y1=ATR_mod$Length2[ATR_mod$Sex==1])
segments(x0=ATR_mod$iAge1[ATR_mod$Sex==1]/365, x1=ATR_mod$Age2[ATR_mod$Sex==1]/365, y0=(Report$value[names(Report$value) %in% "Length1_hat"])[ATR_mod$Sex==1], y1=(Report$value[names(Report$value) %in% "Length2_hat"])[ATR_mod$Sex==1], col="red")
title("Females")
legend("bottomright", legend=c("Observed","Expected"), col=1:2, lwd=1, bty="n")
plot(1, type="n", xlim=c(0,max(ATR_mod$Age2[1:Nindiv]/365)), ylim=c(0,max(ATR_mod$Length2[1:Nindiv])), xlab="Age", ylab="Length (cm)", las=1)
segments(x0=ATR_mod$iAge1[ATR_mod$Sex==2]/365, x1=ATR_mod$Age2[ATR_mod$Sex==2]/365, y0=ATR_mod$Length1[ATR_mod$Sex==2], y1=ATR_mod$Length2[ATR_mod$Sex==2])
segments(x0=ATR_mod$iAge1[ATR_mod$Sex==2]/365, x1=ATR_mod$Age2[ATR_mod$Sex==2]/365, y0=(Report$value[names(Report$value) %in% "Length1_hat"])[ATR_mod$Sex==2], y1=(Report$value[names(Report$value) %in% "Length2_hat"])[ATR_mod$Sex==2], col="red")
title("Males")
legend("bottomright", legend=c("Observed","Expected"), col=1:2, lwd=1, bty="n")
dev.off()

# Year effects
png("YearEffect.png", width=5, height=5, units="in", res=300)
par(mfrow=c(2,1))
plot(exp(REs_y), type="l", col=2, lwd=2)
hist(exp(REs_y))
dev.off()

# END
