######################################################################################################
# RANDOM EFFECTS MODELLING OF GROWTH USING TAG RECAPTURE DATA
######################################################################################################
# Authors: Jim Thorson, Darcy Webber

# Fixed effects:
# Random effects:

rm(list=ls())

######################################################################################################
# DATA
######################################################################################################
# Some useful functions for converting the data frame from annual to
# daily/weekly/monthly time steps.

time.step <- function(ATR_mod, units = "weeks")
{
    if (units == "days") { mult <- 365.25 }
    if (units == "weeks") { mult <- 52.15 }
    ATR_mod$Age2 <- ATR_mod$Age2 * mult
    ATR_mod$Age1 <- as.numeric(ATR_mod$Age2 - difftime(ATR_mod$Date2, ATR_mod$Date1, units = units))
    ATR_mod$iAge1 <- round(ATR_mod$Age1)
    ATR_mod$Date0 <- ATR_mod$Date2 - as.difftime(ATR_mod$Age2, unit = units)
    ATR_mod$iLiberty <- round(as.numeric(difftime(ATR_mod$Date2, ATR_mod$Date1, units = units)))
    # We define a year as 1 Nov. to 31 Oct.
    ATR_mod$Year0 <- rep(NA, nrow(ATR_mod))
    ATR_mod$Year1 <- rep(NA, nrow(ATR_mod))
    ATR_mod$Year2 <- rep(NA, nrow(ATR_mod))
    year0 <- 1972
    for (i in 0:100)
    {
        yr <- year0 + i
        y1 <- strptime(paste(yr, "-11-01", sep = ""), format = "%Y-%m-%d")
        y2 <- strptime(paste(yr+1, "-10-31", sep = ""), format = "%Y-%m-%d")
        ind <- which(ATR_mod$Date0 >= y1 & ATR_mod$Date0 < y2)
        ATR_mod$Year0[ind] <- i
        ind <- which(ATR_mod$Date1 >= y1 & ATR_mod$Date1 < y2)
        ATR_mod$Year1[ind] <- i
        ind <- which(ATR_mod$Date2 >= y1 & ATR_mod$Date2 < y2)
        ATR_mod$Year2[ind] <- i
    }
    # Here we identify the number of days since day0 until the birth of each
    # individual (Time0), the capture of each individual (Time1), and the
    # recapture of each individual (Time2).  day0 is defined as the start of
    # the year in which the first individual was born.
    min(ATR_mod$Date0) # = "1973-01-09 23:00:00 NZST", therefore day 0 is to be
                       # 1972-08-01.  This means our first year is 1 Aug 1972
                       # to 31 July 1973.    
    max(ATR_mod$Date2) # = "2012-01-28 NZDT", therefore our final year is 1 Aug
                       # 2011 to 31 July 2012.
    day0 <- strptime("1972-08-01", format = "%Y-%m-%d")
    ATR_mod$Time0 <- round(as.numeric(difftime(ATR_mod$Date0, day0, units = units)))
    ATR_mod$Time1 <- ATR_mod$Time0 + ATR_mod$iAge1
    ATR_mod$Time2 <- ATR_mod$Time1 + ATR_mod$iLiberty
    return(ATR_mod)
}

# Load TMB
library(TMB)

# Specify which file/version
file <- "ATR"
compile(paste(file, ".cpp", sep=""))

# Load data
load("ATR.RData")
load("ATR_mod.RData")

# Change to daily/weekly estimates
#ATR_mod <- time.step(ATR_mod, units = "days")
ATR_mod <- time.step(ATR_mod, units = "weeks")
head(ATR_mod)

# Validate that the counter in the model will index the correct years (daily)
for (II in 1:nrow(ATR_mod)) {
    time0 = ATR_mod$Time0[II]
    year1 = ATR_mod$Year0[II]
    for (i in 0:(ATR_mod$iAge1[II]-1))
    {
        time1 = time0 + i
        if ( time1 %% 365 == 0. ) { year1 = year1 + 1; }
    }
    cat("Time1:", time1, ATR_mod[II,]$Time1, "| Year1:", year1, ATR_mod[II,]$Year1, "|", year1 == ATR_mod[II,]$Year1, "\n")
}

# Validate that the counter in the model will index the correct years (weeks)
for (II in 1:nrow(ATR_mod)) {
    time0 = ATR_mod$Time0[II]
    year1 = ATR_mod$Year0[II]
    for (i in 0:(ATR_mod$iAge1[II]-1))
    {
        time1 = time0 + i
        if ( time1 %% 52 == 0. ) { year1 = year1 + 1; }
    }
    cat("Time1:", time1, ATR_mod[II,]$Time1, "| Year1:", year1, ATR_mod[II,]$Year1, "|", year1 == ATR_mod[II,]$Year1, "\n")    
    time1 = ATR_mod$Time1[II]
    year2 = ATR_mod$Year1[II]
    for (i in 0:(ATR_mod$iLiberty[II]-1))
    {
        time2 = time0 + i
        if ( time2 %% 52 == 0. ) { year2 = year2 + 1; }
    }
    cat("Time2:", time2, ATR_mod[II,]$Time2, "| Year2:", year2, ATR_mod[II,]$Year2, "|", year2 == ATR_mod[II,]$Year2, "\n")
}


######################################################################################################
# Make AD object
######################################################################################################
dyn.load(dynlib(file))
Nindiv <- nrow(ATR_mod)
Data <- list(iAge1 = ATR_mod[1:Nindiv,'iAge1'], iLiberty = ATR_mod[1:Nindiv,'iLiberty'],
             Length1 = ATR_mod[1:Nindiv,'Length1'], Length2 = ATR_mod[1:Nindiv,'Length2'],
             Sex = ATR_mod[1:Nindiv,'Sex'],
             Time0 = ATR_mod[1:Nindiv,'Time0'], Time1 = ATR_mod[1:Nindiv,'Time1'],
             Year0 = ATR_mod[1:Nindiv,'Year0'],
             Year1 = ATR_mod[1:Nindiv,'Year1'],
             Area1 = ATR_mod[1:Nindiv,'Area1'] )
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

# With year-effects
Params <- list(ln_gamma=log(10000), logit_psi=qlogis(0.2), ln_L0=rep(log(1),2),
               ln_bmean=rep(log(0.2),2), ln_bdev=rep(0,Nindiv), ln_sd_bdev=c(log(0.01),log(0.01)),
               ln_sd_obs=log(20),
               z1=rep(0,Nindiv), z2=rep(0,Nindiv), ln_sd_z=log(0.1),
               ln_ydev=rep(0,Nyears), ln_sd_ydev=log(0.01),
               ln_xdev=rep(0,Nareas), ln_sd_xdev=log(0.01))
obj <- MakeADFun(data = Data, parameters = Params,
                 map = list(ln_xdev=factor(rep(NA,Nareas)), ln_sd_xdev=factor(NA)),
                 random = c("ln_bdev", "z1", "z2", "ln_ydev"))

# With area-effects
Params <- list(ln_gamma=log(10000), logit_psi=qlogis(0.2), ln_L0=rep(log(1),2),
               ln_bmean=rep(log(0.2),2), ln_bdev=rep(0,Nindiv), ln_sd_bdev=c(log(0.01),log(0.01)),
               ln_sd_obs=log(20),
               z1=rep(0,Nindiv), z2=rep(0,Nindiv), ln_sd_z=log(0.1),
               ln_ydev=rep(0,Nyears), ln_sd_ydev=log(0.01),
               ln_xdev=rep(0,Nareas), ln_sd_xdev=log(0.01))
obj <- MakeADFun(data = Data, parameters = Params,
                 map = list(ln_ydev=factor(rep(NA,Nyears)), ln_sd_ydev=factor(NA)),
                 random = c("ln_bdev", "z1", "z2", "ln_xdev"))


######################################################################################################
# Run model
######################################################################################################
newtonOption(smartsearch=TRUE)
obj$fn(obj$par)
obj$gr(obj$par)
obj$control <- list(trace=10)
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

# Plot histogram of REs
png("REs.png", width=5, height=5, units="in", res=300)
par(mfrow=c(2,2))
#plot(density((Report$value["bmeanF"]*exp(REs_b))[ATR_mod$Sex==1]))
#lines(density((Report$value["bmeanM"]*exp(REs_b))[ATR_mod$Sex==2]))
xlim <- range(Report$value[names(Report$value) %in% "bmean"][1]*exp(REs_b))
hist((Report$value[names(Report$value) %in% "bmean"][1]*exp(REs_b))[ATR_mod$Sex==1], xlab="b", main="Females", las = 1, xlim = xlim, col = "grey")
abline(v=Report$value[names(Report$value) %in% "bmean"][1], col=2, lwd=2, lty = 2)
box()
hist((Report$value[names(Report$value) %in% "bmean"][2]*exp(REs_b))[ATR_mod$Sex==2], xlab="b", main="Males", las = 1, xlim = xlim, col = "grey")
abline(v=Report$value[names(Report$value) %in% "bmean"][2], col=2, lwd=2, lty = 2)
box()
xlim <- range(REs_z1, REs_z2)
hist(REs_z1, xlab="z1", main = "", las = 1, col = "grey", xlim = xlim)
box()
hist(REs_z2, xlab="z2", main = "", las = 1, col = "grey", xlim = xlim)
box()
#plot(density(c(REs_z1,REs_z2)), xlab="z", type="n")
#lines(density(REs_z1))
#lines(density(REs_z2))
#hist(Report$value[names(Report$value) %in% "a_indiv"], xlab="a")
#abline(v=Report$value["ameanF"], col=2)
#abline(v=Report$value["ameanM"], col=2)
dev.off()

# Plot observed vs. predicted length at age
png("ObsVsPred.png", width=5, height=5, units="in", res=300)
par(mfrow=c(1,2))
ylim <- range(ATR_mod$Length1[1:Nindiv], ATR_mod$Length2[1:Nindiv])
xlim <- range(Report$value[names(Report$value) %in% "Length1_hat"], Report$value[names(Report$value) %in% "Length2_hat"])
plot( x=Report$value[names(Report$value) %in% "Length1_hat"], y=ATR_mod$Length1[1:Nindiv], xlab="Predicted length", ylab="Observed length", las = 1, xlim = xlim, ylim = ylim)
abline(0,1, col=2, lwd = 2); box()
plot( x=Report$value[names(Report$value) %in% "Length2_hat"], y=ATR_mod$Length2[1:Nindiv], xlab="Predicted length", ylab="Observed length", las = 1, xlim = xlim, ylim = ylim)
abline(0,1, col=2, lwd = 2); box()
dev.off()

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
