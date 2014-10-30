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
source("../src/time-step.R")
source("../src/plot_theme.R")
source("../src/plot_palette.R")
source("../src/plot.obs.pred.R")
source("../src/plot.histogram.R")
source("../src/plot.indiv.growth.R")
source("../src/plot.linf.R")

# Compile the model
compile("ATR.cpp")


######################################################################################################
# DATA
######################################################################################################
# Load data
load("../data/ATR_mod.RData")

# Change to daily/weekly estimates
ATR_mod <- time.step(ATR_mod, units = "weeks")


######################################################################################################
# Make AD object
######################################################################################################
dyn.load(dynlib("ATR"))
Options <- c("YearTF"=0, "AreaTF"=0, "IndivTF"=0, "IndivTimeTF"=0) #1st slot: 
Nindiv <- nrow(ATR_mod)
Data <- list(Options=Options, iAge1 = ATR_mod[1:Nindiv,'iAge1'], iLiberty = ATR_mod[1:Nindiv,'iLiberty'],
             Length1 = ATR_mod[1:Nindiv,'Length1'], Length2 = ATR_mod[1:Nindiv,'Length2'],
             Sex = ATR_mod[1:Nindiv,'Sex'],
             Time0 = ATR_mod[1:Nindiv,'Time0'], Time1 = ATR_mod[1:Nindiv,'Time1'],
             Year0 = ATR_mod[1:Nindiv,'Year0'], Year1 = ATR_mod[1:Nindiv,'Year1'],
             Area1 = ATR_mod[1:Nindiv,'Area1'])
Nyears <- 40
Nareas <- length(unique(ATR_mod$Area1))

# With area-effects
Params <- list(ln_gamma = log(10000), logit_psi = qlogis(0.2), ln_L0 = rep(log(1), 2),
               ln_bmean = rep(log(0.2), 2), ln_bdev = rep(0, Nindiv), ln_sd_bdev = c(log(0.01), log(0.01)),
               ln_sd_obs = log(20), z1 = rep(0, Nindiv), z2 = rep(0, Nindiv), ln_sd_z = log(0.1),
               ln_ydev = rep(0, Nyears), ln_sd_ydev = log(0.01),
               ln_xdev = rep(0, Nareas), ln_sd_xdev = log(0.01))
Random = NULL
Map <- list()
if(Options[1]==0){
  Map[["ln_ydev"]] = factor(rep(NA, Nyears))
  Map[["ln_sd_ydev"]] = factor(NA)
}else{
  Random = c(Random, "ln_ydev")
} 
if(Options[2]==0){
  Map[["ln_sd_xdev"]]=factor(NA)
  Map[["ln_xdev"]]=factor(rep(NA,length(Params$ln_xdev))) 
}else{
  Random = c(Random, "ln_xdev")
}
if(Options[3]==0){
  Map[["ln_bdev"]]=factor(rep(NA,length(Params$ln_bdev)))
  Map[["ln_sd_bdev"]]=factor(rep(NA,2)) 
}else{
  Random = c(Random, "ln_bdev")
}
if(Options[4]==0){
  Map[["z2"]]=factor(rep(NA,length(Params$ln_bdev)))
  Map[["z1"]]=factor(rep(NA,length(Params$z1)))
  Map[["ln_sd_z"]]=factor(NA)  
}else{
  Random = c(Random, "z1", "z2")
}
obj <- MakeADFun(data = Data, parameters = Params, map = Map, random = Random, inner.control=list(maxit=50))

# List of parameters that are "on"
names( obj$par )

######################################################################################################
# Run model
######################################################################################################
newtonOption(smartsearch = TRUE)
obj$fn(obj$par)
obj$gr(obj$par)
obj$hessian <- TRUE
obj$control <- list(trace=100)
ConvergeTol <- 1 # 1:Normal; 2:Strong
#obj$env$inner.control$step.tol <- c(1e-12,1e-15)[ConvergeTol] # Default : 1e-8  # Change in parameters limit inner optimization
#obj$env$inner.control$tol10 <- c(1e-8,1e-12)[ConvergeTol]  # Default : 1e-3     # Change in pen.like limit inner optimization
#obj$env$inner.control$grad.tol <- c(1e-12,1e-15)[ConvergeTol] # # Default : 1e-8  # Maximum gradient limit inner optimization
summary(obj)

Upr = rep(Inf, length(obj$par))
  Upr[match("logit_psi",names(obj$par))] = qlogis(0.999)
Lwr = rep(-Inf, length(obj$par))
  Lwr[match("logit_psi",names(obj$par))] = qlogis(0.001)
  Lwr[match("ln_sd_z",names(obj$par))] = log(0.001)
  Lwr[match("ln_sd_xdev",names(obj$par))] = log(0.001)
  Lwr[match("ln_sd_bdev",names(obj$par))] = log(0.001)

ptm <- proc.time()
opt <- nlminb(start = obj$par, objective = obj$fn, upper = Upr, lower = Lwr, control = list(eval.max = 1e4, iter.max = 1e4, rel.tol = c(1e-10, 1e-8)[ConvergeTol], trace = 1))
opt[["final_gradient"]] = obj$gr(opt$par)
Diag <- obj$report()
Report <- sdreport(obj)
proc.time() - ptm

Hess <- optimHess(par = opt$par, fn = obj$fn)
opt <- nlminb(start = opt$par, objective = obj$fn, upper = Upr, lower = Lwr, control = list(eval.max = 1e4, iter.max = 1e4, rel.tol = c(1e-10, 1e-8)[ConvergeTol], trace = 1))

cbind(Lwr, opt$par, Upr)
capture.output(Report, file = "Report.txt")

Delta = rep(0,length(opt$par))
  Delta[2] = 1e-5
( obj$fn(opt$par - Delta/2) - obj$fn(opt$par + Delta/2) )  / abs(max(Delta))

dyn.unload(dynlib("ATR"))
Report$pdHess

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
plot.linf()

# END
