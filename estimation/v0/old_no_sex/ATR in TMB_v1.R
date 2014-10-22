
# Make sure R is clean
rm(list = ls())

# Load TMB
require(TMB)
require(ggplot2)
require(reshape2)

# Source some R
source("../../../src/difftime.R")
source("../../../src/time-step.R")
source("../../../src/plot_theme.R")
source("../../../src/plot_palette.R")
source("../../../src/plot.obs.pred.R")
source("../../../src/plot.histogram.R")
source("../../../src/plot.indiv.growth.R")
source("../../../src/plot.linf.R")

# Compile the model
compile("ATR_v1.cpp")


######################################################################################################
# DATA
######################################################################################################
# Load data
load("../../../data/ATR_mod.RData")

# Change to daily/weekly estimates
ATR_mod <- time.step(ATR_mod, units = "weeks")


######################################################################################################
# Make AD object
######################################################################################################
dyn.load(dynlib("ATR_v1"))
Nindiv <- nrow(ATR_mod)
Data <- list(Age1 = ATR_mod[1:Nindiv,'iAge1'], Age2 = round(ATR_mod[1:Nindiv,'Age2']),
             Length1 = ATR_mod[1:Nindiv,'Length1'], Length2 = ATR_mod[1:Nindiv,'Length2'])
Nyears <- 40
Nareas <- length(unique(ATR_mod$Area1))

Params = list(ln_kmean=log(0.2), ln_Linf=log(200), ln_L0=log(20), ln_kdev=rep(0,Nindiv), ln_sd_kdev=log(0.01), ln_sd_obs=log(20))
obj = MakeADFun(data=Data, parameters=Params, random="ln_kdev")

# Run model
newtonOption(smartsearch=FALSE)
obj$fn(obj$par)
obj$gr(obj$par)
obj$control <- list(trace=10)
obj$hessian <- TRUE
opt = nlminb( start=obj$par, objective=obj$fn )

# Inspect results
summary(obj)
Report = sdreport(obj)

# Append model outputs to ATR_mod
ATR_mod$Length1_hat <- Report$value[names(Report$value) %in% "Length1_hat"]
ATR_mod$Length2_hat <- Report$value[names(Report$value) %in% "Length2_hat"]

plot.obs.pred()
plot.histogram()
plot.indiv.growth()
plot.linf()

# END
