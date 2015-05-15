# Make sure R is clean
rm(list = ls())

# Load tagGrowth
require(TMB)

# Compile code
system("rm ../../inst/executables/ATR.o ../../inst/executables/ATR.so")
compile("../../inst/executables/ATR.cpp", "-O0 -g")

# TMB debugger
gdbsource("Estimate_Simulations.R")
gdbsource("Estimate_Simulations.R", interactive = TRUE)

