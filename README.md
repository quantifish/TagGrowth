Welcome to TagGrowth
==========

The software TagGrowth includes an estimation model for analysing growth using
tag-recapture data where the individual fish have been aged upon recapture and a
model for simulating growth of individual fish to test the performance of the
estimation model.


##Introduction 

Variation among individuals can be easily included by treating
each individual's demographic parameters as a random effect that arises from a
population-level distribution. We start with the specialized von Bertalanffy
growth function:

dL/dt = a - bL

![equation](http://latex.codecogs.com/gif.latex?dL%2Fdt%3Da%2DbL)

where dL/dt is change in length as a function of time, a scales with energy
acquisition, and b represents metabolic upkeep costs. However, individuals that
are more highly active may obtain more food (increased a) and simultaneously
have greater upkeep costs (increased b). Following Shelton et al. (2013), we
include this correlation via the following equation:

a = γbΨ

![equation](http://latex.codecogs.com/gif.latex?a%3Dγb%5EΨ)

where γ and Ψ approximate the allometric scaling of energy costs and
acquisition. Integration then yields:

L(t+Δₜ) = L(t) e-bᵢΔₜ + bᵢ Ψ-1 (1 - e-bᵢ) γ sum j=0 Δₜ-1 e-bᵢⱼ + zᵢ

where Δt is the number of time-periods elapsed between length intervals, and
where the Brody growth coefficient k = b (as in the conventional von Bertalanffy
growth function). Readers are referred to Shelton et al. (2013) for an expanded
model that incorporates variability in γ over time, although we retain the
assumption that b varies among individuals (and hence has subscript i), where it
follows a normal distribution (truncated at zero) with estimated mean and
variance parameters.

Following previous notation, parameters are estimated by integrating across all
random effects b, while noting that Eq. 15 also requires estimation of Li(t0),
i.e., the length upon first observation for each individual.


##Installation

Install the package from R using

    # Install package
    install.packages("devtools")
    require(devtools)
    install_github("quantifish/TagGrowth")
    # Load package
    require(TagGrowth)

Please see the examples folder for an example of how to run the model.


##Simulation

"examples/simulation/ATR.R"


##Estimation 

We implement this model using the Template Model Builder (TMB) software called
from R using the TMB package (https://github.com/kaskr/adcomp). The model is
written in C++ "inst/executables/ATR.cpp", and an R script
"examples/estimation/ATR.R" loads the data and fits the model.


##Further reading

Citations to come.


![equation](http://latex.codecogs.com/gif.latex?1%2Bsin%28mc%5E2%29%0D%0A)
