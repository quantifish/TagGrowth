Welcome to tag-growth
==========

The software tag-growth includes an estimation model for analysing growth using
tag-recapture data where the individual fish have been aged upon recapture and
a model for simulating growth of individual fish to test the performance of the
estimation model.

##Introduction 

Variation among individuals can be easily included by treating
each individual's demographic parameters as a random effect that arises from a
population-level distribution. We start with the specialized von Bertalanffy
growth function:

dL/dt = a - bL

where dL/dt is change in length as a function of time, a scales with energy
acquisition, and b represents metabolic upkeep costs. However, individuals that
are more highly active may obtain more food (increased a) and simultaneously
have greater upkeep costs (increased b). Following Shelton et al. (2013), we
include this correlation via the following equation:

a = γbΨ

where γ and Ψ approximate the allometric scaling of energy costs and
acquisition. Integration then yields:

L(t) = L(t)

where Δt is the number of time-periods elapsed between length intervals, and
where the Brody growth coefficient k = b (as in the conventional von
Bertalanffy growth function). Readers are referred to Shelton et al. (2013) for
an expanded model that incorporates variability in γ over time, although we
retain the assumption that b varies among individuals (and hence has subscript
i), where it follows a normal distribution (truncated at zero) with estimated
mean and variance parameters.

Following previous notation, parameters are estimated by integrating across all
random effects b, while noting that Eq. 15 also requires estimation of Li(t0),
i.e., the length upon first observation for each individual.


##Installation

Install the package from R using

    # Install package
    install.packages("devtools")
    library("devtools")
    install_github("quantifish/tag-growth")
    # Load package
    library(tag-growth)

This does not work yet as we need to rename the pkg TagGrowth rather than
tag-growth or something.

Please see the examples folder for an example of how to run the model.


##Simulation

Details to come.


##Estimation 

We implement this model using the Template Model Builder (TMB) software called
from R using the TMB package (https://github.com/kaskr/adcomp). The model is
written in C++ "estimation/ATR.cpp", and an R script "estimation/ATR.R" loads
the data and fits the model.


##Further reading

Citations to come.
