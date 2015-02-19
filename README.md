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

![equation](http://latex.codecogs.com/gif.latex?%5Cfrac%7BdL%7D%7Bdt%7D%20%3D%20a%20-%20kL)

where dL/dt is change in length as a function of time, a scales with energy
acquisition, and k represents metabolic upkeep costs. However, individuals that
are more highly active may obtain more food (increased a) and simultaneously
have greater upkeep costs (increased b). Following Shelton et al. (2013), we
include this correlation via the following equation:

![equation](http://latex.codecogs.com/gif.latex?a_i%20%3D%20%5Cgamma%20k_i%5E%5Cpsi)

where γ and Ψ approximate the allometric scaling of energy costs and
acquisition. Integration then yields:

L(t+Δₜ) = L(t) e-bᵢΔₜ + bᵢ Ψ-1 (1 - e-bᵢ) γ sum j=0 Δₜ-1 e-bᵢⱼ + zᵢ

![equation](http://latex.codecogs.com/gif.latex?L%28t&plus;%5CDelta_t%29%3DL%28t%29%5Cexp%5Cleft%28%5Cfrac%7B-k%7D%7Bn_%5CDelta%7D%5CDelta_t%5Cright%29&plus;%5Cleft%28%5Cfrac%7B-k%7D%7Bn_%5CDelta%7D%5Cright%29%5E%7B%5Cpsi-1%7D%5Cleft%281-%5Cexp%5Cleft%28%5Cfrac%7B-k%7D%7Bn_%5CDelta%7D%5Cright%29%5Cright%29%5Cfrac%7B%5Cgamma%7D%7Bn_%5CDelta%7D%5Csum%5E%7B%5CDelta_t-1%7D_%7Bj%3D1%7D%5Cexp%5Cleft%28%5Cfrac%7B-k%7D%7Bn_%5CDelta%7Dj%5Cright%29&plusz_%7B%5CDelta_t%7D)

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
