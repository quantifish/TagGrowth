Welcome to TagGrowth
==========

The software **TagGrowth** includes an estimation model for analysing growth using
tag-recapture data where the individual fish have been aged upon recapture and a
model for simulating growth of individual fish to test the performance of the
estimation model.


## Table of contents

- [Introduction](#introduction)
- [Installation](#installation)
- [Case study](#case-study)
- [Simulation study](#simulation-study)
- [Further reading](#further-reading)


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

![equation](http://latex.codecogs.com/gif.latex?L%28t&plus;%5CDelta_t%29%3DL%28t%29%5Cexp%5Cleft%28%5Cfrac%7B-k%7D%7Bn_%5CDelta%7D%5CDelta_t%5Cright%29&plus;%5Cleft%28%5Cfrac%7B-k%7D%7Bn_%5CDelta%7D%5Cright%29%5E%7B%5Cpsi-1%7D%5Cleft%281-%5Cexp%5Cleft%28%5Cfrac%7B-k%7D%7Bn_%5CDelta%7D%5Cright%29%5Cright%29%5Cfrac%7B%5Cgamma%7D%7Bn_%5CDelta%7D%5Csum%5E%7B%5CDelta_t-1%7D_%7Bj%3D1%7D%5Cexp%5Cleft%28%5Cfrac%7B-k%7D%7Bn_%5CDelta%7Dj%5Cright%29&plus;z_%7B%5CDelta_t%7D)

where

![equation](http://latex.codecogs.com/gif.latex?z_%7B%5CDelta_t%7D%20%5Csim%20%5Cmathcal%7BN%7D%20%5Cleft%280%2C%20%5Csigma%5E2_z%20%5Cleft%5B%20%5Cleft%28%20%5Cfrac%7B-k%7D%7Bn_%5CDelta%7D%20%5Cright%29%5E%7B%5Cpsi-1%7D%20%5Cleft%28%201%20-%20%5Cexp%20%5Cleft%28%20%5Cfrac%7B-k%7D%7Bn_%5CDelta%7D%20%5Cright%20%29%20%5Cright%20%29%20%5Cright%5D%5E2%20%5Csum%5E%7B%5CDelta_t%20-%201%7D_%7Bj%3D1%7D%20%5Cexp%20%5Cleft%28%20-2%20%5Cfrac%7B-k%7D%7Bn_%5CDelta%7D%20j%20%5Cright%20%29%20%5Cright%29)

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
    devtools::install_github("quantifish/TagGrowth")
    
    # Load package
    library(TagGrowth)

Please see the examples folder for an example of how to run the model.


##Case study

A case stduy is done using Antarctic toothfish in `examples/case_study/`. The tag-recapture (`TR.RData`) and aging (`AGE.RData`) data are linked using the script `Link_AGE_TR.R`. The linked data set is provided in `data/ATR_mod.RData`. The original data sets `TR.RData` and `AGE.RData` are not provided.

We implement this model using the Template Model Builder (TMB) software called from R using the TMB package (https://github.com/kaskr/adcomp). The model is written in C++ `inst/executables/ATR.cpp`, and an R script `Fit_Models.R` loads the data and fits the model. The script `plot.R` is also provided to plot the outputs of the case study.


##Simulation study

A simulation study based on Antarctic toothfish is done in `examples/simulation_study/`. Simulation is done in `Simulate_Growth.R`. Estimation is then done using `Estimate_Simulations.R`. The script `plot.R` is also provided to plot the outputs of the case study.


##Further reading

Need tp put Shelton ref here.

A Zotero (https://www.zotero.org/) bibliography is provided in `examples/TagGrowth.rdf`.

http://www.codecogs.com/latex/eqneditor.php
