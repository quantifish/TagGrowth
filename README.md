Welcome to tag-growth
==========

The software tag-growth is a model for analysing growth using tag-recapture data where the individual fish have been aged upon recapture.

##Introduction
Variation among individuals can be easily included by treating each individual's demographic parameters as a random effect that arises from a population-level distribution.  We start with the specialized von Bertalanffy growth function:

dL/dt = a - bL

where dL/dt is change in length as a function of time, a scales with energy acquisition, and b represents metabolic upkeep costs.  However, individuals that are more highly active may obtain more food (increased a) and simultaneously have greater upkeep costs (increased b).  Following Shelton et al. (2013), we include this correlation via the following equation:

a = γbΨ

where γ and Ψ approximate the allometric scaling of energy costs and acquisition.  Integration then yields:

L(t) = L(t)

where Δt is the number of time-periods elapsed between length intervals, and where the Brody growth coefficient k = b (as in the conventional von Bertalanffy growth function).
