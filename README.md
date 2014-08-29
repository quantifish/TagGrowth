tag-growth
==========

Variation among individuals can be easily included by treating each individual's demographic parameters as a random effect that arises from a population-level distribution.  We start with the specialized von Bertalanffy growth function:

dL/dt = a - bL

where dL/dt is change in length as a function of time, a scales with energy acquisition, and b represents metabolic upkeep costs.  However, individuals that are more highly active may obtain more food (increased a) and simultaneously have greater upkeep costs (increased b).  Following Shelton et al. (2013), we include this correlation via the following equation:

a = γbΨ

where γ and Ψ approximate the allometric scaling of energy costs and acquisition.  Integration then yields:

L(t) = L(t)

where Δt is the number of time-periods elapsed between length intervals, and where the Brody growth coefficient k = b/3 (as in the conventional von Bertalanffy growth function).  Readers are referred to Shelton et al. (2013) for an expanded model that incorporates variability in γ over time, although we retain the assumption that b varies among individuals (and hence has subscript i), where it follows a normal distribution (truncated at zero) with estimated mean and variance parameters.
