# Simulation

could only do the power analysis for one data-generating scenario (i.e.,
variation in K and z) and all estimation models, with 4 x 4 x 200 runs.  I
think its reasonable to do another 4 x 4 x 200 runs, with 4 estimation
models, 4 simulation scenarios, and 200 replicates each, with sample size
fixed at what we saw in the data.  Breaking up factorials is a standard
trick, e.g., here:
http://www.nrcresearchpress.com/doi/abs/10.1139/cjfas-2012-0330.  Anyway,
this would double simulation design size, but wouldn't require rerunning
the power analysis.

Simulation of data sets is done using the script `Simulate_Growth.R`. `Simulate_Growth.R` calls
`Growth_Model.R` to run the simulations. All simulations are saved as `.RData`
files.

The model is then fit to each of the simulated data sets using `Estimate_Simulations.R`. The
parameter estimates for each simulation are saved along with the simulated data
as `.RData` files.

Table showing the simulation design, the number of fits that were positive
definite Hessian (pdH), and the directory that contains the 200 simulated/estimated
data sets.

WARNING: this takes a long time to run (over a week).

| Random effects | Power |pdH   | Directory |
| -------------- |:-----:|:----:|:---------:|
| None           | 50    | 196  | v0/50     |
|                | 100   | 195  | v0/100    |
|                | 250   | 194  | v0/250    |
|                | 500   | 191  | v0/500    |
| k              | 50    | 183  | v1/50     |
|                | 100   | 193  | v1/100    |
|                | 250   | 193  | v1/250    |
|                | 500   | 192  | v1/500    |
| z              | 50    | 60   | v2/50     |
|                | 100   | 86   | v2/100    |
|                | 250   | 172  | v2/250    |
|                | 500   | 189  | v2/500    |
| k, z           | 50    | 138  | v3/50     |
|                | 100   | 95   | v3/100    |
|                | 250   | 92   | v3/250    |
|                | 500   | 158  | v3/500    |



| Random effects | Model fitted | Power |pdH   | Directory |
| -------------- |:------------:|:-----:|:----:|:---------:|
| None           | 50    | 196  | v0/50     |
|                | 100   | 195  | v0/100    |
|                | 250   | 194  | v0/250    |
|                | 500   | 191  | v0/500    |
| k              | 50    | 183  | v1/50     |
|                | 100   | 193  | v1/100    |
|                | 250   | 193  | v1/250    |
|                | 500   | 192  | v1/500    |
| z              | 50    | 60   | v2/50     |
|                | 100   | 86   | v2/100    |
|                | 250   | 172  | v2/250    |
|                | 500   | 189  | v2/500    |
| k, z           | 50    | 138  | v3/50     |
|                | 100   | 95   | v3/100    |
|                | 250   | 92   | v3/250    |
|                | 500   | 158  | v3/500    |


## v0

Random effects: None

Now has sex specific gamma pars. Fixed psi = 0


## v1

Random effects: k

As above but with random effects on k.


## v2

Random effects: z


## v3

Random effects: k, z
