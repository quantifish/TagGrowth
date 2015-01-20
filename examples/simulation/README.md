# Simulation

Simulation of data sets is done using Simulate_Growth.R. Simulate_Growth.R calls
Growth_Model.R to run the simulations. All simulations are saved as .RData
files.

The model is then fit to simulated data sets using Estimate_Simulations.R. The
parameter estimates for each simulation are saved along with the simulated data
as .RData files.

Table showing the simulation design, the number of fits that were positive
definite Hessian, and the directory that contains the 200 simulated/estimated
data sets.

| Random effects | Power |pdH  | Directory |
| -------------- |:-----:|:---:|:---------:|
| None           | 50    | 97  | v0/50     |
|                | 100   | 78  | v0/100    |
|                | 250   | 90  | v0/250    |
|                | 500   | 95  | v0/500    |
| k              | 50    | 97  | v1/50     |
|                | 100   | 78  | v1/100    |
|                | 250   | 90  | v1/250    |
|                | 500   | 95  | v1/500    |
| z              | 50    | 97  | v2/50     |
|                | 100   | 78  | v2/100    |
|                | 250   | 90  | v2/250    |
|                | 500   | 95  | v2/500    |
| k, z           | 50    | 97  | v3/50     |
|                | 100   | 78  | v3/100    |
|                | 250   | 90  | v3/250    |
|                | 500   | 95  | v3/500    |


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



