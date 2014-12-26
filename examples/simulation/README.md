# Simulation

Simulation of data sets is done using Simulate_Growth.R. Simulate_Growth.R calls
Growth_Model.R to run the simulations.

The model is then fit to simulated data sets using Estimate_Simulations.R.

![equation](http://www.sciweavers.org/tex2img.php?eq=1%2Bsin%28mc%5E2%29%0D%0A&bc=White&fc=Black&im=jpg&fs=12&ff=arev&edit=0)

| Random effects | pdH | Folder |
| -------------- |:---:|:------:|
| None           | 97  | v0     |
| k              | 78  | v1     |
| z              | 90  | v2     |
| k, z           | 95  | v3     |


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



