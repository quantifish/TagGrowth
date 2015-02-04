# Case Study - Antarctic toothfish

Antarctic toothfish are large...


## Methods

We identified those fish that had been tagged...


## Results

This folder contains all the code requied to fit a model.  Model outputs are
placed in each of the version folders depending on which random-effects are
being fitted. The models run, if they were positive definitive Hessian (pdH) and the folders they are stored
in is summarised in the table below.

| Random effects | pdH | Folder |
| -------------- |:---:|:------:|
| None           | Yes | v0     |
| k              | Yes | v1     |
| z              | Yes | v2     |
| y              | No  | v3     |
| k, z           | Yes | v4     |
| k, y           | -   | v5     |
| z, y           | Yes | v6     |
| k, z, y        | -   | v7     |


### No variation (v0)

Random effects: None
pdH: Yes
Directory: v0


### Variation in k (v1)

Random effects: k
pdH: Yes
Directory: v1


### Variation in z (v2)

Random effects: z
pdH: Yes
Directory: v2


### v3

Random effects: y
pdH: No
Directory: v3


### Variation in k and z (v4)

Random effects: k, z
pdH: Yes
Directory: v4


### v5

Random effects: k, y
pdH: Failed to fit
Directory: v5


### v6

Random effects: z, y
pdH: Yes
Directory: v6


### v7

Random effects: k, z, y
pdH: Failed to fit
Directory: v7

