# Case Study - Antarctic toothfish

Antarctic toothfish are large Nototheniids native to the Southern Ocean.  They
can grow to be more than 2m in length, weighing over 100kg, and can live for up
to 50 years of age.  The exploratory toothfish fishery in the Ross Sea region
began in 1997 and is managed by The Commission for the Conservation of Antarctic
Marine Living Resources (CCAMLR).  Since then the fishery has increased to about
3000 tonnes per year.  Fishing is restricted to the summer months (December
usually until to March), once the ice shelf recedes allowing vessels access to
the region.  The Antarctic toothfish tagging programme was initiated in the 2001
fishing season by New Zealand vessels involved in the fishery.  In 2004,
toothfish tagging was made compulsory for all vessels participating in the
fishery.  Currently toothfish are required to be double tagged at a rate of 1
fish per tonne landed.  The tagging programme records information on the date,
depth, location, sex, and size of each tagged/recaptured fish.  A small subset
of the recaptured fish are aged by reading their otolith.  The otoliths are
assumed to be aged without error, a reasonable assumption for this species.

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

