# Tag-recapture data

This folder contains example capture-mark-recapture (CMR) data from a wild
population of Antarctic toothfish (*Dissostichus mawsoni*) in the Ross Sea.

Antarctic toothfish are large Nototheniids native to the Southern Ocean.  They
can grow to be more than 2m in length, weighing over 100kg, and can live for up
to 50 years of age.  The exploratory toothfish fishery in the Ross Sea region
began in 1997 and is managed by The Commission for the Conservation of Antarctic
Marine Living Resources (CCAMLR).  Since then the fishery has increased to about
3000 tonnes per year.  Fishing is restricted to the summer months (December
usually until to March), once the ice shelf recedes allowing vessels access to
the region.  The Antarctic toothfish tagging programme was initiated in the in
the 2001 fishing season by New Zealand vessels involved in the fishery.  In
2004, toothfish tagging was made compulsory for all vessels participating in the
fishery.  Currently toothfish are required to be double tagged at a rate of 1
fish per tonne landed.  The tagging programme records information on the date,
depth, location, sex, and size of each tagged/recaptured fish.  A small subset
of the recaptured fish are aged by reading their otolith.

We identified those fish that had been tagged, recaptured and aged upon
recapture.  This yielded 315 individuals of which 166 were female and 149 male.
All individuals were originally tagged between 2001 and 2008, and were
subsequently recaptured between 2002 and 2013.  These data allowed us to
identify the observed length (cm) at first capture *Lobs(t1)* and recapture
*Lobs(t1+t2)*. The time at liberty *(t2)* and the measured age of the fish at
recapture *(t1+t2)* was used to calculate the age of the fish at tagging *(t1)*.

The code used to link and groom the tag-recapture (TR.RData) and age (AGE.RData)
data sets is located in examples/case_study/Link_AGE_TR.R. The data are saved to
this directory as two separate files (ATR_mod.RData and ATR_mod_csv).
