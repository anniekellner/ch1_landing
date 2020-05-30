##########################################################################
######    Adding environmental covariates to ice bear data    ############
##########################################################################

# May 23, 2020

rm(list = ls())

load('land_bears_ows.RData') # land bears 
head(land.bears.all.ows) # need SIC data back to June 1 

load('Ice_Measurements.RData')
head(ice.df)

load('lsm.RData')
head(lsm)

load('Patch.RData') # only 2 - 3 bears. From SDMTools analysis that no longer is available. Use lsm instead.
head(patch)
tail(patch)

load('great_circle_dist.RData') # swim only

load('ice.calc.RData') # distance to pack 
head(ice.calc)

load('ice_calc.RData')
head(tt)
