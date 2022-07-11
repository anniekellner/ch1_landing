##################################################
##    DO FEWER LAND BEARS HAVE COYS?  ############
##################################################

## There is no association between ice bears and coys (land bears and ice bears have coys in ~equal measure)

library(dplyr)

rm(list = ls())

# ------  Load data --------------- #

repro <- readRDS('./data/derived-data/repro.Rds')

chi <- select(repro, repro, bear_type)

table(chi$bear_type, chi$repro)

# Run chi-squared test with three categories (den, coy, yearling)

chisq.test(chi$bear_type, chi$repro) # not significant
fisher.test(chi$bear_type, chi$repro)

# Run with two categories (coy, not coy)

chi$coy <- ifelse(chi$repro == "coy", "coy", "not coy")
head(chi)

chisq.test(repro$bear_type, repro$coy) # not significant
