##############################################################
#########   LEARNING MARKED   ################################
##############################################################

# 05/22/2020


library(marked)

# data structure
data(dipper) # example data
head(dipper)

# record for each capture history (ch)

process.data(dipper)

pd <- process.data(dipper)
head(pd[[1]])
head(dipper)

dd <- make.design.data(pd)
head(dd[[1]])

?model.matrix


create.dmdf(pd)