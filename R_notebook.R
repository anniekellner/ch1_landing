#Erase all
rm(list = ls())

################
#### MATRICES ##
################

#Set up
M=matrix(0,nrow = 3, ncol = 3)
M[,1]=c((2/32),(18/32),(20/32))
M[,2]=c((9/32),(3/32),(12/32))
M[,3]=c((11/32),(21/32), NA)
colnames(M) <- c("S", "S^c","Marginal")
rownames(M) <- c("R", "R^c", "Marginal")
View(M)

#Summing
M[3,1] + M[3,2]

###################################
### PROBABILITY DISTRIBUTIONS #####
###################################

#d = density; point estimate of [z_i]
#p = area to the left of z_i under the curve
#q = given quantile (e.g. 0.025), what is z_i
#r = simulate dataset given parameters ->  rnorm(10000,0,1) rnorm(n,mean,var)

# elements of lists are referred to using double brackets

df = as.data.frame(rbind(co[[1]], co[[2]], ....co[[n]])) #co=coda object in JAGS

##########################
#### for loop ##########

for (j in 1:length(N)){
  dNdt[j] <- r*N[j]*(1-N[j]/K) # Logistic Equation with population size j
}

##############################
### JAGS example ############
############################

# From ESS 575 (Tom Hobbs) - JAGS introduction


{
  sink("LogisticJAGS.R")
  cat("
      model{
      #priors
      K ~ dunif(0, 4000)
      r ~ dunif (0, 2)
      sigma ~ dunif(0, 2)
      tau <- 1 / sigma^2
      # likelihood
      for(i in 1:n){
      mu[i] <- r - r / K * x[i]
      y[i] ~ dnorm(mu[i], tau)
      }
      
      # Derived quantities
      
      max.dNdt <- K/2 # max growth rate
      
      for (j in 1:length(N)){
      dNdt[j] <- r*N[j]*(1-N[j]/K) #growth rate
      }
      }
      ", fill=TRUE)
  sink()
}

#Initial values for each chain 
inits = list( 
  list(K = 1500, r = .2, sigma = .01),
  list(K = 1000, r = .15, sigma = .5),
  list(K = 900, r = .3, sigma = .01))

N <- seq(0,1500,5)
N[1] <- 1 #not sure what this does

data = list(  
  n = nrow(Logistic),
  x = as.double(Logistic$PopulationSize),
  y = as.double(Logistic$GrowthRate),
  N=N)

# Call to JAGS

n.adapt = 5000 #number of iterations for adaptation (choose sampler and assure optimum mixing)
n.update = 10000 # number of iterations for burn-in
n.iter = 10000

jm = jags.model("LogisticJAGS.R", data = data, inits = inits, 
                n.chains = length(inits), n.adapt = n.adapt)
update(jm, n.iter = n.update)
zm = coda.samples(jm, variable.names = c("K", "r", "sigma", "tau", "max.dNdt", "dNdt"), 
                  n.iter = n.iter, n.thin = 1)

### RMarkdown
#Insert Image
![Alt text](C:/Users/akell/Dropbox/Spring_2018/ESS_575/Labs_mywork/DAG_occupancy_lab.png) #From ESS 575 Occupancy Lab
# Helpful website for embedding images into Markdown: https://rpubs.com/RatherBit/90926