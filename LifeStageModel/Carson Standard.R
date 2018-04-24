################################################# 
# Title: Example of Pop Dynamics Model Code 
# Purpose: Seeded with popualtion data from March OCEANS survey at mid tide height, and N Site Spring M. californianus matrix from Carson et al. 2011
# Created by : L. Pandori
# Created on : 12/4/17
# Last Edited : 12/4/17
#################################################
# clear workspace
rm(lists=ls())
# load required packages
library(popbio)
# give stages from Carson et al. 2011
stages<-c('j', 'a1', 'a2')

# give spring northern M cali population matrix from Carson et al. 2011
A<-matrix(c(0.0074, 14.33, 22.13, 0.0216, 0.1659, 0, 0, 0.1487, 0.1877), nrow = 3, byrow = TRUE, dimnames = list(stages,stages))
A

# create original population size vector using population data from ccsp spring survey from OCEANS (mid tide height only)
n<-c(3824,3877,2299)

# create population projection matrix
  # parts: 
      # A = lefkovitch matrix
      # n = original pop matrix
      # X = number of timesteps
p<-pop.projection(A,n,50)

# look at summary 

# can find lambda
p$lambda

# can find population sizes at each step
p$pop.sizes

plot(p$pop.sizes)

p$lambda
# To find quasi time to extinction...if extinction = < 20 individuals
stoch.quasi.ext(A,n,Nx = 20, tmax = 50, maxruns = 10, nreps = 5000)
