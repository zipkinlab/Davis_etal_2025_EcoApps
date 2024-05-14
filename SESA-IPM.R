rm(list = ls())

# setup ------------------------------------------------------------------------
# load libraries, set working directory, and load functions
getwd()
setwd("")
options(max.print=99999)
library(openxlsx)
library(tidyverse)
library(lubridate)
library(coda)
library(ggpubr)
library(MCMCvis)
library(scales)
library(plotrix)

# the data ---------------------------------------------------------------------
dir()
dat <- read.csv("SESA_nests_2003_2022_18.csv")

# breeding pairs and productivity
year <- dat$year
nyears <- length(year)
y <- dat$minPairs # the number of breeding pairs per year (nest count but excludes renest attempts) 
j <- dat$minHatch # the number of chicks hatched
B <- dat$minNests # number of nests monitored
mean(y)

# read in covariate data 
yeareffects = read.xlsx("YearCovs.xlsx")

#fecundity covs
snow = yeareffects$Melt
fox_high = yeareffects$Fox_mid_high

#adult survival covs
pdo = yeareffects$PDO[1:20]
snow = as.numeric(as.vector(snow))
fox_high = as.numeric(as.vector(fox_high))

#standardize effects
zsnow <- as.vector(scale(snow[3:20]))
zpdo <- as.vector(scale(pdo[3:20]))

# the m-array ---------------------------------------------------------------------
#Load function to create a m-array based on capture-recapture data (CH)
marray <- function(CH){
  nind <- dim(CH)[1]
  n.occasions <- dim(CH)[2]
  m.array <- matrix(data = 0, ncol = n.occasions+1, nrow = n.occasions)
  
  # Calculate the number of released individuals at each time period
  for (t in 1:n.occasions){
    m.array[t,1] <- sum(CH[,t])
  }
  for (i in 1:nind){
    pos <- which(CH[i,]!=0)
    g <- length(pos)
    for (z in 1:(g-1)){
      m.array[pos[z],pos[z+1]] <- m.array[pos[z],pos[z+1]] + 1
    } #z
  } #i
  
  # Calculate the number of individuals that are never recaptured
  for (t in 1:n.occasions){
    m.array[t,n.occasions+1] <- m.array[t,1] - sum(m.array[t,2:n.occasions])
  }
  out <- m.array[1:(n.occasions-1),2:(n.occasions+1)]
  return(out)
}

########################################################################
# Capture-recapture data: m-array of adults (AHY)
########################################################################


#read in capture histories for birds marked as adults during 2003-2022
CH.A <- read.table("SESA_ad_2003_2022_18.txt")

#convert to matrix
CH.A <- data.matrix(CH.A)

#create m-array
marray.a <- marray(CH.A)


#############################################################################
# Integrated population model (IPM) for Utqiagvik SESA
# Code by Kayla Davis, Michigan State University, 2023
# Data provided by 
# Adapted from original scripts by Marc K?ry & Michael Schaub (2021)
# See main text for full description of modeling framework
#
# Notations:
# nyears = 18
# y = number of breeding pairs annually (recorded as minNests)
# J = number of eggs hatched annually

#############################################################################
set.seed(1)
sink("sesa_18")
cat("
model {
    #----------------------------------------
    # 1. Define the priors for the parameters
    #----------------------------------------
    
    # Priors for mean demographic parameters on appropriate scale
    mphia ~ dbeta(3,3) #3s is uninformative, try 20s for slightly more informative
    l.mphia <- log(mphia/(1-mphia))
    mfec ~ dunif(0,4)
    l.mfec <- log(mfec)
    mres ~ dbeta(3,3)
    l.mres <- log(mres/(1-mres))
    mgamma ~ dunif(0,25)
    l.mgamma <- log(mgamma)
   
    # Priors for beta coefficients
    beta.fec1 ~ dnorm(0, 0.1)
    beta.fec2 ~ dnorm(0, 0.1)
    beta.fec3 ~ dnorm(0, 0.1)

    beta.phia1 ~ dnorm(0, 0.1)
    beta.phia2 ~ dnorm(0, 0.1)
    beta.phia3 ~ dnorm(0, 0.1)
    
    beta.gam1 ~ dnorm(0, 0.1)
    beta.gam2 ~ dnorm(0, 0.1)
    beta.gam3 ~ dnorm(0, 0.1)

  

    # Precision of standard deviations of temporal variability
    sig.phia ~ dunif(0, 5)
    tau.phia <- pow(sig.phia, -2) 
    sig.res ~ dunif(0, 5)
    tau.res <- pow(sig.res, -2)
    sig.obs ~ dunif(0.5, 5)   # residual variance
    tau.obs <- pow(sig.obs, -2)


    # Distribution of error terms (Bounded to help with convergence)
    for (t in 1:(nyears-1)){
    epsilon.phia[t] ~ dnorm(0, tau.phia)T(-5,5)
    epsilon.res[t] ~ dnorm(0, tau.res)T(-5,5)
    }
    
    
    #----------------------------------------
    # 2. Define the linear models
    #----------------------------------------
    
    for (t in 1:(nyears-1)){
    logit(phia[t]) <- l.mphia + beta.phia1 * zpdo[t] + beta.phia2 * zsnow[t] + beta.phia3 * zsnow[t+1]
                      + epsilon.phia[t]       # zsnow is melt from previous year if using [t] and not [t+1]
    }
    
    for (t in 1:(nyears-4)){
    logit(p[t]) <- l.mres + epsilon.res[t] 
    }
    
    p[15] <- 0 
    
    for (t in (nyears-2):(nyears-1)){
    logit(p[t]) <- l.mres + epsilon.res[t] 
    }
    
    for (t in 1:nyears){
    log(f[t]) <- l.mfec +  beta.fec1 * zsnow[t] + beta.fec2 * (zsnow[t]^2) + beta.fec3 * fox_high[t] 
    } 
    
    for (t in 2:nyears){
    log(gamma[t]) <- l.mgamma + beta.gam1 * zsnow[t] + beta.gam2 * (zsnow[t]^2) + beta.gam3 * ((f[t-1] - mean(f))/sd(f))
                  
    }
   
    
    #-----------------------
    # 3. Derived parameters
    #-----------------------
    mean.phia <- exp(l.mphia)/(1+exp(l.mphia))        # Mean adult survival 
    mean.fec <- mean(f)              # Mean productivity/hatchling counts
    mean.fec.nofox <- exp(l.mfec)                   
    mean.fec.fox <- exp(l.mfec + beta.fec3)
    mean.gam <- exp(l.mgamma)                       # Mean number of immigrants
    mean.p <- exp(l.mres)/(1+exp(l.mres))

    # Population growth rate (total adult breeders [1+ y olds])
    for (t in 1:(nyears-1)){
    lambda[t] <- Nad[t+1] / (Nad[t] + 0.001)
    logla[t] <- log(lambda[t])
    }

    mlam <- exp((1/(nyears-1))*sum(logla[1:(nyears-1)]))   # Geo mean all yrs
    mlam.five <- exp((1/(nyears-13))*sum(logla[13:(nyears-1)]))    # Last 5 y
    mlam.ten <- exp((1/(nyears-8))*sum(logla[8:(nyears-1)]))    # Last 10 y

    mlam.fox <- exp((1/(nyears-6))*sum(logla[c(2:13)]))   # Geo mean fox control yrs
    mlam.no <- exp((1/(nyears-14))*sum(logla[c(14:(nyears-1))]))
    
    #predictions over new values for snow (100 from min to max zsnow) for mean fec (gam) and no fox control (fec)
    for (i in 1:nz){
    l.gam.pred[i] <- l.mgamma + beta.gam1 * z[i] + beta.gam2 * (z[i]^2)
    gam.pred[i] <- exp(l.gam.pred[i])
    l.fec.pred[i] <- l.mfec + beta.fec1 * z[i] + beta.fec2 * (z[i]^2)
    fec.pred[i]<- exp(l.fec.pred[i])
    l.phia.pred[i] <- l.mphia + beta.phia1 * mean(zpdo) + beta.phia2 * z[i] + beta.phia3 * mean(zsnow)
    phia.pred[i] <- ilogit(l.phia.pred[i])
    l.pdo.pred[i] <- l.mphia + beta.phia1 * zp[i] + beta.phia2 * mean(zsnow) + beta.phia3 * mean(zsnow)
    pdo.pred[i] <- ilogit(l.pdo.pred[i])
    }
    
    #--------------------------------------------
    # 4. The likelihoods of the single data sets
    #--------------------------------------------
    
    # 4.1. Likelihood for population count data (state-space model)
    
    # Model for initial adult population size (year 1)
      Nad[1] ~ dpois(y[1])
    
    # For years 2-20
    for (t in 2:nyears){
      S[t] ~ dbin(phia[t-1], Nad[t-1])      # No. surviving adults
      I[t] ~ dpois(gamma[t])               # No. immigrants
      }

    # Observation process
    for (t in 2:nyears){
      Nad[t] <- S[t] + I[t]       # Total number of breeding pairs
      y[t] ~ dnorm(Nad[t], tau.obs)T(0,100)
      }
    
    # 4.2 Likelihood for capture-recapture data: CJS model
    # Multinomial likelihood
    for (t in 1:(nyears-1)){
    marray.a[t,1:nyears] ~ dmulti(pr.a[t,], r.a[t])  
    q[t] <- 1-p[t]
    }
    
    # m-array cell probabilities for adults
    for (t in 1:(nyears-1)){
    # Main diagonal
    pr.a[t,t] <- phia[t]*p[t]
    # above main diagonal
    for (j in (t+1):(nyears-1)){
    pr.a[t,j] <- prod(phia[t:j])*prod(q[t:(j-1)])*p[j]
    } #j
    # Below main diagonal
    for (j in 1:(t-1)){
    pr.a[t,j] <- 0
    } #j
    # Last column
    pr.a[t,nyears] <- 1-sum(pr.a[t,1:(nyears-1)])
    } #t
    

    # 4.3. Likelihood for productivity data: Poisson regression
    for (t in 1:nyears) {
      j[t] ~ dpois(rho1[t])     # number eggs hatched
      rho1[t] <- f[t] * B[t]    # number of nests (B) and fecundity (f)
      }
    
    }
    ",fill = TRUE)
sink()

###################################################################
# Bundle data
jags.data <- list(zpdo = zpdo, zsnow = zsnow, fox_high = fox_high[3:20], 
                  nyears = nyears, marray.a = marray.a, r.a = rowSums(marray.a), 
                  y = y, j = j, B = B, 
                  z = seq(min(zsnow), max(zsnow), length.out = 100), zp = seq(min(zpdo), max(zpdo), length.out = 100), nz = 100)


# Initial values
set.seed(1)
inits <- function(){list(mphia = runif(1, 0.45, 0.55), mfec = runif(1, 0, 2), mres = runif(1, 0, 0.5), mgamma = runif(1, 1, 10), 
                         sig.phia = runif(1, 0.5, 5), sig.obs = runif(1, 2, 4), 
                         beta.fec1 = rnorm(1, 0, 1), beta.fec2 = rnorm(1, 0, 1), beta.fec3 = rnorm(1, 0, 1), 
                         beta.phia1 = rnorm(1, 0, 1), beta.phia2 = rnorm(1, 0, 1), beta.phia3 = rnorm(1, 0, 1), 
                         beta.gam1 = rnorm(1, 0, 1), beta.gam2 = rnorm(1, 0, 1), beta.gam3 = rnorm(1, 0, 1))} 


# Parameters monitored
parameters <- c("phia","f","lambda", "gamma", "p",
                "mean.phia","mean.fec","mean.fec.fox", "mean.fec.nofox", "mean.gam", "mean.p", 
                "mlam", "mlam.five", "mlam.ten", "mlam.fox", "mlam.no",
                "mphij", "mphia","mfec", "mres", "mgamma",
                "sig.phia", "sig.obs", "epsilon.phia", "epsilon.res",
                "beta.phia1", "beta.phia2", "beta.phia3", 
                "beta.fec1","beta.fec2", "beta.fec3", 
                "beta.gam1","beta.gam2", "beta.gam3",
                "S", "R", "I", "Nad", "Nck", "gam.pred", "fec.pred", "phia.pred", "pdo.pred")

# MCMC settings
# ni <- 1000000   
# nt <- 10
# nb <- 900000
# nc <- 3
# nadapt <- 500000

# Testing
ni <- 1000   
nt <- 2
nb <- 200
nc <- 3
nadapt <- 100

# Call JAGS from R
library(jagsUI)
sesa <- jags(jags.data, inits, parameters, "sesa_18", n.adapt = nadapt, n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE, store.data = TRUE)

sesa

# ~~~~ save output for use later ~~~~
save(sesa, file="sesa_18.Rdata")

# check out results
plot(sesa)
par(mfrow = c(1,1))
traceplot(sesa, parameter="phia")
traceplot(sesa, parameter="gamma")
traceplot(sesa, parameter="Nad")
traceplot(sesa, parameter="I")
traceplot(sesa, parameter="lambda")
traceplot(sesa, parameter="sig.obs")
traceplot(sesa, parameter="deviance")
traceplot(sesa, parameter="f")
traceplot(sesa, parameter="S")
  


MCMCsummary(sesa,
            params = c("beta.phia1","beta.phia2","beta.phia3","beta.fec1", "beta.fec2", "beta.fec3","beta.gam1", "beta.gam2", "beta.gam3"),
            probs = c(0.025, 0.05, 0.075, 0.1,0.25, 0.5, 0.75, 0.9, 0.925, 0.95, 0.975),
            round = 2)





# post-processing ---------------------------------------------------------------------

# probability that pop growth rate with fox > pop growth rate without fox control
sum(sesa$sims.list$mlam.fox > sesa$sims.list$mlam.no)/30000

