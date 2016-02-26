# This is an R script to simulate data from
# a truncated  negative binomial
# and recover it using stan and Jags
# Created by: Andrew Bibian
# Date created: 09/18/2015

# The goal is simulate data as if they came from multiple
# years and are associate with two vital rate functions.
# one of the vital rate functions follows a normal distribution
# the other follows a negative binomial and both have
# uncorrelated random effect noise added on top 
# 
# V2: this differs in terms of the stan model parameterization
# No longer estimating random effect mean


# Removing objects from R session
rm(list=ls())

# Set working direction
setwd("C:/Users/MillerLab/Dropbox/Bayes group project/stan/Stan Neg. Bin. Test")
#setwd("C:/Users/ac79/Downloads/Dropbox/Bayes group project/stan/Stan Neg. Bin. Test")
#setwd("/Users/Bibsian/Dropbox/Bayes group project/stan/Stan Neg. Bin. Test")

###################################################################################
###... Install any required packages that are not currently installed ----------
####################################################################################
# List required packages
adm.req <-c('rstan', 'R2jags', 'R2OpenBUGS', 'MASS')

# Load currently installed, required packages
tmp <- lapply(adm.req, require, character.only = T)

# Find the required packages that still need to be installed
adm.need <- adm.req[!(paste0("package:",adm.req) %in% search())]

# Install required packages that are not currently installed
if(length(adm.need)>0){ install.packages(adm.need,dependencies=T) }

# Now, make sure all packages are loaded
tmp <- lapply(adm.req, require, character.only = T)

tmp


############ 
## Defining variables that will be used in the simulation
## 1) number of year effects (j)
##    number of observation (n)
## 2) mean and sd of random year effects (m1, m2/ sd1, sd2)
## 3) correlation cofficient (rho12)
## 4) mean of negative binomial (vnb.mu, emulates param of vital rate fxn)
## 5) alpha of negative binomal (vnb,alpha, emulates param of vital rate fxn)
## 6) mean of normal (vnorm.mu, emulates param of a vital rate fxn)
## 7) sd of normal (vnorm.sd, emulates param of a vital rate fxn)

#############
# Number of year effects
j<- 12

# number of obs per year
n<-100

# Defining mean and stanndard deviations for the 
# Two sets of random effects
m1<- 0
sd1<- 1.1

m2<- 0
sd2<- 1.8

# Negative binomial Parameters (emulates vital rate fxn)
vnb.mu<- 2
vnb.alpha<- 1.7

# Normal dist Parameters (emulates vital rate fxn)
vnorm.mu<- 0.8
vnorm.sd<-2

###########
## Simulating random year effects, one for
## each distribution
###########
ryr1<- rnorm(j, m1, sd1)
ryr2<- rnorm(j, m2, sd2)

# Creating the 'true' data
# True means for the normal and neg.bin parameters
# Note, every level of the random year effects (ryr)
# has n number of observation associated with it
df1<- rep(x= vnorm.mu + ryr1, each=n)
df2<- rep(x= vnb.mu + ryr2, each=n)


# Adding noise to the 'true' data to
# simulate what we would get in the field
df1<- rnorm( n=length(df1), mean=df1, sd= vnorm.sd)
df2<- rnbinom(n=length(df2), mu=exp(df2), size=vnb.alpha)

mean(df1)
mean(df2)

############
## Runing stan model
###########
stan.data<- list( n=length(df1), j=j, y1=df1, y2=df2, rfx= rep(seq(1,j, 1),n))
m.stan.ranef.v2<- 
'data{
  int n; // declaring the number of observations
  int j; // declaring the number of years (rand. eff.)
  real y1[n]; // declaring the data from the normal dist.
  int<lower=0> y2[n]; //declaring the data from neg.bin. dist
  int<lower=0> rfx[n]; // declaring index for random effects
}

parameters{
  
  
  //Inverse gamma
  // Random Effects
  //real ran_ef1_igamma[j]; // random effects 1
  //real ran_ef2_igamma[j]; // random effects 2
  
  // Mean and variance of RE
  //real<lower=0> ran_ef1_sd2_igamma; // random effects 1 sigma, inv_gamma prior
  
  //real<lower=0> ran_ef2_sd2_igamma; // random effect 2 sigma, inv_gamma prior
  
  //Uniform
  // Random Effects
  real ran_ef1_uniform[j]; // random effects 1
  real ran_ef2_uniform[j]; // random effects 2
  
  // Mean and variance of RE
  real<lower=0> ran_ef1_sd2_uniform; // random effects 1 sigma, inv_gamma prior
  
  real<lower=0> ran_ef2_sd2_uniform; // random effect 2 sigma, inv_gamma prior
  
  // Inverse gamma
  // Fixed Effects
  //real norm_mu_igamma; // mean of normal dist
  //real<lower=0> norm_sigma2_igamma; // sigma of normal dist
  
  //real<lower=0> nb_mu_igamma; // mean of neg.bin
  //real<lower=0> nb_alpha_igamma; // overdispersion of neg.bin
  
  //real global_int_norm_igamma; // global intercept for the normal model
  //real global_int_nb_igamma; // global intercept for the nb model

  //Uniform 
  real norm_mu_uniform; // mean of normal dist
  real<lower=0> norm_sigma2_uniform; // sigma of normal dist
  
  real<lower=0> nb_mu_uniform; // mean of neg.bin
  real<lower=0> nb_alpha_uniform; // overdispersion of neg.bin

  real global_int_norm_uniform; // global intercept for the normal model
  real global_int_nb_uniform; // global intercept for the nb mondel

  }
  
  transformed parameters{
    //real<lower=0> ran_ef1_sd_trans;
    //real<lower=0> ran_ef2_sd_trans;
    //real<lower=0> norm_sigma_trans;
    
    //ran_ef1_sd_trans <- sqrt (ran_ef1_sd2_igamma);
    //ran_ef2_sd_trans <- sqrt (ran_ef2_sd2_igamma);
    //norm_sigma_trans <- sqrt (norm_sigma2_igamma);
    
  
  }
  
  model{
  // declaring transformed mean
  // Inv Gamma
  //real yhat_norm_igamma[n];
  //real yhat_nb_igamma[n];
  
  // Uniform
  real yhat_norm_uniform[n];
  real yhat_nb_uniform[n];
  
  // Hyper Priors i.e. random effects
  
  // Inverse Gamma
  //ran_ef1_sd2_igamma ~ inv_gamma(0.01, 0.01);
  
  //ran_ef2_sd2_igamma ~ inv_gamma(0.01, 0.01);
  
  // Uniform
  ran_ef1_sd2_uniform ~ uniform(0, 100);
  
  ran_ef2_sd2_uniform ~ uniform(0, 100);
  
  // Random Effect draws
  for (i in 1:j){
  // Inverse gamma
    //ran_ef1_igamma[i]~ normal(0, ran_ef1_sd_trans);
    //ran_ef2_igamma[i]~ normal(0, ran_ef2_sd_trans);
    
    //Uniform
    ran_ef1_uniform[i]~ normal(0, ran_ef1_sd2_uniform); 
    ran_ef2_uniform[i]~ normal(0, ran_ef2_sd2_uniform);
  }
  
  // Priors
  // Inverse Gamma
  //norm_mu_igamma ~ normal(0, 100);
  //norm_sigma2_igamma ~ inv_gamma(0.01, 0.01);
  
  //nb_mu_igamma ~ gamma(0.01, 0.01);
  //nb_alpha_igamma ~ uniform (0.00001, 100);

  //global_int_norm_igamma ~ normal(0, 100);
  //global_int_nb_igamma ~ normal (0, 100);
  
  // Uniform
  norm_mu_uniform ~ normal(0, 100);
  norm_sigma2_uniform ~ uniform(0.00001, 100);
  
  nb_mu_uniform ~ gamma(0.01, 0.01);
  nb_alpha_uniform ~ uniform (0.00001, 100);
  
  global_int_norm_uniform ~ normal(0, 100);
  global_int_nb_uniform ~ normal(0, 100);
  
  // indexing the transformed mean 
  
  for ( i in 1:n){
  // Inverse Gamma
    //yhat_norm_igamma[i] <- ran_ef1_igamma[rfx[i]] + norm_mu_igamma + global_int_norm_igamma;
    //yhat_nb_igamma[i] <- ran_ef2_igamma[rfx[i]] + nb_mu_igamma + global_int_nb_igamma;  
    
    // Uniform
    yhat_norm_uniform[i] <- ran_ef1_uniform[rfx[i]] + norm_mu_uniform + global_int_norm_uniform;
    yhat_nb_uniform[i] <- ran_ef2_uniform[rfx[i]] + nb_mu_uniform + global_int_nb_uniform; 
  }
  
  // Likelihoods
  // Inv Gamma
  //y1 ~ normal(yhat_norm_igamma, norm_sigma_trans);
  //y2 ~ neg_binomial_2_log(yhat_nb_igamma, nb_alpha_igamma);
  
  // Uniform
  y1 ~ normal(yhat_norm_uniform, norm_sigma2_uniform);
  y2 ~ neg_binomial_2_log(yhat_nb_uniform, nb_alpha_uniform);  

}'


fit<- stan( model_code=m.stan.ranef.v2, data=stan.data, iter=5000, warmup=1000, chains=1)
post<-extract(fit)


stan2coda <- function(z) {
  mcmc.list(lapply(1:ncol(z), function(x) mcmc(as.array(fit)[,x,])))
}

fit.coda<- stan2coda(fit)
mcmcplot(fit.coda)
