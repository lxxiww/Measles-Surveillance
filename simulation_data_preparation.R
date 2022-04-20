##################
####    simulation data prepare
##################
load('real_data_covariates_only.RData')
library(fields)
library(geosphere)
library(mvtnorm)
expCov<-function(distMat,phi)  exp(-distMat/phi)
generate_beta <- function(phi,sig, dist = distance){
  expCov<-function(distMat,phi)  exp(-distMat/phi)
  Cov<-sig*sig*expCov(dist,phi)
  beta <- as.numeric(rmvnorm(n=1,mean=rep(0,nrow(Cov)), sigma = Cov, method = 'chol'))
  return(beta)
}
scale_age <- function(age, cutoff = 25){
  x = age/12
  x[x>cutoff] = cutoff
  return(round(x,2))
}

##########################################
sim_setting = list()
spatial_dependence =  c(200,500,800) 
set.seed(54)
for(i in 1:3){
  sim_vac = generate_beta(spatial_dependence[i] ,sig = 1,dist_D)
  #plot_ethiopia_dist(data.frame(index_D = 1:70, sim_vac),'sim vac')
  sim_age =  generate_beta(spatial_dependence[i] ,sig = 1,dist_D)
  #plot_ethiopia_dist(data.frame(index_D = 1:70, sim_age),'sim age')
  sim_W =  generate_beta(spatial_dependence[i] ,sig = 1,dist_D)
  #plot_ethiopia_dist(data.frame(index_D = 1:70, sim_W),'sim W')
  sim_setting[[i]] = data.frame(index_D = 1:70, beta_age = sim_age, 
                                beta_vac = sim_vac, W = sim_W)
}
sim_d = covariates #real_data_covariates without confirmation results
sim_d$age = scale_age(sim_d$age)
temp = sim_d%>% left_join(sim_setting[[1]], by = 'index_D')
sim_d$P_low = 1/(1+exp(-(sim_d$age * temp$beta_age + sim_d$vac*temp$vac + temp$W )))
temp = sim_d%>% left_join(sim_setting[[2]], by = 'index_D')
sim_d$P_mid = 1/(1+exp(-(sim_d$age * temp$beta_age + sim_d$vac*temp$vac + temp$W )))
temp = sim_d%>% left_join(sim_setting[[3]], by = 'index_D')
sim_d$P_high = 1/(1+exp(-(sim_d$age * temp$beta_age + sim_d$vac* temp$vac + temp$W )))
################################
sim_d$Y_low = rbinom(dim(sim_d)[1], 1, sim_d$P_low)
sim_d$Y_mid= rbinom(dim(sim_d)[1], 1, sim_d$P_mid)
sim_d$Y_high = rbinom(dim(sim_d)[1], 1, sim_d$P_high)
head(sim_d)

