## This exercise is to identify how many times LASSO identifies correct number 
## of redundant regressors.

library(glmnet) #For fitting LASSO

## With different sample size and diff num of sim

p = 70 #num of regressors
nonsig = 30
nvec = seq(from=80,to=1000,by=100) #num of obs
nsim=100

set_zero = c()
crr_zero = c()

for( j in 1:length(nvec)){
  n = nvec[j]
  sim_zero = c()
  sim_crr_zero = c()
  for(k in 1:nsim){
    avec = sample(-10:30,size=p,replace = T)
    bvec = avec+sample(10:40,size=p,replace = T)
  
    X = matrix(nrow=n,ncol=p)
    for(i in 1:p){
      X[,i] = runif(n,avec[i],bvec[i])
    }
  
    bvec = sample(-10:40,size=p,replace = T) #Regression coefficients 
  
  ## Put some regression coef to zero
    b0 = sample(1:p,size=nonsig,replace = F)
    bvec[b0] = 0
  
    Yvec = X%*%bvec + rnorm(n,0,1) #Response variable 
  
    fit=cv.glmnet(X,Yvec,alpha=1,nfolds = 5)
  
    fit1 = glmnet(X,Yvec,alpha=1,nfolds = 5,lambda = fit$lambda.min) 
    idx = which(fit1$beta==0)
    sim_zero[k] = p-fit$nzero[fit$index[1]]
    sim_crr_zero[k] = length(intersect(b0,idx))
    }
    set_zero[j] = max(sim_zero)
    crr_zero[j] = mean(sim_crr_zero)
}
set_zero
crr_zero
prop=round(crr_zero/30,4)

plot(nvec,prop,main="Proportion vs Sample Size",xlab = "Sample Size",ylab = "Proportion",lwd=2,col=rgb(0.2,0.1,0.5,0.9),type="b",xaxt="n")
axis(1,at = c(80, 180, 280, 380, 480, 580, 680, 780, 880, 980))
