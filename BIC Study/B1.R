library(MASS)
## Generate some X matrix values. 
## Consider some betavec where some regressors are actually 0.
## construct response variable uisng above data with addition of error.
## Use BIC to identify unused regressors.


nvec = seq(from=100,to=1000,by=100)
nreg=50
nsim=50
avg_prop = c()
for(k in 1:length(nvec)){
  prop = c()
  for(i in 1:nsim){
    n = nvec[k]
    avec = sample(-10:100,size=nreg,replace = T)
    bvec = avec+ sample(10:100,size=nreg,replace = T)
    X = matrix(nrow=n,ncol=nreg)
    for(j in 1:nreg){
      X[,j] = runif(n,min = avec[j],max = bvec[j])
    }

  # View(X) ## regressor matrix done.

    bvec = runif(nreg,min=-10,max=20)
    notsign = 20
    b0 = sample(1:nreg,size=notsign,replace = F)
    bvec[b0] = 0 # 20 regression coefficients are exactly zero.

  ## Construct response variable

    Yvec = X%*%bvec+rnorm(n,0,1)

  ## Fit a model
    X = as.data.frame(X)
    fit = lm(Yvec~.,data = X)

  ## Check proportion of correct identification of zero regressors 
    final_model = stepAIC(fit, direction = "both", k = log(n),trace = 0)  # BIC uses k = log(n)
    prop[i] = (nreg-length(coef(final_model)))/notsign
  }
avg_prop[k]=mean(prop)
}


## Probility of choosing true model is increasing with increase in sample size.

plot(nvec,avg_prop,xlab="sample size",ylab = "probability",main="Probability VS Sample size",pch=16,type="b",lwd=2,col="#0066CC",ylim = c(0.83,0.95))
grid()