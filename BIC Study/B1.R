library(MASS)
## Generate some X matrix values. 
## Consider some betavec where some regressors are actually 0.
## construct response variable uisng above data with addition of error.
## Use BIC to identify unused regressors.

set.seed(1)
nreg=50
nsim=100
prop = c()
for(i in 1:nsim){
  avec = sample(-10:100,size=nreg,replace = F)
  bvec = avec+ sample(10:100,size=nreg,replace = F)

  n = 100
  X = matrix(nrow=n,ncol=nreg)
  for(j in 1:nreg){
    X[,j] = runif(n,min = avec[j],max = bvec[j])
  }

# View(X) ## regressor matrix done.

  bvec = runif(nreg,min=-10,max=20)
  notsign = 10
  b0 = sample(1:nreg,size=notsign,replace = F)
  bvec[b0] = 0 # 10 regression coefficents are exactly zero.

## Construct response variable

  Yvec = X%*%bvec+rnorm(n,0,1)

## Fit a model
  X = as.data.frame(X)
  fit = lm(Yvec~.,data = X)

## Check proportion of correct identification of zero regressors 
  final_model = stepAIC(fit, direction = "both", k = log(n),trace = 0)  # BIC uses k = log(n)
  prop[i] = (nreg-length(coef(final_model)))/notsign
}

prop
mean(prop)
