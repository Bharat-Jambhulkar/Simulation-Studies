## This exercise is to identify how many times LASSO identifies correct number 
## of redundant regressors.

library(glmnet) #For fitting LASSO

## Generate the data 

p = 70 #num of regressors

n = 300 #num of obs

avec = sample(-10:30,size=p,replace = T)
bvec = avec+sample(10:40,size=p,replace = T)

X = matrix(nrow=n,ncol=p)

for(i in 1:p){
  X[,i] = runif(n,avec[i],bvec[i])
}
#View(X) #Regressor matrix done.

bvec = sample(-10:40,size=p,replace = T) #Regression coefficients 

## Put some regression coef to zero
nonsig = 30
b0 = sample(1:p,size=nonsig,replace = F)

bvec[b0] = 0

Yvec = X%*%bvec + rnorm(n,0,1) #Response variable 

fit=cv.glmnet(X,Yvec,alpha=1,nfolds = 5)

x=fit$nzero[fit$index[1]]

