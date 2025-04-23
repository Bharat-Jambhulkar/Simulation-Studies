## Simulate series

lambda = 2;phi=0.6
n=200
X = c()
X[1] = rpois(1,lambda)
for(i in 2:n){
  X[i] = rbinom(1,size=X[i-1],prob = phi) + rpois(1,lambda)
}
ts.plot(X)
acf(X)

#### CLS Estimates ####
#t = length(X)
Y1 = X[-n];Y2 = X[-1] 
num = sum(Y2*Y1) - (sum(Y2)*sum(Y1))/(n-1)

denom = sum(Y1^2) - (sum(Y1))^2/(n-1)

cls_phi = round(num/denom,4)
cls_lambda = (1/n)*(sum(Y2)-cls_phi*sum(Y1))

cat(cls_phi,cls_lambda)
#### Likelihood Maximization ####


L = function(parvec){
  phihat = parvec[1]
  lhat = parvec[2]
  PT = c()
  for(i in 2:n){
    T1 = 0
    z = min(X[i-1],X[i])
    for(r in 0:z){
      T1 = T1 + dbinom(r,size = z,prob=phihat)*dpois(r,lambda = lhat)
    }
    PT[i] = T1 
  }
  PT = PT[-1]
  return(-prod((PT)))
}

startpar = c(cls_phi,cls_lambda)
L(startpar)

Umat = matrix(c(1,0,1,0,0,1),nrow=3,ncol=2,byrow = T)
Cvec = matrix(c(0,-1,0),ncol=1)

A=constrOptim(startpar,f=L,grad = NULL,ui=Umat,ci=Cvec)
round(A$par,4)
