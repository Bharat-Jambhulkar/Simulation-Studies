##LOOCV and k-fold CV

n = 100
X = runif(n,min=2,max=5)

bvec = sample(-5:5,size=2,replace = FALSE)

Yvec = c()

for(i in 1:n){
  Yvec[i] = bvec[1]+bvec[2]*X[i]+rnorm(1,0,2)
}

cor(Yvec,X)
plot(X,Yvec,pch=16,col="red",main = "Scatter Plot",xlab="X",ylab="Y")

##Leave one out cross validation
b0_est = c();b1_est = c()

for(i in 1:n){
  fit = lm(Yvec[-i]~X[-i])
  b0_est[i] = fit$coefficients[1]
  b1_est[i] = fit$coefficients[2]
}

## 5-fold cross validation
nf=5
cs=sample(rep(1:nf,each=n/nf),size=n,replace=FALSE)

IdxMat=matrix(nrow=n/nf,ncol=nf)
for(f in 1:nf)
{
  IdxMat[,f]=which(cs==f)
}

b0_est_cv = c();b1_est_cv = c()
for(j in 1:nf){
  vec=c(IdxMat[,-j])  
  Xfold=X[vec]  
  Yfold=Yvec[vec]
  fit1 = lm(Yfold~Xfold)
  b0_est_cv[j] = fit1$coefficients[1]
  b1_est_cv[j] = fit1$coefficients[2]
}
b0_est_cv
b0_est

plot(b0_est)
