# SLR
n=100
avec= runif(n,-1,1)
bvec = avec + runif(n,2,3)
p=2
X = matrix(nrow=n,ncol=p-1)
for(i in 1:n){
  X[i] = runif(1,min=avec[i],max=bvec[i])
}
betavec = sample(-8:5,size=p)
yvec =c()
for(i in 1:n){
  yvec[i] = betavec[1]+betavec[2]*X[i]+rnorm(1,0,2)
}
data = cbind(X,yvec)
colnames(data) = c("x","y")
head(data)

##Bootstrap

B = 2000
Yhatmat = matrix(nrow=n,ncol=B)
for(i in 1:B){
  s = sample(1:n,size=n,replace = T)
  datastar = data[s,]
  fit = lm(datastar[,2] ~ datastar[,1])
  Yhatmat[,i] = fit$fitted.values
}
View(Yhatmat)
Yhatvec = rowMeans(Yhatmat)

plot(yvec,Yhatvec,pch=16,col="red",main="Scatter Plot",xlab="Original",ylab="Aggregated")
cor(Yhatvec,yvec)
