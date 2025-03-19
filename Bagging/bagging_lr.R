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


### Logistic Regression Setup
n=100
avec= runif(n,-1,1)
bvec = avec + runif(n,2,3)
p=2
X = matrix(nrow=n,ncol=p-1)
for(i in 1:n){
  X[i] = runif(1,min=avec[i],max=bvec[i])
}

betavec = c(0.01,-1.5)

pihat = round(exp(betavec[1]+betavec[1]*X)/(1+exp(betavec[1]+betavec[1]*X)),4)
#hist(pihat)
yvec =c()
for(i in 1:n){
  yvec[i] = rbinom(1,size=1,prob=pihat[i])
}
data = cbind(X,yvec)
colnames(data) = c("x","y")
head(data)
table(data[,2])

## Bootstrap
B = 2000
Yhatmat = matrix(nrow=n,ncol=B)
for(i in 1:B){
  s = sample(1:n,size=n,replace = T)
  datastar = data[s,]
  fit = glm(datastar[,2] ~ datastar[,1],family = "binomial")
  Yhatmat[,i] = fit$fitted.values
}
#Yhatvec = round(rowMeans(Yhatmat),4)
Yhatmat = ifelse(Yhatmat<=0.5,0,1)
#View(Yhatmat)

Yhatvec = c()

for(i in 1:nrow(Yhatmat)){
  ct = table(Yhatmat[i,])
  Yhatvec[i]=ifelse(ct[1]<ct[2],1,0)
}
##Confusion Matrix
table(yvec,Yhatvec)
