## Acceptance-Rejection Sampling
n = 500
set.seed(10)
par=list(shape=3,rate=6)
x = rgamma(n,shape=par$shape,rate=par$rate)

plot(density(x),col='red',lwd=2,ylim=c(0,3),main="Density Plot",xlab = "x")

abline(h=3*(1/(max(x)-min(x))),col="#00FFFF",lwd=2)

scalar=3*(1/(max(x)-min(x)))
news = c()
i=1
while(i<=n){
  x1 = runif(1,min=min(x),max=max(x)) #Sample from uniform
  u1 = runif(1,min=0,max=scalar) #Rejection criteria
  if(u1<dgamma(x1,shape = par$shape,rate=par$rate)){
    news[i] = x1
    i = i+1
  }
}
cbind(summary(news),summary(x))

lines(density(news),col="darkblue",lwd=2)
legend("topright",legend = c("Actual Function","A-R Sample"),fill=c("red","darkblue"))


