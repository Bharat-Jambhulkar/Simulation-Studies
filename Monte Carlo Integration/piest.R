nvec = seq(from=10000,to=100000,by=10000)

phiest = c()
for(i in 1:length(nvec)){
  n = nvec[i]
  s = matrix(runif(2*n),ncol=2)
  rcheck = (s[,1]^2)+(s[,2]^2)
  n1 = length(which(rcheck<=1))
  phiest[i] = 4*n1/n
}

phiest

plot(log(nvec),phiest,main="Pi Value Estimation",xlab="pi",ylab="sample size",type = "b",col="#009999",lwd=2,xaxt="n")
abline(h=pi,lwd=2,col="black")
axis(1,at=nvec, labels=nvec)

plot(s,ylim=c(-1,1),xlim=c(-1,1),pch=16)
w = which(rcheck<=1)
lines(s[w,],col="red",type="p",pch=17)
