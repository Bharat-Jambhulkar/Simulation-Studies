nvec = seq(from=1,to=100000,by=1000)
a = 0
b = 1
s = runif(n=max(nvec),a,b)
avg_hx = c()

intmat = matrix(nrow=length(nvec),ncol=2)
for(i in 1:length(nvec)){
  n = nvec[i]
  x = s[1:n]
  avg_hx[i] = mean(x^2)
  ssq = sum((x - avg_hx[i])^2)/(n-1)
  se = ssq/sqrt(n)
  intmat[i,] = cbind(avg_hx[i]-1.96*se,avg_hx[i]+1.96*se)
}
summary(avg_hx)
plot(nvec,avg_hx,col="red",type="l")
abline(h=1/3)
lines(intmat[,1],col="blue")
lines(intmat[,2],col="purple")
