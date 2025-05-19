### Kernel Density Estimation ####
n=100
X = rbeta(n,2,4)

hist(X)
m = min(X);m
M = max(X);M

avec=seq(from=m,to=M+0.5,length.out=1000)
aden = numeric(length = length(avec))

### Uniform Kernal ####
h=0.3
for(i in 1:length(avec)){
  u = (avec[i]-X)/h
  w = which(abs(u)<=1)
  if(length(w)==0) aden[i] = 0
  if(length(w)!=0){
    ku = rep(0.5,length(w))
    aden[i] = sum(ku)/n*h
  }
}
plot(avec,aden,type="l",lwd=2)

### Triangular Kernal ####

h=0.5
for(i in 1:length(avec)){
  u = (avec[i]-X)/h
  w = which(abs(u)<=1)
  if(length(w)==0) aden[i] = 0
  if(length(w)!=0){
    ku = (1-abs(u[w]))
    aden[i] = sum(ku)/n*h
  }
}
plot(avec,aden,type="l",lwd=2)

### Epanechnikov Kernal ####
h=0.5
for(i in 1:length(avec)){
  u = (avec[i]-X)/h
  w = which(abs(u)<=1)
  if(length(w)==0) aden[i] = 0
  if(length(w)!=0){
    ku = (3/4)*(1-(u[w])^2)
    aden[i] = sum(ku)/n*h
  }
}
plot(avec,aden,type="l",lwd=2)

### Gaussian Kernal ####
h=0.2
for(i in 1:length(avec)){
  u = (avec[i]-X)/h
  ku = dnorm(u)
  aden[i] = sum(ku)/n*h
}
plot(avec,aden,type="l",lwd=2)
