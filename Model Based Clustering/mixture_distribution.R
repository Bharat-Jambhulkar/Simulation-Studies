## Sample from mixture of three distributions ##

## Cell probabilities 

p1=0.2
p2=0.5
p3 = 1-(p1+p2)
n=15
s = c()
for(i in 1:n){
  u = runif(1)
  if(u<p1) s[i] = rnorm(1,mean = 2,sd=1)
  if(u>=p1 & u<p1+p2) s[i] = rnorm(1,mean = 10,sd=1)
  if(u>=p1+p2) s[i] = rnorm(1,mean = 6,sd=1)
}

plot(density(s))
