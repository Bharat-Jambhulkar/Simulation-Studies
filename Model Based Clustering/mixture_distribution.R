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


#### Model Based Clustering/ Gaussian Mixture Model ####
n = 500
lbd = 0.3
true_mu1 =-1; true_sd1 = 1
true_mu2 = 3; true_sd2 = 1
y = rbinom(n,size=1,prob = lbd)
s1 = rnorm(n,mean=true_mu1,sd = true_sd1)
s2 = rnorm(n,mean=true_mu2,sd = true_sd2)

fin_s = y*s1+(1-y)*s2

## For visualization
m = min(fin_s);m
M = max(fin_s);M

plot(density(fin_s),xlim=c(m-0.5,M+0.5),lwd=2,ylim=c(0,0.4))
lines(density(s1),lwd=2,col=2)
lines(density(s2),lwd=2,col=3)
#View(cbind(s1,s2,y,fin_s))

## Estimate mu1, mu2, sigma1 and sigma2

d = 1
i=2
l = EI= mu1= mu2= s1 = s2 = c()
l[1] = 0.3;mu1[1] = 0;mu2[1] = 1; s1[1]=0.5; s2[1] = 1

while(d>0.001){
  Enum= dnorm(fin_s,mean=mu1[i-1],sd=s1[i-1])*l[i-1]
  Eden = (dnorm(fin_s,mean=mu1[i-1],sd=s1[i-1])*l[i-1]) + (dnorm(fin_s,mean=mu2[i-1],sd=s2[i-1])*(1-l[i-1]))
  EI = Enum/Eden
  l[i] = sum(EI)/n
  mu1[i] = sum(EI*fin_s)/sum(EI)
  mu2[i] = sum((1-EI)*fin_s)/sum(1-EI)
  s1[i] = sqrt(sum(EI*(fin_s-mu1[i])^2)/sum(EI))
  s2[i] = sqrt(sum((1-EI)*(fin_s-mu2[i])^2)/sum(1-EI))
  p1 = c(l[i],mu1[i],mu2[i],s1[i],s2[i]);p2 = c(l[i-1],mu1[i-1],mu2[i-1],s1[i-1],s2[i-1])
  d = max(abs(p1-p2))
  i = i+1
}
i
round(p1,4)
yhat=round(EI)
table(y,yhat)


## Plot original mixing density and estimated mixing density 

x = seq(from=m,to=M,length=500)
f1 = dnorm(x,mean=true_mu1,sd=true_sd1)
f2 = dnorm(x,mean=true_mu2,sd=true_sd2)

fmix = lbd*f1+(1-lbd)*f2

lhat = l[i-1]
f1hat = dnorm(x,mean=mu1[i-1],sd=s1[i-1])
f2hat = dnorm(x,mean=mu2[i-1],sd=s2[i-1])

fmixhat = lhat*f1hat+(1-lhat)*f2hat

plot(density(fmix),lwd=2,ylim=c(0,8))
lines(density(fmixhat),lwd=2,ylim=c(0,6),col=2)
