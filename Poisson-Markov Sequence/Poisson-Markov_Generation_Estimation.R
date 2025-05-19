## Poisson Markov sequence and estimation of parameters
## Generate seq 

lambda = 3
alpha = 0.6
Xt = c()
n=100
Xt[1] = rpois(1,lambda)
for(i in 2:n){
  Xt[i] = rbinom(1,size=Xt[i-1],prob=alpha) + rpois(1,lambda)
}
ts.plot(Xt)

## Estimation part
alphavec = c()
lambdavec = c()
# initialize
alphavec[1] = 0.1
lambdavec[1] = 1
i=2
diff = 1
while(diff>0.001){
  Ewt =c()
  for(t in 2:n){
    Rw = 0:min(Xt[t],Xt[t-1])
    T1 = dpois(Xt[t]-Rw,lambda = lambdavec[i-1])
    T2 = dbinom(Rw,size=Xt[t-1],prob=alphavec[i-1])
    Num = T1*T2
    PMF = Num/sum(Num)
    Ewt[t] = sum(Rw*PMF)
  }
  Ezt = Xt[2:n] - Ewt[2:n] 
  alphavec[i] = sum(Ewt[2:n])/sum(Xt[1:(n-1)])
  lambdavec[i] = mean(Ezt)
  diff = max(abs(lambdavec[i]-lambdavec[i-1]),abs(alphavec[i]-alphavec[i-1]))
  i = i+1
}
i
lambdavec[i-1]
alphavec[i-1]
