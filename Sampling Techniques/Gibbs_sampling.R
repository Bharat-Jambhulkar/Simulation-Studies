## Gibbs Sampling
"
Que: Suppose we want to generate a random sample from Bivariate Normal
distribution with mean (0,0) and var-cov mat = 1,0,0,1 (read row-wise)

step 1: take abitrary number (x0,y0) = (1.5,-0.5)
step 2: suppose marginal f(x|y) follows N(0,1) and f(y|x) follows N(0,1)
step 3: take y1 = rnorm(0.8*x0,1)
step 4: take x1 = rnorm(0.4*y1,1)
step 5: append (x1,y1) into data.

"
x = y = c()

x[1]=1.5
y[1]=-0.5
s = 100000

for(i in 2:s){
  y[i] = rnorm(1,mean = 0.8*x[i-1],1)
  x[i] = rnorm(1,mean = 0.4*y[i],1)
}

data = cbind(x,y)
#View(data)
Burn = 1000

smpl = data[-c(1:Burn),]

ts.plot(smpl[,1][1:1000])

acf(smpl[,1])

ls = seq(from=1,to=s-Burn,by=10)
head(ls)
final_smpl = smpl[ls,]
acf(final_smpl[,1]) #linear dependence is removed
acf(final_smpl[,2])
