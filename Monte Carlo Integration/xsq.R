optn = function(nvec,s){
  avg_hx = c()
  diff = 1
  i = 1
  while(diff >0.005){
    n = nvec[i]
    x = s[1:n]
    avg_hx[i] = mean(x^2)
    ssq = sum((x - avg_hx[i])^2)/(n-1)
    se = sqrt(ssq)/sqrt(n)
    diff = (2*1.96*se)
    i = i+1
  }
  return(list(nvec[i-1],diff)) 
}


#difference is small at n=11000

## refine further
a = 0
b = 1
nsim = 500
noptvec = c()
for (i in 1:nsim){
  nvec = seq(from=1000,to=200000,by=5000)
  s = runif(n=max(nvec),a,b)
  nopt = optn(nvec = nvec,s=s)
  nvec = seq(from=50000,to=75000,by=100)
  s = runif(n=max(nvec),a,b)
  nopt = optn(nvec,s)
  nvec = seq(from=53000,to=56000,by=100)
  s = runif(n=max(nvec),a,b)
  nopt = optn(nvec,s)
  nvec = seq(from=54000,to=56000,by=1)
  s = runif(n=max(nvec),a,b)
  nopt = optn(nvec,s)
  noptvec[i] = nopt[[1]]
}

summary(noptvec)

"
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
7346    7539    7592    7590    7645    7813 
"
nvec2 = c()
nsim = 500
for( i in 1:nsim){
  nvec1 = seq(from=67500,to=69000,by=1)
  s = runif(max(nvec1),0,1)
  n0 = optn(nvec1,s)
  nvec2[i] =  n0[[1]]
}

summary(nvec2)
hist(nvec2)

"
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
67954   68217   68286   68293   68368   68648 
"
