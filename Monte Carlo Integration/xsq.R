optn = function(nvec,s){
  avg_hx = c()
  diff = 1
  i = 1
  while(diff >0.005){
    n = nvec[i]
    x = s[1:n]
    avg_hx[i] = mean(x^2)
    ssq = sum((x - avg_hx[i])^2)/(n-1)
    se = ssq/sqrt(n)
    diff = (avg_hx[i]+1.96*se)-(avg_hx[i]-1.96*se)
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
  nvec = seq(from=1000,to=100000,by=5000)
  s = runif(n=max(nvec),a,b)

  nopt = optn(nvec = nvec,s=s)
  nvec = seq(from=10000,to=12000,by=100)
  nopt = optn(nvec,s)
  nvec = seq(from=7000,to=10000,by=100)
  nopt = optn(nvec,s)
  nvec = seq(from=7000,to=8000,by=1)
  nopt = optn(nvec,s)
  noptvec[i] = nopt[[1]]
}

summary(noptvec)

"
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
7346    7539    7592    7590    7645    7813 
"
