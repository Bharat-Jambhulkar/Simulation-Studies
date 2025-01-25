y1 = 130
y2 = 110
n1 =n2= 200

b1 = 6
a1 = (7/3)*b1-(4/3)

par11 = a1+y1;par12 = b1-y1+n1

N=500
p1 = rbeta(N,par11,par12)

a2 = 4
b2 = 4

par21 = a2+y2;par22 = b2-y2+n2

p2 = rbeta(N,par21,par22)


dif = p1-p2

plot(density(dif),lwd=2)

quantiles = quantile(dif, probs = c(0.025, 0.975))

abline(v=c(quantiles[1],quantiles[2]))

## zero is not included in the interval 
N=500 #sample size
contain_zrr = c() 
nsim = 100
nsimulation = 1000
for(j in 1:nsimulation){
  quantmat = matrix(nrow = nsim,ncol=2)
  for(i in 1:nsim){
    p1 = rbeta(N,par11,par12)
    p2 = rbeta(N,par21,par22)
    dif = p1-p2
    quantiles = quantile(dif, probs = c(0.025, 0.975))
    quantmat[i,] = cbind(quantiles[1],quantiles[2])
  }
  w=which(quantmat[,1]<0 & quantmat[,2]>0)
  contain_zrr[j]=length(w)
}

table(contain_zrr)

barplot(table(contain_zrr))


