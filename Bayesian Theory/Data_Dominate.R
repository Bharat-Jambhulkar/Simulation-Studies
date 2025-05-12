## Data Dominates

nseq = c(5,25,50,75)
theta = 2
th = seq(from=0.001,to=6,by=0.001)
al1 = 0.3;l1=0.8;al2=2;l2=3
prpdf1 = dgamma(th,rate=al1,shape=l1)
prpdf2 = dgamma(th,rate=al2,shape=l2)
plot(th,prpdf1,type='l',lwd=2,col=3,main='Prior PDF')
lines(th,prpdf2,type='l',lwd=2,col=4)

par(mfrow=c(2,2))
set.seed(1509)
for(i in 1:length(nseq)){
  n = nseq[i]
  x = rpois(n,theta)
## plot posterior density
  postpdf1 = dgamma(th,rate=(n+al1),shape=sum(x)+l1)
  postpdf2 = dgamma(th,rate=(n+al2),shape=sum(x)+l2)

  plot(th,postpdf1,type='l',lwd=2,col=3,main='Posterior PDF',ylim=c(0,max(postpdf1,postpdf2)+0.1),xlab = paste("th",sep=' ',n))
  lines(th,postpdf2,type='l',lwd=2,col=4)
}

