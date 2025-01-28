## LD50 Dose example Bayesian Inference 

n = 15
yi = c(0,0,2,2,8,10,12,14,15,14)

#rs= sort(sample(1:20,size=5,replace = T))
#min(rs == sort(rs))
smat = matrix(nrow=length(yi),ncol=10)
i=1
while(i<11){
  rs = round(rbeta(10,shape1 = yi[i]+1,shape2= n-yi[i]+1),2)
  if(min(rs == sort(rs))== 1){
    smat[i,] = rs
    i = i+1
  }
}
View(smat)
smat
LD50 = matrix(nrow=10,ncol=2)
for(j in 1:ncol(smat)){
  for(i in 1:nrow(smat)){
    LD50[j,]=c(min(which(smat[,j]>=0.5)),smat[min(which(smat[,j]>=0.5)),j])
  }
}
LD50

barplot(LD50[,1])
