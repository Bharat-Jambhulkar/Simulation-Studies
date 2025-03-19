D=iris

Y=D[,5]
nc=length(unique(Y)); nc


B=1000
n=nrow(D); n
Samp=matrix(nrow=n,ncol=B)

## Notebook Second Approach 
for(i in 1:B)
{
  Samp[,i]=sample(1:n,size=n,replace=TRUE)
}
#View(Samp)

AC=matrix(nrow=n,ncol=B)
for(j in 1:B)
{ 
  PDFMat = matrix(nrow=n,ncol=nc)
  SD=D[Samp[,j],]
  G=list()
  G[[1]]=subset(SD,SD$Species=="setosa")
  G[[2]]=subset(SD,SD$Species=="versicolor")
  G[[3]]=subset(SD,SD$Species=="virginica")
  nvec=c(nrow(G[[1]]),nrow(G[[2]]),nrow(G[[3]]))
  Sig=(nvec[1]*cov(G[[1]][,1:4])+nvec[2]*cov(G[[2]][,1:4])+nvec[3]*cov(G[[3]][,1:4]))/sum(nvec)
  for(i in 1:nc){
    muhat=colMeans(G[[i]][,1:4])
    mumat=matrix(muhat,nrow=n,ncol=4,byrow=TRUE)
    OM=as.matrix(D[,1:4]-mumat)
    PDFMat[,i]=diag(OM%*%solve(Sig)%*%t(OM))
  }
  for(k in 1:nrow(AC)){
    AC[k,j]=which.min(PDFMat[k,])
  }
}
#Mode
FinClass = c()
for(i in 1:nrow(AC)){
  FinClass[i]=names(sort(table(AC[i,]))[length(table(AC[i,]))])
}

Finclass=strtoi(FinClass)
table(D[,5],Finclass)
