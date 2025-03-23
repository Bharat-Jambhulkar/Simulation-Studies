D = iris[,1:4]

y = D$Sepal.Length
X = D[,-1]
B=1000
yhatmat = matrix(nrow=length(y),ncol=B)
for(i in 1:B){
  idx = sample(1:(ncol(D)-1),size = 2,replace=T)
  Xnew = X[,c(idx)]
  fit = lm(y~.,data=Xnew)
  yhatmat[,i] = fit$fitted.values
}

yhatvec = rowMeans(yhatmat)

cor(y,yhatvec)
plot(y,yhatvec)
