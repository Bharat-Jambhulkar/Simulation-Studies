##Steps
"
1. Take a dataset with continuous, categorical, and count features.
2. For each column take some missing value percentage. 
3. For each column generate a Bernoulli random number and for which index 
    Bernoulli is equal 1 put NA at that place.
4. Use different imputation technique to fill NA.
5. Create visualization
"


library(ISLR)
Odata = Auto
head(Odata)

sapply(Odata,class)
Odata$cylinders = as.integer(Odata$cylinders)
Odata$origin = as.integer(Odata$origin)

sapply(Odata,class)

set.seed(23325)
pr = seq(0.1,0.25,by=0.01)
prop = sample(pr,size = ncol(Odata)-1,replace=T)

idxmat = matrix(nrow=nrow(Odata),ncol=ncol(Odata)-1)

for(i in 1:ncol(idxmat)){
  idxmat[,i] = rbinom(n=nrow(idxmat),size=1,prob=prop[i])
}

head(idxmat)
for(i in 1:(ncol(Odata)-1)){
  w = which(idxmat[,i]== 1)
  Odata[w,i] = NA
}

head(Odata)

## Got the missing data

## Missing Percentage

round((colMeans(is.na(Odata)))*100,4)

sapply(Odata,class)

Idata = Odata

for(i in 1:ncol(Odata)){
  c = class(Odata[,i])
  if(c == "numeric"){
    Idata[is.na(Idata[,i]),i] = mean(Odata[,i],na.rm = T)
  }else if(c == "integer"){
    Idata[is.na(Idata[,i]),i] = median(Odata[,i],na.rm = T)
  }
}

## Graphs
## Density Plot

for(i in 1:(ncol(Idata)-1)){
  c = colnames(Idata[i])
  if(c != "cylinders" && c != "origin" && c != "displacement"){
    plot(density(Idata[,i]),lwd=2,col="#1E90FF",main="Density Plot",xlab=colnames(Idata[i]))
    lines(density(Auto[,i]),lwd=2,col="#FF1493")
    legend("topright",legend=c("Imputed","Original"),fill = c("#1E90FF","#FF1493"))
  }
}

plot(density(Auto$displacement),lwd=2,col="#FF1493",main="Density Plot",xlab="displacement")
lines(density(Idata$displacement),lwd=2,col="#1E90FF")
legend("topright",legend=c("Original","Imputed"),fill = c("#FF1493","#1E90FF"))

## Barplot

par(mfrow=c(1,2))
barplot(table(Auto$cylinders),ylim=c(0,250),col=rgb(0.8,0.1,0.1,0.6),xlab = "No. of Cylinders")

barplot(table(Idata$cylinders),ylim=c(0,250),col="#69b3a2",xlab = "No. of Cylinders")

legend("topright",legend=c("Original","Imputed"),fill = c(rgb(0.8,0.1,0.1,0.6),"#69b3a2"))


barplot(table(Auto$origin),ylim=c(0,300),col=rgb(0.8,0.1,0.1,0.6),xlab = "Origin")

barplot(table(Idata$origin),col="#69b3a2",ylim=c(0,300),xlab = "Origin")

legend("topright",legend=c("Original","Imputed"),fill = c(rgb(0.8,0.1,0.1,0.6),"#69b3a2"))

## Scatter Plot

plot(Auto$mpg,Auto$horsepower,main="Scatter Plot",xlab="mpg",ylab="horsepower",pch=16,col="#FF6347")
plot(Idata$mpg,Idata$horsepower,main="Scatter Plot",xlab="mpg",ylab="horsepower",pch=16,col="#FFD700")

legend("topright",legend=c("Original","Imputed"),fill = c("#FF6347","#FFD700"))


plot(Auto$horsepower,Auto$acceleration,main="Scatter Plot",xlab="acceleration",ylab="horsepower",pch=16,col="#FF6347")
plot(Idata$horsepower,Auto$acceleration,main="Scatter Plot",xlab="acceleration",ylab="horsepower",pch=16,col="#FFD700")

legend("topright",legend=c("Original","Imputed"),fill = c("#FF6347","#FFD700"))

## Done