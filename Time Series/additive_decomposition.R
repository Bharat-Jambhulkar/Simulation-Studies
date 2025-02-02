data = read.csv("BeerUse.csv")

#View(BeerUse) 

plot(BeerUse$Production,type="l",lwd=2)

y = BeerUse$Production

## 4-MA Trend Estimation
Tt = c()
m=4 #Quarter
k = ceiling((m-1)/2)
for(i in 1:length(y)){
  if(i<(length(y)-k)){
    Tt[i+(k-1)] = mean(y[c(i:(i+(m-1)))])
  }
}

Tt = na.omit(Tt)

lines(Tt,col="red",lwd=2)

## To calculate 2X4-MA 

Tt2 = c()
m=2 
k = ceiling((m-1)/2)
for(i in 1:length(Tt)){
  if(i<(length(Tt)-k)){
    Tt2[i+k] = mean(Tt[c(i:(i+(m-1)))])
  }
}

Tt2 = na.omit(Tt2)

lines(Tt2,col="blue",lwd=2)

Tt2.adj = c(0,0,Tt2,0,0,0) #Adjust length of both vectors 

## Calculate Detrend Series
Detrend = y-Tt2.adj

Q1 = c()

i = 1

while(i<=length(Detrend)){
  Q1[i] = Detrend[i]
  i = i+4
}

S1  = mean(Q1,na.rm = T) #Adjustment is remaining

Q2 = c()

i = 2

while(i<=length(Detrend)){
  Q2[i] = Detrend[i]
  i = i+4
}

S2  = mean(Q2,na.rm=T)

Q3 = c()

i = 3

while(i<=length(Detrend)){
  Q3[i] = Detrend[i]
  i = i+4
}


S3  = mean(Q3,na.rm=T)

Q4 = c()

i = 4

while(i<=length(Detrend)){
  Q4[i] = Detrend[i]
  i = i+4
}

S4  = mean(Q4,na.rm=T)

seasonal_index = c(S1,S2,S3,S4)


sn_idx = seasonal_index-mean(seasonal_index) #Adjusted such that all add to zero.

round(sum(sn_idx),4)

St = rep(sn_idx,length.out = 154)

It = y-Tt2.adj-St #remainder

plot(It[-c(1:2,152:154)],type="l",lwd=2,col="red",main="Remainder Component",ylab="values",xlab="timepoints")

acf(It)

## Additive Decomposition Done.
#########
