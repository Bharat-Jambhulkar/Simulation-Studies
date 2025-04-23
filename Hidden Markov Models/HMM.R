## Generate a observed and an unobserved series 

TPM = round(matrix(c(2/3,1/3,1/2,1/2),nrow=2,ncol=2,byrow = T),4)

## State space for Yt = {0,1}
## Possible value for Xt = {2,3,4}
## Write an emission matrix 

E = round(matrix(c(1/3,2/3,2/3,1/3),nrow=2,ncol=2,byrow=T),4)

Y_state = c(1,2)
Yt = c()
n=10
Yt[1] = sample(Y_state,size = 1,prob=c(1/2,1/2))
for(i in 2:n){
  if(Yt[i-1] == 1) Yt[i] = sample(Y_state,size = 1,prob = TPM[1,])
  if(Yt[i-1] == 2) Yt[i] = sample(Y_state,size = 1,prob = TPM[2,])
}

## Generate a observed chain 
Xt = c()
X_values = c(3,4)
for(i in 1:n){
  if(Yt[i] == 1) Xt[i] = sample(X_values,size=1,prob = c(1/3,2/3))
  if(Yt[i] == 2) Xt[i] = sample(X_values,size=1,prob = c(2/3,1/3))
}
Xt

pihatmat = matrix(nrow=1000,ncol=2)
qmathat = matrix(nrow=2,ncol=2)
tpmhat = matrix(nrow=2,ncol=2)

qmathat[1,] = c(0.1,0.9)
qmathat[2,] = c(0.6,0.4)

pihatmat[1,] = c(0.5,0.5)

tpmhat[1,1] = 0.5;tpmhat[2,1] = 0.5;tpmhat[1,2] = 0.5;tpmhat[2,2] = 0.5

V = matrix(nrow=1000,ncol=2)

yhat = c()
#yhat[1] = sample(c(1,2),size=1,prob=c(0.5,0.5))
Xt[1]

V[1,1] = pihatmat[1,1]*qmathat[1,2]

V[1,2] = pihatmat[1,2]*qmathat[2,2]
head(V)
for(i in 2:length(Xt)){
  if(Xt[i]==3){
    V[i,1] = V[i-1,1]*tpmhat[1,1]*qmathat[1,1]
    V[i,2] = V[i-1,2]*tpmhat[2,1]*qmathat[2,1]
  } 
  if(Xt[i]==4){
    V[i,1] = V[i-1,1]*tpmhat[1,2]*qmathat[1,2]
    V[i,2] = V[i-1,2]*tpmhat[2,2]*qmathat[2,2]
  }
  yhat[i-1] = which.max(V[i-1,])
}
yhat
