### Generate a Markov Chain ####

Y = c()
Y[1] = sample(c(1,2,3),prob=c(0.5,0.3,0.2),size=1)
tpm = matrix(c(0.7,0.2,0.1,0.3,0.5,0.2,0.2,0.5,0.3),nrow=3,ncol=3,byrow = T)

n=100
st = c(1,2,3)
for(i in 1:n){
  if(Y[i]==1) Y[i+1] = sample(st,size=1,prob = c(0.7,0.2,0.1))
  if(Y[i]==2) Y[i+1] = sample(st,size=1,prob = c(0.3,0.5,0.2))
  if(Y[i]==3) Y[i+1] = sample(st,size=1,prob = c(0.2,0.5,0.3))
}

Y

