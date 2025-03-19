data = iris[1:100,1:5]
pairs(data[1:4], main = "Iris Data",
      pch = 21, bg = c("green3", "blue")[unclass(data$Species)])

"
Non-parametric Bootstrap Method
"
n = length(data$Species)
B = 2000
betahatmat = matrix(nrow=B,ncol=2)
for(b in 1:B){
  idx = sample(1:n,size = n,replace = T)
  datastar = data[idx,]
  model <- glm(datastar$Species ~ datastar$Sepal.Width, data = datastar, family = binomial)
  betahatmat[b,1]=model$coefficients[1]
  betahatmat[b,2]=model$coefficients[2]
}

View(betahatmat)
par(mfrow=c(1,2))
hist(betahatmat[,1],main = "Histogram of Intercept Parameter", 
     xlab = "Values", 
     ylab = "Frequency", 
     col = "lightblue", 
     border = "black",)

hist(betahatmat[,2],main = "Histogram of Slope Parameter", 
     xlab = "Values", 
     ylab = "Frequency", 
     col = "lightgreen", 
     border = "black",)

## 95% Quantile based confidence interval for parameters
q1=round(quantile(betahatmat[,1],probs = c(0.025,0.975)),4)
q2=round(quantile(betahatmat[,2],probs = c(0.025,0.975)),4)

## 95% normal approximation confidence interval for parameters
Xbar1 = mean(betahatmat[,1])
seboot1=sqrt(var(betahatmat[,1]))
LL1=Xbar1-1.96*seboot1; round(LL1,4)
UL1=Xbar1+1.96*seboot1; round(UL1,4)


Xbar2 = mean(betahatmat[,2])
seboot2=sqrt(var(betahatmat[,2]))
LL2=Xbar2-1.96*seboot2; round(LL2,4)
UL2=Xbar2+1.96*seboot2; round(UL2,4)


plot(density(betahatmat[,1]),main="Intercept Parameter",lwd=2,col="lightblue",xlab="values")
points(q1, c(0, 0), pch = 19, col = "red")
points(c(LL1,UL1), c(0, 0), pch = 19, col = "darkblue")
legend("topright",fill=c("red","darkblue"),legend=c("Quantile","Normal"))

plot(density(betahatmat[,2]),main="Slope Parameter",lwd=2,col="lightgreen",xlab="values")
points(q2, c(0, 0), pch = 19, col = "red")
points(c(LL2,UL2), c(0, 0), pch = 19, col = "darkblue")
legend("topleft",fill=c("red","darkblue"),legend=c("Quantile","Normal"))


#### Semi-Parametric Approach 

"
Steps:
Fit the model
get fitted probabilities 
get fitted class
Suffle the residuals
construct the response variable
"

View(data)

binary = c(rep(0,50),rep(1,50))
data = cbind(data$Sepal.Width,binary)
data  =data.frame(data)
colnames(data) = c("Sepal.Width","Species")
View(data)

fit = glm(data$Species~data$Sepal.Width,family = "binomial")

fitted_class = c()

w1 = which(fit$fitted.values<=0.5)
w2 = which(fit$fitted.values>0.5)

fitted_class[w1] = 0

fitted_class[w2] = 1

View(cbind(data$Species,fitted_class))

res = data$Species - fitted_class

View(cbind(data$Species,fitted_class,res))
B=2000
betahatmat = matrix(nrow=B,ncol=2)
for(b in 1:B){
  s = sample(res,size=50,replace=T)
  ystar = fitted_class + s
  ystar[which(ystar<0)]=0
  ystar[which(ystar>1)]=1
  fit1 = glm(ystar~data$Sepal.Width,family = "binomial")
  betahatmat[b,1] = fit1$coefficients[1]
  betahatmat[b,2] = fit1$coefficients[2]
}

View(betahatmat)

par(mfrow=c(1,2))
hist(betahatmat[,1],main = "Histogram of Intercept Parameter", 
     xlab = "Values", 
     ylab = "Frequency", 
     col = "#7F00FF", 
     border = "black",)

hist(betahatmat[,2],main = "Histogram of Slope Parameter", 
     xlab = "Values", 
     ylab = "Frequency", 
     col = "#33FFFF", 
     border = "black",)

## 95% Quantile based confidence interval for parameters
q1=round(quantile(betahatmat[,1],probs = c(0.025,0.975)),4)
q2=round(quantile(betahatmat[,2],probs = c(0.025,0.975)),4)

## 95% normal approximation confidence interval for parameters
Xbar1 = mean(betahatmat[,1])
seboot1=sqrt(var(betahatmat[,1]))
LL1=Xbar1-1.96*seboot1; round(LL1,4)
UL1=Xbar1+1.96*seboot1; round(UL1,4)


Xbar2 = mean(betahatmat[,2])
seboot2=sqrt(var(betahatmat[,2]))
LL2=Xbar2-1.96*seboot2; round(LL2,4)
UL2=Xbar2+1.96*seboot2; round(UL2,4)


plot(density(betahatmat[,1]),main="Intercept Parameter",lwd=2,col="lightblue",xlab="values")
points(q1, c(0, 0), pch = 19, col = "red")
points(c(LL1,UL1), c(0, 0), pch = 19, col = "darkblue")
legend("topright",fill=c("red","darkblue"),legend=c("Quantile","Normal"))

plot(density(betahatmat[,2]),main="Slope Parameter",lwd=2,col="lightgreen",xlab="values")
points(q2, c(0, 0), pch = 19, col = "red")
points(c(LL2,UL2), c(0, 0), pch = 19, col = "darkblue")
legend("topleft",fill=c("red","darkblue"),legend=c("Quantile","Normal"))


