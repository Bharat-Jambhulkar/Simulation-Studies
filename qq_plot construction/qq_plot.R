my_qq = function(samp,dist,param){
  if(class(dist)=="character" & class(param)=="list"){
   if(length(samp)>1){
    if(dist=="uniform"){
      theoretical_quant = qunif(ppoints(length(samp)),min = param[[1]],max=param[[2]])
      quant = TRUE
    }else if(dist=="exponential"){
      theoretical_quant = qexp(ppoints(length(samp)),rate=param[[1]])
      quant = TRUE
    }else if(dist=="gamma"){
      theoretical_quant = qgamma(ppoints(length(samp)),shape = param[[1]],rate=param[[2]])
      quant = TRUE
    }else if(dist=="chi-squared"){
      theoretical_quant = qchisq(ppoints(length(samp)),df = param[[1]])
      quant = TRUE
    }else if(dist=="normal"){
      theoretical_quant = qnorm(ppoints(length(samp)),mean = param[[1]],sd=param[[2]])
      quant = TRUE
    }else if(dist=="cauchy"){
      theoretical_quant = qcauchy(ppoints(length(samp)),location = param[[1]],scale = param[[2]])
      quant = TRUE
    }else if(dist=="weibull"){
      theoretical_quant = qweibull(ppoints(length(samp)),shape = param[[1]],scale = param[[2]])
      quant = TRUE
    }else if(dist=="lognormal"){
      theoretical_quant = qlnorm(ppoints(length(samp)),meanlog = param[[1]],sdlog = param[[2]])
      quant = TRUE
    }else if(dist=="beta"){
      theoretical_quant = qbeta(ppoints(length(samp)),shape1 = param[[1]],shape2 = param[[2]])
      quant = TRUE
    }else if(dist=="t"){
      theoretical_quant = qt(ppoints(length(samp)),df = param[[1]])
      quant = TRUE
    }else if(dist=="f"){
      theoretical_quant = qf(ppoints(length(samp)),df1 = param[[1]],df2 = param[[2]])
      quant = TRUE
    }else{
      quant = FALSE
    }
    if(quant==T){
    qqplot(sort(samp),theoretical_quant, 
            main = paste("QQ Plot for", dist,"Distribution"), 
            ylab = "Theoretical Quantiles", 
            xlab = "Sample Quantiles")
    grid()
    abline(0, 1, col = "red", lwd = 2)}else{paste("Unsupported distribution.")} 
   }else{
      paste("Length should be greater than 1.")
    }  
    }else{
    paste("Data type not as expected. Make sure sample is numeric vector, dist is character string and parameter is a list.")
    }
}
s = rcauchy(200)

my_qq(s,dist = "cauchy",param = list(0,1))
