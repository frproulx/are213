h <- 30

# added identity function to kernel (cuts off outside bw)
kernel.epa <- function(u){
  if(abs(u)<1){return(0.75*(1-u*u))
  }else{return(0)}
}

smokelist <- with(ps1.data.clean, (which(tobacco.rescale == 1)))
nonsmokelist <- with(ps1.data.clean, (which(tobacco.rescale == 0)))

data.sm <- with(ps1.data.clean[smokelist,], data.frame(birth.weight = dbrwt, weight = 1/propensityreduced))

data.ns <- with(ps1.data.clean[nonsmokelist,], data.frame(birth.weight = dbrwt, weight = 1/(1-propensityreduced)))

kd.est <- function(data, x = "x", weight = "weight", x.val, kernel = "epa"){
  # data is a dataframe with x-values and weights
  # x is the name of x-variables
  # weight is the weights for each
  # x.val is the particular value of x that is being evaluated
  # kernel is the kernel type (ONLY EPA NOW)
  
  
  
  
}

ps3k.sm <- with(ps1.data.clean, mean(propensityreduced[which(dbrwt == 3000 & tobacco.rescale == 1)]))
ps3k.ns <- with(ps1.data.clean, mean(propensityreduced[which(dbrwt == 3000 & tobacco.rescale == 0)]))




##for(i in 1:nrow(subset(ps1.data.clean, tobacco.rescale == 1))){
##with(subset(ps1.data.clean, tobacco.rescale == 1),
##     kern3000.sm.num <- kern3000.sm.num +
##      kernel.epa(((propensity3000.sm-propensityreduced[i])/h)*dbrwt))
## with(subset(ps1.data.clean, tobacco.rescale == 1),
##      kern3000.sm.den <- kern3000.sm.den +
##      kernel.epa((propensity3000.sm-propensityreduced[i])/h))
## }
## kern3000.sm <- kern3000.sm.num / kern3000.sm.den


## kernel3000.sm <- with(subset(ps1.data.clean,tobacco.rescale == 1), data.frame(window = (3000 - dbrwt/h)))
## kernel3000.sm$numerator <- with(subset(ps1.data.clean, tobacco.rescale == 1), kernel.epa(((3000/propensity3000.sm) - (dbrwt/propensityreduced))/h))
## kernel3000.sm$denominator <- with(subset(ps1.data.clean, tobacco.rescale == 1), kernel.epa(((3000/propensity3000.sm) - (dbrwt/propensityreduced))/h))

## with(kernel3000.sm[window < 1 & window > -1], sum(numerator))/(nrow(kernel3000.sm[abs(window < 1)])*h)
