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

ps3k.sm <- with(ps1.data.clean, mean(propensityreduced[which(dbrwt == 3000 & tobacco.rescale == 1)]))
ps3k.ns <- with(ps1.data.clean, mean(propensityreduced[which(dbrwt == 3000 & tobacco.rescale == 0)]))

# find smoking at 3000

# find vector of contributions to kernel weight, weighted by inverse p-score
for(i in 1:length(data.sm$birth.weight)){
  data.sm$ksum[i] <- data.sm$weight[i] * kernel.epa((data.sm$birth.weight[i] - 3000) / h)
}

reasonable.weight <- which(data.sm$weight < 10)

# sum contributions and divide by number of obs., 
kernel.3000.sm <- sum(data.sm$ksum) / (sum(data.sm$weight[reasonable.weight]) * length(data.sm$ksum) * h)

# this almost works but doesn't quite match the plots.  :(

