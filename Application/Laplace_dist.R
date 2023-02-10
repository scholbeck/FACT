dlap = function(x,mu=0,sigma=1,log=FALSE){
  log.den = - log(2) - log(sigma) - abs(x-mu)/sigma
  if(log) log.den
  else exp(log.den)
}

# Cumulative Distribution Function
plap = function(x,mu=0,sigma=1,log.p=FALSE){
  unlist(lapply(x, function(xval){
    if(xval<mu){
      log.cdf1 = -log(2) + (xval-mu)/sigma
      if(log.p) return(log.cdf1)
      else return(exp(log.cdf1))
    }
    if(xval>=mu){
      log.cdf2 = -log(2) + (-xval-mu)/sigma
      if(log.p) log(1 - exp(log.cdf2))
      else 1 - exp(log.cdf2)
    }
  }))
}

# Quantile Function
qlap = function(p,mu=0,sigma=1){
  unlist(lapply(p, function(prob){
    if(prob<0.5)  return(mu + sigma*log(2*prob))
    if(prob>=0.5) return(mu - sigma*log(2*(1-prob)))
  }))
}

# Random number generation 
rlap = function(n,mu=0,sigma=1){
  u = runif(n)
  rgen = unlist(lapply(u, function(unif) ifelse(unif<0.5,mu + sigma*log(2*unif),mu - sigma*log(2*(1-unif)))))
  return(rgen)
}
