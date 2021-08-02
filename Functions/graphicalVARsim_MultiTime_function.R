graphicalVARsim_MultiTime <- function (nTime, beta, kappa, mean = rep(0, ncol(kappa)), init = mean, 
                                       warmup = 100, lbound = rep(-Inf, ncol(kappa)), 
                                       ubound = rep(Inf, ncol(kappa)), 
                                       var_slow = c(3, 4), time_slow = c(5, 10)) {
  
  
  stopifnot(!missing(beta))
  stopifnot(!missing(kappa))
  ?stopifnot
  # number of nodes
  Nvar <- ncol(kappa)
  
  # first set of data points (0 unless specified)
  init <- rep(init, length = Nvar)
  ?rep
  # number of time points to simulate in total
  totTime <- nTime + warmup
  
  # empty data frame
  Data <- t(matrix(init, Nvar, totTime))
  ?t
  # inverse kappa matrix
  Sigma <- solve(kappa)
  ?solve
  
  for (t in 2:totTime) {
    for (n in 1:Nvar){
      
      # normal updating
      if (!(n %in% var_slow)){
        # temporal effect
        Data[t,n] <- mean[n] + t(beta %*% (Data[t - 1, ] - mean))[,n] + 
          # comtemp effect
          mvtnorm::rmvnorm(1, rep(0, Nvar), Sigma)[,n]
        
        # slow updating
      } else {
        slow <- rep(rep(0:1, c(time_slow[match(n, var_slow)]-1,1)), length.out = totTime)
        if (slow[t-1] == 0){
          Data[t,n] <- Data[t - 1,n]
        } else if (slow[t-1] == 1){
          Data[t,n] <- mean[n] + t(beta %*% (Data[t - 1, ] - mean))[,n] + 
            mvtnorm::rmvnorm(1, rep(0, Nvar), Sigma)[,n]
        }
      }
    }
    
    Data[t, ] <- ifelse(Data[t, ] < lbound, lbound, Data[t,])
    
    Data[t, ] <- ifelse(Data[t, ] > ubound, ubound, Data[t,])
    
  }
  # return without warmup
  return(Data[-seq_len(warmup), , drop = FALSE])
}

