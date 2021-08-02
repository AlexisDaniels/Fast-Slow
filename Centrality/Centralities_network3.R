############## TRUE NETWORK WITH OPTIMISM ###########

beta3 <- getmatrix(mod3, "beta")
# rownames(beta3) <- labels_short3
# colnames(beta3) <- labels_short3
# diag(beta3) <- NA

#Explore network's centrality
true3 = as.igraph(true3)

V(true3)$name <- xvars3
E(true3)$weight <- abs(E(true3)$weight)

#Expected Influence
influence3 <- expectedInf(true3, step = c("both", 1, 2), 
                          directed = T)
influence3 <- influence3[2]
influence3 <- influence3$step2
influence3 <- influence3 %>% sort(decreasing = TRUE)  #Optimism is the highest in ExpInf

#Strength
strength3 <- igraph::strength(true3, mode = "all", loops = FALSE,
                              vids = V(true3))
strength3 <- sort(strength3, decreasing = TRUE) #Optimism is the highest in strength
#However, optimism is mostly connected to the 2nd and 3rd strongest nodes..

results_true_3 <- list("influence" = influence3, "strength" = strength3)

results_true_3
############ TRUE NETWORK WITHOUT OPTIMISM ############

#Explore network's centrality
true3 = as.igraph(true3_wo)

V(true3)$name <- xvars3[1:4]
E(true3)$weight <- abs(E(true3)$weight)

#Expected Influence
influence3 <- expectedInf(true3, step = c("both", 1, 2), 
                          directed = T)
influence3 <- influence3[2]
influence3 <- influence3$step2
influence3 <- influence3 %>% sort(decreasing = TRUE)  #Optimism is the highest in ExpInf

#Strength
strength3 <- igraph::strength(true3, mode = "all", loops = FALSE,
                              vids = V(true3))
strength3 <- sort(strength3, decreasing = TRUE) #Optimism is the highest in strength
#However, optimism is mostly connected to the 2nd and 3rd strongest nodes..

results_true_3wo <- list("influence" = influence3, "strength" = strength3)


#################### BOOTSTRAPPING ##################
############ SIMULATED NETWORK WITH OPTIMISM ############


############ SIMULATED NETWORK WITHOUT OPTIMISM ############


############ SIMULATED NETWORK WITH LOW OPTIMISM ############
nBoots = 3

beta_low_3 <- beta3
beta_low_3[,5] <- beta_low_3[,5] * 0.5
beta_low_3[5,1:4] <- beta_low_3[5,1:4] * 0.5
beta_low_3 == beta3

f <- function(i){ # specify the desired function and parameter values here
  bootstrappin(n_sim = 1500, beta = beta_low_3, kappa = kappa3, vars1 = xvars3, vars2 = xvars3,
               Layout = Layout)
}

res_low_3w <- lapply(X = 1:nBoots, f)

## Clean up the results 
get <- function(data, type, vars) {
  result <- matrix(data = NA, nrow = length(vars), ncol = nBoots)
  for(i in 1:nBoots) {
    if(type == "strength") {
      result[,i] <- data[[i]]$strength
    } else if (type == "influence") {
      result[,i] <- data[[i]]$influence
    }
  }
  return(result)
}

strength_list_low_3w <- get(data = res_low_3w, type = "strength", vars = xvars3)
influence_list_low_3w <- get(data = res_low_3w, type = "influence", vars = xvars3)


############ SIMULATED NETWORK WITHOUT LOW OPTIMISM ############

f <- function(i){ # specify the desired function and parameter values here
  bootstrappin(n_sim = 1500, beta = beta_low_3, kappa = kappa3, vars1 = xvars3, 
               vars2 = xvars3[1:4], Layout = Layout[1:4,])
}


system.time({
res_low_3wo <- lapply(X = 1:nBoots, f)
})

## Clean up the results 
strength_list_low_3wo <- get(data = res_low_3wo, type = "strength", vars = xvars3[1:4])
influence_list_low_3wo <- get(data = res_low_3wo, type = "influence", vars = xvars3[1:4])




############ SIMULATED NETWORK WITH HIGH OPTIMISM ############

beta_high_3 <- beta3
beta_high_3[,5] <- beta_high_3[,5] * 1.5
beta_high_3[5,1:4] <- beta_high_3[5,1:4] * 1.5
beta_high_3 == beta3

f <- function(i){ # specify the desired function and parameter values here
  bootstrappin(n_sim = 1500, beta = beta_high_3, kappa = kappa3, vars1 = xvars3, vars2 = xvars3,
               Layout = Layout)
}

res_high_3w <- lapply(X = 1:nBoots, f)

## Clean up the results 
strength_list_high_3w <- get(data = res_high_3w, type = "strength", vars = xvars3)
influence_list_high_3w <- get(data = res_high_3w, type = "influence", vars = xvars3)

############ SIMULATED NETWORK WITHOUT HIGH OPTIMISM ############

f <- function(i){ # specify the desired function and parameter values here
  bootstrappin(n_sim = 1500, beta = beta_high_3, kappa = kappa3, vars1 = xvars3, 
               vars2 = xvars3[1:4], Layout = Layout[1:4,])
}

res_high_3wo <- lapply(X = 1:nBoots, f)

## Clean up the results 
strength_list_high_3wo <- get(data = res_high_3wo, type = "strength", vars = xvars3[1:4])
influence_list_high_3wo <- get(data = res_high_3wo, type = "influence", vars = xvars3[1:4])

