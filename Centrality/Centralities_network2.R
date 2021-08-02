############## TRUE NETWORK WITH OPTIMISM ###########

beta2 <- getmatrix(mod2, "beta")
rownames(beta2) <- labels_short2
colnames(beta2) <- labels_short2

#Explore network's centrality
true2 = as.igraph(true2)

V(true2)$name <- xvars2
E(true2)$weight <- abs(E(true2)$weight)

#Expected Influence
influence2 <- expectedInf(true2, step = c("both", 1, 2), 
                          directed = T)
influence2 <- influence2[2]
influence2 <- influence2$step2
influence2 <- influence2 %>% sort(decreasing = TRUE)  #Optimism is the highest in ExpInf

#Strength
strength2 <- igraph::strength(true2, mode = "all", loops = FALSE,
                              vids = V(true2))
strength2 <- sort(strength2, decreasing = TRUE) #Optimism is the highest in strength
#However, optimism is mostly connected to the 2nd and 3rd strongest nodes..

results_true_2 <- list("influence" = influence2, "strength" = strength2)


############ TRUE NETWORK WITHOUT OPTIMISM ############

#Explore network's centrality
true2 = as.igraph(true2_wo)

V(true2)$name <- xvars2[1:4]
E(true2)$weight <- abs(E(true2)$weight)

#Expected Influence
influence2 <- expectedInf(true2, step = c("both", 1, 2), 
                          directed = T)
influence2 <- influence2[2]
influence2 <- influence2$step2
influence2 <- influence2 %>% sort(decreasing = TRUE)  #Optimism is the highest in ExpInf

#Strength
strength2 <- igraph::strength(true2, mode = "all", loops = FALSE,
                              vids = V(true2))
strength2 <- sort(strength2, decreasing = TRUE) #Optimism is the highest in strength
#However, optimism is mostly connected to the 2nd and 3rd strongest nodes..

results_true_2wo <- list("influence" = influence2, "strength" = strength2)


#################### BOOTSTRAPPING ##################
############ SIMULATED NETWORK WITH OPTIMISM ############


############ SIMULATED NETWORK WITHOUT OPTIMISM ############


############ SIMULATED NETWORK WITH LOW OPTIMISM ############
nBoots = 3

beta_low_2 <- beta2
beta_low_2[,5] <- beta_low_2[,5] * 0.5
beta_low_2[5,1:4] <- beta_low_2[5,1:4] * 0.5
beta_low_2 == beta2

f <- function(i){ # specify the desired function and parameter values here
  bootstrappin(n_sim = 1500, beta = beta_low_2, kappa = kappa2, vars1 = xvars2, vars2 = xvars2,
               Layout = Layout)
}

res_low_2w <- lapply(X = 1:nBoots, f)

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

strength_list_low_2w <- get(data = res_low_2w, type = "strength", vars = xvars2)
influence_list_low_2w <- get(data = res_low_2w, type = "influence", vars = xvars2)


############ SIMULATED NETWORK WITHOUT LOW OPTIMISM ############

f <- function(i){ # specify the desired function and parameter values here
  bootstrappin(n_sim = 1500, beta = beta_low_2, kappa = kappa2, vars1 = xvars2, 
               vars2 = xvars2[1:4], Layout = Layout[1:4,])
}

res_low_2wo <- lapply(X = 1:nBoots, f)

## Clean up the results 
strength_list_low_2wo <- get(data = res_low_2wo, type = "strength", vars = xvars2[1:4])
influence_list_low_2wo <- get(data = res_low_2wo, type = "influence", vars = xvars2[1:4])




############ SIMULATED NETWORK WITH HIGH OPTIMISM ############

beta_high_2 <- beta2
beta_high_2[,5] <- beta_high_2[,5] * 1.5
beta_high_2[5,1:4] <- beta_high_2[5,1:4] * 1.5
beta_high_2 == beta2

f <- function(i){ # specify the desired function and parameter values here
  bootstrappin(n_sim = 1500, beta = beta_high_2, kappa = kappa2, vars1 = xvars2, vars2 = xvars2,
               Layout = Layout)
}

res_high_2w <- lapply(X = 1:nBoots, f)

## Clean up the results 
strength_list_high_2w <- get(data = res_high_2w, type = "strength", vars = xvars2)
influence_list_high_2w <- get(data = res_high_2w, type = "influence", vars = xvars2)

############ SIMULATED NETWORK WITHOUT HIGH OPTIMISM ############

f <- function(i){ # specify the desired function and parameter values here
  bootstrappin(n_sim = 1500, beta = beta_high_2, kappa = kappa2, vars1 = xvars2, 
               vars2 = xvars2[1:4], Layout = Layout[1:4,])
}

res_high_2wo <- lapply(X = 1:nBoots, f)

## Clean up the results 
strength_list_high_2wo <- get(data = res_high_2wo, type = "strength", vars = xvars2[1:4])
influence_list_high_2wo <- get(data = res_high_2wo, type = "influence", vars = xvars2[1:4])

