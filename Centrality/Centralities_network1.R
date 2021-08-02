############## TRUE NETWORK WITH OPTIMISM ###########

beta1 <- getmatrix(mod1, "beta")
rownames(beta1) <- labels_short1
colnames(beta1) <- labels_short1

#Explore network's centrality
true1 = as.igraph(true1)

V(true1)$name <- xvars1
E(true1)$weight <- abs(E(true1)$weight)

#Expected Influence
influence1 <- expectedInf(true1, step = c("both", 1, 2), 
                          directed = T)
influence1 <- influence1[2]
influence1 <- influence1$step2
influence1 <- influence1 %>% sort(decreasing = TRUE)  #Optimism is the highest in ExpInf

#Strength
strength1 <- igraph::strength(true1, mode = "all", loops = FALSE,
                              vids = V(true1))
strength1 <- sort(strength1, decreasing = TRUE) #Optimism is the highest in strength
#However, optimism is mostly connected to the 2nd and 3rd strongest nodes..

results_true_1 <- list("influence" = influence1, "strength" = strength1)


############ TRUE NETWORK WITHOUT OPTIMISM ############

#Explore network's centrality
true1 = as.igraph(true1_wo)

V(true1)$name <- xvars1[1:4]
E(true1)$weight <- abs(E(true1)$weight)

#Expected Influence
influence1 <- expectedInf(true1, step = c("both", 1, 2), 
                          directed = T)
influence1 <- influence1[2]
influence1 <- influence1$step2
influence1 <- influence1 %>% sort(decreasing = TRUE)  #Optimism is the highest in ExpInf

#Strength
strength1 <- igraph::strength(true1, mode = "all", loops = FALSE,
                              vids = V(true1))
strength1 <- sort(strength1, decreasing = TRUE) #Optimism is the highest in strength
#However, optimism is mostly connected to the 2nd and 3rd strongest nodes..

results_true_1wo <- list("influence" = influence1, "strength" = strength1)


#################### BOOTSTRAPPING ##################
############ SIMULATED NETWORK WITH OPTIMISM ############

load("/Users/AlexisDanielsStein/Desktop/Sophie/Data Analysis/Data/Training/layout.RData")

############ SIMULATED NETWORK WITHOUT OPTIMISM ############


############ SIMULATED NETWORK WITH LOW OPTIMISM ############
nBoots = 3

beta_low_1 <- beta1
beta_low_1[,5] <- beta_low_1[,5] * 0.5
beta_low_1[5,1:4] <- beta_low_1[5,1:4] * 0.5
beta_low_1 == beta1

f <- function(i){ # specify the desired function and parameter values here
  bootstrappin(n_sim = 1500, beta = beta_low_1, kappa = kappa1, vars1 = xvars1, vars2 = xvars1,
               Layout = Layout)
}

res_low_1w <- lapply(X = 1:nBoots, f)

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

strength_list_1w <- get(data = res_low_1w, type = "strength", vars = xvars1)
influence_list_1w <- get(data = res_low_1w, type = "influence", vars = xvars1)


############ SIMULATED NETWORK WITHOUT LOW OPTIMISM ############

f <- function(i){ # specify the desired function and parameter values here
  bootstrappin(n_sim = 1500, beta = beta_low_1, kappa = kappa1, vars1 = xvars1, 
               vars2 = xvars1[1:4], Layout = Layout[1:4,])
}

res_low1_wo <- lapply(X = 1:nBoots, f)

## Clean up the results 
strength_list_low_1wo <- get(data = res_low_1wo, type = "strength", vars = xvars1[1:4])
influence_list_low_1wo <- get(data = res_low_1wo, type = "influence", vars = xvars1[1:4])




############ SIMULATED NETWORK WITH HIGH OPTIMISM ############

beta_high_1 <- beta1
beta_high_1[,5] <- beta_high_1[,5] * 1.5
beta_high_1[5,1:4] <- beta_high_1[5,1:4] * 1.5
beta_high_1 == beta1

f <- function(i){ # specify the desired function and parameter values here
  bootstrappin(n_sim = 1500, beta = beta_high_1, kappa = kappa1, vars1 = xvars1, vars2 = xvars1,
               Layout = Layout)
}

res_high_1w <- lapply(X = 1:nBoots, f)

## Clean up the results 
strength_list_high_1w <- get(data = res_high_1w, type = "strength", vars = xvars1)
influence_list_high_1w <- get(data = res_high_1w, type = "influence", vars = xvars1)

############ SIMULATED NETWORK WITHOUT HIGH OPTIMISM ############

f <- function(i){ # specify the desired function and parameter values here
  bootstrappin(n_sim = 1500, beta = beta_high_1, kappa = kappa1, vars1 = xvars1, 
               vars2 = xvars1[1:4], Layout = Layout[1:4,])
}

res_high_1wo <- lapply(X = 1:nBoots, f)

## Clean up the results 
strength_list_high_1wo <- get(data = res_high_1wo, type = "strength", vars = xvars1[1:4])
influence_list_high_1wo <- get(data = res_high_1wo, type = "influence", vars = xvars1[1:4])

