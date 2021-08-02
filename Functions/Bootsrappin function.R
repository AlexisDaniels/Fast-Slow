#### Bootstrapping function ####
library("tsDyn")
library("doMC")
library("tidyverse")

par(mfrow = c(2,3))

load("/Users/AlexisDanielsStein/Desktop/Sophie/Data Analysis/Data/Training/layout.RData")

xvars2 <- xvars1

variables <- 5
xvars2 <- xvars1[1:variables]
Layout <- layout
Layout <- Layout[1:variables,]

#### BOOTSTRAPPING FUNCTION ####
bootstrappin <- function(n_sim = 1500, beta, kappa, vars1, vars2, Layout = Layout) {

  # results = foreach (i = 1:nBoots, .combine = combine, .packages = c("psychonetrics", "igraph",
  #                                                          "bootnet", "qgraph", "networktools",
  #                                                          "parallel", "doParallel")) %dopar% {
  # for (i in 1:nBoots) {
     tryCatch({
      sim <- graphicalVARsim_MultiTime(n_sim, beta, kappa, mean = c(0,0,0,0,0),
                                       var_slow = c(5), time_slow = c(5))
      vector <- sort(c(seq(2,n_sim, 5), seq(3,n_sim, 5),
                       seq(4,n_sim, 5), seq(5,n_sim, 5)))
      sim[vector, 5] <- NA
      
      colnames(sim) <- vars1
      
      mod <- gvar(sim, vars = vars2, estimator = "FIML", standardize = "z")
      mod <- setoptimizer(mod, optimizer = "nlminb") # note Alexis: #This optimizer is dependent on OS
      mod <- mod %>% runmodel
      
      temporal <- getmatrix(mod, "PDC")
      
      
      plot <- qgraph(temporal, layout = Layout, theme = "colorblind", 
                     directed = TRUE, diag = F, label.cex = 2, 
                     title = "Bootstrap", labels = vars2,
                     vsize = 12, asize = 5, mar = rep(6,4))
      
      plot_i = as.igraph(plot)
      
      V(plot_i)$name <- vars2
      E(plot_i)$weight <- abs(E(plot_i)$weight)
      
      #Strength
      strength <- igraph::strength(plot_i, mode = "all", loops = FALSE, vids = V(plot_i))
      nodes_strength <- sort(strength, decreasing = TRUE)
      names_strength <- matrix(names(nodes_strength),length(vars2),1)
      # strength_list <- cbind(strength_list, names_strength)

      #Influence
      influence <- expectedInf(plot_i, step = 1, directed = T)
      influence <- influence$step1
      influence <- influence %>% sort(decreasing = TRUE)
      names_influence <- matrix(names(influence),length(vars2),1)
      # influence_list <- cbind(influence_list, names_influence)
      
      results <- list("strength" = names_strength, "influence" = names_influence)
      

      return(results)
     }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  # }
  # stopCluster(cl)
}


#EXAMPLE
f <- function(i){ # specify the desired function and parameter values here
  bootstrappin(n_sim = 1500, beta = beta1, kappa = kappa1, vars1 = xvars1, vars2 = xvars2,
               Layout = Layout)
}


# number of cores 
nCores <- detectCores(logical = TRUE) # tells you the number of cores your computer can use for the simulations 

## Number of iterations
nBoots <- 3

# system.time({
#   res <- mclapply(X = 1:nBoots, f, mc.cores = 1)
# })

## Clean up the results 
get <- function(data, type, vars) {
  result <- matrix(data = NA, nrow = length(vars), ncol = nBoots)
  for(i in 1:nBoots) {
    if(type == "strength") {
      result[,i] <- data[[i]]$strength
    } else if(type == "influence") {
      result[,i] <- data[[i]]$influence
    }
  }
  return(result)
}

# strength_list <- get(data = res, type = "strength", vars = xvars1)
# influence_list <- get(data = res, type = "influence", vars = xvars1)
# 
# print(strength_list)
print(influence_list)