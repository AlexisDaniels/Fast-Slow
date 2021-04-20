####  NETWORK 1 - 3 CONDITIONS) ####
library("ggplot2")
library("qgraph")
library("graphicalVAR")
library("psychonetrics")
library("tidyverse")
library("matrixStats")
library("lcsm")
library("growthcurver")
library("NetworkComparisonTest")
library("bootnet")


pg_full <- read.csv('ESMdata.csv', sep = ",")
xvars <- c(
  'mood_relaxed',
  'mood_down',
  'mood_irritat',
  'mood_satisfi',
  'mood_lonely',
  'mood_anxious',
  'mood_enthus',
  'mood_suspic',
  'mood_cheerf',
  'mood_guilty',
  'mood_doubt',
  'mood_strong',
  'mor_feellike')  
mood_optimism <- pg_full[,c(5,6,11:22,71)]

xbeepvar <- 'beepno'
xdayvar <- 'dayno'
labels_short <- c(
  'relaxed',
  'down',
  'irritat',
  'satisfi',
  'lonely',
  'anxious',
  'enthus',
  'suspic',
  'cheerf',
  'guilty',
  'doubt',
  'strong',
  'optimism'
)

colnames(is.na(mood_optimism))
mood_optimism <- na.omit(mood_optimism) 
head(mood_optimism)

colMeans(mood_optimism, na.rm = TRUE) #Optimism was the highest mean --> What to do here?

#Delete repeated measurements
i <- 1
for (i in 1:nrow(mood_optimism)) {
  if(mood_optimism[i, colnames(mood_optimism) == "dayno"] == 
     mood_optimism[(i+1), colnames(mood_optimism) == "dayno"]) {
    mood_optimism[(i+1), c(15)] <- NA
  }
  else {
    mood_optimism[(i+1), c(15)] <- mood_optimism[(i+1), c(15)]
  }
  if (i == 889) {
    stop()
  }
}

#Disregard the error: it states that the last iteration couldn't be completed

View(mood_optimism)

#### CONDITION 1 ####
#### NETWORK 1 WITH DIFFERENT SLOW_VAR CONDITION ####

# NETWORK 1: Negative connectivity (optimism -> down-suspic-lonely-anxious)
# NETWORK 2: Positive connectivity (optimism -> satisfied-enthusiasm-cheerful-strong)
# NETWORK 3: Mild connectivity (optimism to doubt-irritable-guilty- relaxed)


#NETWORK 1 - Negative connectivity (optimism -> down-suspic-lonely-anxious)
#### TRUE NETWORK


#Form model
xvars1 <- c(
  'mood_down',
  'mood_lonely',
  'mood_anxious',
  'mood_suspic',
  'mor_feellike')

labels_short1 <- c(
  'down',
  'lonely',
  'anxious',
  'suspic',
  'optimism')

#Estimate model with FIML to account for missing data
mod1 <- gvar(mood_optimism,  vars = xvars1, beepvar = xbeepvar, dayvar = xdayvar, estimator = "FIML",
             standardize = "z")
# Use different optimizer since default ("nlminb") gives error (code from Alexis)
mod1 <- setoptimizer(mod1, optimizer = "nlminb") # note Alexis: #This optimizer is dependent on OS

# Run model and modelsearch (this takes a while):
# mod <- mod %>% prune %>% modelsearch # Error not positive semi-definite... different optimizer with setoptimizer(...)
mod1 <- mod1 %>% runmodel
#Let's look at the true network (Does it look like I expect it?) *Important step
#Select only the fast_variables? ####
layout(t(1:1))

temporal1 <- getmatrix(mod1, "PDC")
beta1 <- getmatrix(mod1, "beta")
contemporaneous1 <- getmatrix(mod1, "omega_zeta")
kappa1 <- contemporaneous1

true <- qgraph(temporal1, layout = "spring", theme = "colorblind", 
               directed = TRUE, diag = TRUE, label.cex = 2, 
               title = "True Network 1", labels = labels_short1, 
               vsize = 12, asize = 5, mar = rep(6,4))

#Estimate the true network without optimism
#mod2 <- gvar(mood_optimism,  vars = xvars1[1:4], beepvar = xbeepvar, dayvar = xdayvar, estimator = "FIML",
#             standardize = "z")
#mod2 <- setoptimizer(mod2, optimizer = "nlminb") # note Alexis: #This optimizer is dependent on OS
#mod2 <- mod2 %>% runmodel#

#temporal2 <- getmatrix(mod2, "PDC")
#beta2 <- getmatrix(mod2, "beta")
#contemporaneous2 <- getmatrix(mod2, "omega_zeta")
#kappa2 <- contemporaneous2
#true2 <- qgraph(temporal2, layout = true$layout[1:4,], theme = "colorblind", 
#                directed = TRUE, diag = TRUE, label.cex = 2, 
#                title = "True Network 2", labels = labels_short1[1:4], 
#                vsize = 12, asize = 5, mar = rep(6,4))

#Simulate data from true network
sim <- graphicalVARsim_MultiTime(1500, beta1, kappa1, var_slow = 5, time_slow = 5)
head(sim)
colnames(sim) <- xvars1
net_sim <- graphicalVAR(sim[,c(1:4)], vars = xvars1[1:4],  
                        #The hypothethical overlooking of the slow_var is at this stage
                        lambda_beta = 0.05, lambda_kappa = 0.05)
temp_net_sim <- net_sim$PDC
cont_net_sim <- net_sim$PCC

sim_plot <- qgraph(temp_net_sim[1:4,], layout = true$layout[1:4,], theme = "colorblind", 
                   directed = TRUE, diag = TRUE, label.cex = 2, 
                   title = "Simulated Network from True Network 1", labels = labels_short1[1:4], 
                   vsize = 12, asize = 5, mar = rep(6,4))


#NETWORK 1 - Negative connectivity (optimism -> down-suspic-lonely-anxious)
#### CONDITION 1 - Slowly increasing optimism ####


#Extract networks and set slow_var to increasing value  ####
layout(t(1:1))

temporal1 <- getmatrix(mod1, "PDC")
beta1 <- getmatrix(mod1, "beta")
contemporaneous1 <- getmatrix(mod1, "omega_zeta")
kappa1 <- contemporaneous1


#Example with slow variable increasing
beta_compare1 <- beta1
beta_increase1 <- beta1
parameter1 <- 0.2   #Found arbitrarily
increase_sim_1 <- matrix(0,1,5)

i <- 1
for (i in 1:28) {
  parameter1 <- parameter1 + 0.055  #Found arbitrarily 
  # maybe I can try to make it behave in a way that increases --> decreases  NOT DONE YET
  
  #for (j in 1:nrow(beta1)) {
  #    beta_increase1[j,c(5)] <- (beta_increase1[j,c(5)] + (-0.01))   #ALWAYS CHECK THE SIM DATA
  #    beta_increase1[c(5),j] <- (beta_increase1[c(5),j] + (-0.01))
  #  }
  sim_1 <- graphicalVARsim(50, beta_increase1, kappa1, warmup = 100) 
  w_mean <- c(0, 0, 0, 0, parameter1) #Here, only optimism values increase
  sim_2 <- graphicalVARsim(50, beta_increase1, kappa1, warmup = 100, mean = w_mean) 
  #Check ubound and lbound
  #Check what happens when I use Sara's function.. Check if sim-net is similar to true_net (?)
  
  increase_sim_1 <- rbind(increase_sim_1, sim_2)
}
#Apparently, it is enough to do it with the means only

#View(increase_sim_1)
colnames(increase_sim_1) <- xvars1
colMeans(increase_sim_1)
colSds(increase_sim_1)

head(increase_sim_1)
tail(increase_sim_1)
increase_sim_1 <- increase_sim_1[-1,]
#View(increase_sim_1)
nrow(increase_sim_1)   #1500 

x <- 1:nrow(increase_sim_1)
increase_sim_1 <- cbind(x, increase_sim_1)


#Plot trajectory for optimism
increase_sim_1 <- data.frame(increase_sim_1)
ggplot(increase_sim_1, aes(x = x, y = mor_feellike)) + geom_point(alpha=0.7)
optimism_trajectory <- SummarizeGrowth(increase_sim_1$x, increase_sim_1$mor_feellike)
plot(optimism_trajectory)

#Plot trajectory for other variables to compare
ggplot(increase_sim_1, aes(x = x, y = mood_down)) + geom_point(alpha=0.7)
down_trajectory <- SummarizeGrowth(increase_sim_1$x, increase_sim_1$mood_down)
plot(down_trajectory)
#Optimism is increasing over time & mood_down is staying constant.. This is great! (when using means)


beta_increase1 == beta_compare1   #check if for_loop worked

net_increase_sim_1 <- graphicalVAR(increase_sim_1[,2:5], vars = xvars1[1:4],
                                   #Split the data and see optimism as it increases
                                   lambda_beta = 0.05, lambda_kappa = 0.05) 
#Sacha said: don't worry for "Model did not converge in inner loop"

layout(t(1:2))

temp_increase_sim_1 <- net_increase_sim_1$PDC
cont_increase_sim_1 <- net_increase_sim_1$PCC

#Select only fast variables? ####
#First part of the 
temp_increase_sim_net_1 <- qgraph(temp_increase_sim_1, layout = true$layout[1:4,], theme = "colorblind", 
                                  directed = TRUE, diag = TRUE, label.cex = 2, 
                                  title = "Temporal Network 1 (Increasing slow_var): First", labels = labels_short1[1:4], 
                                  vsize = 12, asize = 5, mar = rep(6,4))






#NETWORK 1 - Negative connectivity (optimism -> down-suspic-lonely-anxious)
#### CONDITION 2 - Constantly low optimism ####


# Example with slow variable set as constantly low
temporal1 <- getmatrix(mod1, "PDC")
beta1 <- getmatrix(mod1, "beta")
contemporaneous1 <- getmatrix(mod1, "omega_zeta")
kappa1 <- contemporaneous1

beta_low_1 <- beta1
beta_low_1[,5] <- beta_low_1[,5] * 0.5
beta_low_1[5,1:4] <- beta_low_1[5,1:4] * 0.5
beta_low_1 == beta1

# Simulate data with modifying 'mean' argument
#low_sim_1 <- graphicalVARsim(15000, beta1, kappa1, mean = c(0,0,0,0,-1.5)) 
low_sim_1 <- graphicalVARsim_MultiTime(1500, beta_low_1, kappa1, mean = c(0,0,0,0,0),
                                       var_slow = 5, time_slow = c(5))  #MultiTime simulation


#View(low_sim_1)
colnames(low_sim_1) <- xvars1
# Set optimism mean to low 
# Explore simulated data

tail(low_sim_1)
colMeans(low_sim_1, na.rm = TRUE)
colVars(low_sim_1, na.rm = TRUE)

#1 - Calculate network WITH optimism
net_low_sim_1_w <- graphicalVAR(low_sim_1, vars = xvars1, 
                                lambda_beta = 0.05, lambda_kappa = 0.05) 
#Sacha said: don't worry for "Model did not converge in inner loop"

layout(t(1:2))

temp_low_sim_1_w<- net_low_sim_1_w$PDC
cont_low_sim_1_w <- net_low_sim_1_w$PCC

#Compare (true) simulated network w/optimism to the simulated network w/o optimism

temp_low_sim_net_1_w <- qgraph(temp_low_sim_1_w, layout = true$layout, theme = "colorblind", 
                               directed = TRUE, diag = TRUE, label.cex = 2, labels = labels_short1, 
                               title = "True Network 2 (Low slow_var)",
                               vsize = 12, asize = 5, mar = rep(6,4))


#2 - Calculate network WITHOUT optimism
net_low_sim_1_wo <- graphicalVAR(low_sim_1[,1:4], vars = xvars1[1:4], 
                                 lambda_beta = 0.05, lambda_kappa = 0.05) 
#Sacha said: don't worry for "Model did not converge in inner loop"


temp_low_sim_1_wo <- net_low_sim_1_wo$PDC
cont_low_sim_1_wo <- net_low_sim_1_wo$PCC

#Compare simulated network to the true network

temp_low_sim_net_1_wo <- qgraph(temp_low_sim_1_wo, layout = true$layout[1:4,], theme = "colorblind", 
                                directed = TRUE, diag = TRUE, label.cex = 2, labels = labels_short1[1:4], 
                                title = "Simulated Network 2 (Low slow_var)",
                                vsize = 12, asize = 5, mar = rep(6,4))

#NETWORK 1 - Negative connectivity (optimism -> down-suspic-lonely-anxious)
#### CONDITION 3 - Constantly high optimism ####


#Example with slow variable set as constantly low
temporal1 <- getmatrix(mod1, "PDC")
beta1 <- getmatrix(mod1, "beta")
contemporaneous1 <- getmatrix(mod1, "omega_zeta")
kappa1 <- contemporaneous1

beta_high_1 <- beta1
beta_high_1[,5] <- beta_high_1[,5] * 1.5
beta_high_1[5,1:4] <- beta_high_1[5,1:4] * 1.5
beta_high_1 == beta1

#Simulate data with new beta matrix
#high_sim_1 <- graphicalVARsim(15000, beta1, kappa1, mean = c(0,0,0,0,10)) #Set optimism mean to high
high_sim_1 <- graphicalVARsim_MultiTime(1500, beta_high_1, kappa1, mean = c(0,0,0,0,0),
                                        var_slow = 5, time_slow = c(5))  #MultiTime simulation
colnames(high_sim_1) <- xvars1

colMeans(high_sim_1)
#Calculate network WITH optimism
net_high_sim_1_w <- graphicalVAR(high_sim_1, vars = xvars1, 
                                 lambda_beta = 0.05, lambda_kappa = 0.05) 
#Sacha said: don't worry for "Model did not converge in inner loop"

layout(t(1:2))
temp_high_sim_1_w <- net_high_sim_1_w$PDC
cont_high_sim_1_w <- net_high_sim_1_w$PCC

#Compare simulated network to the true network
temp_high_sim_net_1_w <- qgraph(temp_high_sim_1_w, layout = true$layout, theme = "colorblind", 
                                directed = TRUE, diag = TRUE, label.cex = 2, labels = labels_short1, 
                                title = "True Network 3 (High slow_var)",
                                vsize = 12, asize = 5, mar = rep(6,4))

#2- Calculate network WITHOUT optimism
net_high_sim_1_wo <- graphicalVAR(high_sim_1, vars = xvars1[1:4], 
                                  lambda_beta = 0.05, lambda_kappa = 0.05) 
#Sacha said: don't worry for "Model did not converge in inner loop"

temp_high_sim_1_wo <- net_high_sim_1_wo$PDC
cont_high_sim_1_wo <- net_high_sim_1_wo$PCC

#Compare simulated network to the true network
temp_high_sim_net_1_wo <- qgraph(temp_high_sim_1_wo, layout = true$layout[1:4,], theme = "colorblind", 
                                 directed = TRUE, diag = TRUE, label.cex = 2, labels = labels_short1[1:4], 
                                 title = "Simulated Network 3 (High slow_var)",
                                 vsize = 12, asize = 5, mar = rep(6,4))

#Compare the true network and the three simulated networks
par(mfrow = c(3, 2))
plot(true)
plot(sim_plot)
#plot(true2)
plot(temp_low_sim_net_1_w)
plot(temp_low_sim_net_1_wo)
plot(temp_high_sim_net_1_w)
plot(temp_high_sim_net_1_wo)









#### CALCULATE CENTRALITY ####
#CONDITION 2 - LOW OPTIMISM
one_w <- as.igraph(temp_low_sim_net_1_w)
one_wo <- as.igraph(temp_low_sim_net_1_wo)

edgelist_w <- as.data.frame(temp_low_sim_net_1_w$Edgelist[1:3])
zz_w <- IgraphFromEdgelist(edgelist_w, directed = TRUE)
w <- unlist(edgelist_w[3])
w <- abs(w)

edgelist_wo <- as.data.frame(temp_low_sim_net_1_wo$Edgelist[1:3])
zz_wo <- IgraphFromEdgelist(edgelist_wo, directed = TRUE)
wo <- unlist(edgelist_wo[3])
wo <- abs(wo)

cbind(rbind(edgelist_wo, 0, 0, 0, 0), edgelist_w)

one_w <- set.edge.attribute(zz_w, "weight", value = w)
one_wo <- set.edge.attribute(zz_wo, "weight", value = wo)

closeness(one_w, mode = "all")
closeness(one_wo, mode = "all")

betweenness(one_w, directed = TRUE)
betweenness(one_wo, directed = TRUE)

degree(one_w, mode = "all")
degree(one_wo, mode = "all")



#CONDITION 2 - LOW OPTIMISM
two_w <- as.igraph(temp_high_sim_net_1_w)
two_wo <- as.igraph(temp_high_sim_net_1_wo)

edgelist_w <- as.data.frame(temp_high_sim_net_1_w$Edgelist[1:3])
zz_w <- IgraphFromEdgelist(edgelist_w, directed = TRUE)
w <- unlist(edgelist_w[3])
w <- abs(w)

edgelist_wo <- as.data.frame(temp_high_sim_net_1_wo$Edgelist[1:3])
zz_wo <- IgraphFromEdgelist(edgelist_wo, directed = TRUE)
wo <- unlist(edgelist_wo[3])
wo <- abs(wo)

cbind(rbind(edgelist_wo, 0, 0, 0, 0), edgelist_w)

two_w <- set.edge.attribute(zz_w, "weight", value = w)
two_wo <- set.edge.attribute(zz_wo, "weight", value = wo)

closeness(two_w, mode = "all")
closeness(two_wo, mode = "all")

betweenness(two_w, directed = TRUE)
betweenness(two_wo, directed = TRUE)

degree(two_w, mode = "all")
degree(two_wo, mode = "all")

