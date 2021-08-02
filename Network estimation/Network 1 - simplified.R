#### GVAR ONLY WITH DELETIONS####
#NETWORK 1 - optimism -> down-suspic-lonely-anxious


####  NETWORK 1 - 2 CONDITIONS) ####
library("ggplot2")
library("qgraph")
library("igraph")
library("graphicalVAR")
library("psychonetrics")
library("tidyverse")
library("matrixStats")
library("lcsm")
library("growthcurver")
library("NetworkComparisonTest")
library("bootnet")
library("networktools")
library("plyr")
library("parallel")

par(mfrow = c(4, 2))

pg_full <- read.csv('/Users/AlexisDanielsStein/Desktop/ResMasPsychology/Thesis/Data Analysis/My Sims/Fast Nets & Slow Var/Centrality/Objects/ESMdata.csv', sep = ",")

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
# View(mood_optimism)
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


length(unique(pg_full$dayno))
head(mood_optimism)
nrow(pg_full)
# View(mood_optimism)

colMeans(mood_optimism, na.rm = TRUE) #Optimism is the highest mean

#Assumption checks: Multivariate normality and variance 
# assumptionCheck(mood_optimism[,c(4,7,8,10,15)], type = "network", plot = TRUE)

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
  if (i == nrow(mood_optimism)-1) {
    stop()
  }
}

# View(mood_optimism)
#Disregard the error: it states that the last iteration couldn't be completed



########### TRUE NETWORK ##########


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
  'anxi',
  'suspic',
  'optim')

load("/Users/AlexisDanielsStein/Desktop/Sophie/Data Analysis/Data/Training/layout.RData")
Layout <- layout

estimateNet1 <- function(data = mood_optimism, vars1, plot_title = "Network", 
                        Layout = Layout[1:5,], Labels = labels_short1) {
  
#Estimate model with FIML to account for missing data
  if (any(grepl(pattern = "day", x= colnames(data)))) {
    mod1 <<- gvar(data,  vars = vars1, beepvar = xbeepvar, dayvar = xdayvar, estimator = "FIML",
             standardize = "z")
  } else {
    mod1 <<- gvar(data,  vars = vars1, estimator = "FIML", standardize = "z")
  }
  
# Use different optimizer since default ("nlminb") gives error (code from Alexis)
mod1 <<- setoptimizer(mod1, optimizer = "nlminb") # note Alexis: #This optimizer is dependent on OS

# Run model and modelsearch (this takes a while):
# mod <- mod %>% prune %>% modelsearch # Error not positive semi-definite... different optimizer with setoptimizer(...)
mod1 <<- mod1 %>% runmodel

#Let's look at the true network (Does it look like I expect it?) *Important step
#Select only the fast_variables? ####

temporal1 <<- getmatrix(mod1, "PDC")
beta1 <<- getmatrix(mod1, "beta")
contemporaneous1 <<- getmatrix(mod1, "omega_zeta")
kappa1 <<- contemporaneous1


true_net <<- qgraph(temporal1, layout = Layout, theme = "colorblind", 
                 directed = TRUE, diag = F,
               title = plot_title, labels = Labels,   
               vsize = 20, vsize2 = 20, asize = 6, mar = rep(6,4),
               label.scale = TRUE, label.fill.horizontal = 2, label.scale.equal = TRUE)

# return(plot(true1))
}

par(mfrow = c(2, 1))


#Estimate true network with optimism
estimateNet1(data = mood_optimism, vars1 = xvars1, plot_title = "True Network 1 w Optimism",
            Layout = Layout[1:5,], Labels = labels_short1)
beta <- beta1
kappa <- kappa1
true1 <- true_net

#Explore network's centrality
true1 = as.igraph(true1)

V(true1)$name <- xvars1
E(true1)$weight <- abs(E(true1)$weight)
strength1 <- igraph::strength(true1, mode = "all", loops = FALSE,
                              vids = V(true1))
strength1 <- sort(strength1, decreasing = TRUE)

#Estimate true network without optimism
estimateNet1(data = mood_optimism, vars1 = xvars1[1:4], plot_title = "True Network 1 w/o Optimism",
            Layout = Layout[1:4,], Labels = labels_short1[1:4])
beta1 <- beta
kappa1 <- kappa
true1_wo <- true_net
class(true1_wo)

#Simulate data from true network
#Estimate network with optimism
sim1 <- graphicalVARsim_MultiTime(15000, beta1, kappa1, var_slow = 1, time_slow = 1)
head(sim1)
colnames(sim1) <- xvars1

# sim1[vector, 5] <- NA
head(sim1)

#Estimate network of simulated data with optimism
estimateNet1(data = sim1, vars1 = xvars1, plot_title = "Sim from True Network 1 w Optimism",
            Layout = Layout[1:5,], Labels = labels_short1)
beta1 <- beta
kappa1 <- kappa
sim1_plot1 <- true_net

#Explore network's centrality
sim1_plot1 = as.igraph(sim1_plot1)

V(sim1_plot1)$name <- xvars1
E(sim1_plot1)$weight <- abs(E(sim1_plot1)$weight)
strength1 <- igraph::strength(sim1_plot1, mode = "all", loops = FALSE,
                              vids = V(sim1_plot1))
strength1 <- sort(strength1, decreasing = TRUE)


#Estimate network of simulated data without optimism
estimateNet1(data = sim1, vars1 = xvars1[1:4], plot_title = "Sim from True Network 1 w/o Optimism",
            Layout = Layout[1:4,], Labels = labels_short1[1:4])
beta1 <- beta
kappa1 <- kappa
sim1_plot1_wo <- true_net








############## CONDITION 1 - LOW OPTIMISM ###########

#Establish lower connection in/out optimism
beta_low_1 <- beta1
beta_low_1[,5] <- beta_low_1[,5] * 0.5
beta_low_1[5,1:4] <- beta_low_1[5,1:4] * 0.5
beta_low_1 == beta1

# Simulate data with modifying 'mean' argument
low_sim_gvar_1 <- graphicalVARsim_MultiTime(15000, beta_low_1, kappa1, mean = c(0,0,0,0,0),
                                       var_slow = 5, time_slow = c(5))  #MultiTime simulation

#Delete "repeated measurements"
low_sim_gvar_1[vector, 5] <- NA

# View(low_sim_gvar_1)
colnames(low_sim_gvar_1) <- xvars1
head(low_sim_gvar_1)

#Estimate network WITH optimism
estimateNet1(data = low_sim_gvar_1, vars1 = xvars1, plot_title = "Sim from modified Network 1 w low Optimism",
            Layout = Layout[1:5,], Labels = labels_short1)
beta1 <- beta
kappa1 <- kappa
temp_low_sim_gvar_1_w <- true_net

#Estimate network WITHOUT optimism
estimateNet1(data = low_sim_gvar_1, vars1 = xvars1[1:4], plot_title = "Sim from modified Network 1 w/o low Optimism",
            Layout = Layout[1:4,], Labels = labels_short1[1:4])
beta1 <- beta
kappa1 <- kappa
temp_low_sim_gvar_1_wo <- true_net







############## CONDITION 2 - HIGH OPTIMISM ###########

#Example with slow variable set as constantly low

beta_high_1 <- beta1
beta_high_1[,5] <- beta_high_1[,5] * 1.5
beta_high_1[5,1:4] <- beta_high_1[5,1:4] * 1.5
beta_high_1 == beta1

#Simulate data with new beta matrix
high_sim_gvar_1 <- graphicalVARsim_MultiTime(15000, beta_high_1, kappa1, mean = c(0,0,0,0,0),
                                        var_slow = 5, time_slow = c(5))  #MultiTime simulation
colnames(high_sim_gvar_1) <- xvars1
#View(high_sim_gvar_1)
high_sim_gvar_1[vector, 5] <- NA

#Estimate network WITH optimism
estimateNet1(data = high_sim_gvar_1, vars1 = xvars1, plot_title = "Sim from modified Network 1 w high Optimism",
            Layout = Layout[1:5,], Labels = labels_short1)
beta1 <- beta
kappa1 <- kappa
temp_high_sim_gvar_1_w <- true_net


#Estimate network WITHOUT optimism
estimateNet1(data = high_sim_gvar_1, vars1 = xvars1[1:4], plot_title = "Sim from modified Network 1 w/o high Optimism",
            Layout = Layout[1:4,], Labels = labels_short1[1:4])
beta1 <- beta
kappa1 <- kappa
temp_high_sim_gvar_1_wo <- true_net



#Collect to pdf
pdf(file = "/Users/AlexisDanielsStein/Desktop/ResMasPsychology/Thesis/Network 1 plots.pdf")
par(mfrow = c(1, 3))
plot(true1)
plot(true1_wo)
plot(sim1_plot1)
plot(sim1_plot1_wo)
plot(temp_low_sim_gvar_1_w)
plot(temp_low_sim_gvar_1_wo)
plot(temp_high_sim_gvar_1_w)
plot(temp_high_sim_gvar_1_wo)
dev.off()

pdf(file = "/Users/AlexisDanielsStein/Desktop/ResMasPsychology/Thesis/Final Paper/Figures/True 1.pdf")
par(mfrow = c(1, 1))
plot(true1)
dev.off()

pdf(file = "/Users/AlexisDanielsStein/Desktop/ResMasPsychology/Thesis/Final Paper/Figures/True 2.pdf")
par(mfrow = c(1, 1))
plot(true2)
dev.off()

pdf(file = "/Users/AlexisDanielsStein/Desktop/ResMasPsychology/Thesis/Final Paper/Figures/True 3.pdf")
par(mfrow = c(1, 1))
plot(true3)
dev.off()

pdf(file = "/Users/AlexisDanielsStein/Desktop/ResMasPsychology/Thesis/Final Paper/Figures/True ALl.pdf")
par(mfrow = c(1, 3))
plot(true1)
plot(true2)
plot(true3)
dev.off()
