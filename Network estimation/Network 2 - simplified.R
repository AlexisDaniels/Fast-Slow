#### GVAR ONLY WITH DELETIONS####
#NETWORK 2 - optimism -> doubt-enthusiastic-guilty-lonely

####  NETWORK 2 - 2 CONDITIONS) ####
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

pg_full <- read.csv('/Users/AlexisDanielsStein/Desktop/ResMasPsychology/Thesis/Data Analysis/Peter Groot - Claudia/My Sims/Fast Nets & Slow Var/ESMdata.csv', sep = ",")

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
nrow(mood_optimism)
#View(mood_optimism)

colMeans(mood_optimism, na.rm = TRUE) #Optimism is the highest mean

#Assumption checks: Multivariate normality and variance 
assumptionCheck(mood_optimism[,c(4,7,8,10,15)], type = "network", plot = TRUE)

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
xvars2 <- c(
  'mood_doubt',
  'mood_enthus',
  'mood_guilty',
  'mood_lonely',
  'mor_feellike')

labels_short2 <- c(
  'doubt',
  'enthus',
  'guilty',
  'lonely',
  'optim')

estimateNet2 <- function(data = mood_optimism, vars1, plot_title = "Network", 
                        Layout = Layout[1:5,], Labels = labels_short2) {
  
#Estimate model with FIML to account for missing data
  if (any(grepl(pattern = "day", x = colnames(data)))) {
    mod2 <<- gvar(data,  vars = vars1, beepvar = xbeepvar, dayvar = xdayvar, estimator = "FIML",
             standardize = "z")
  } else {
    mod2 <<- gvar(data,  vars = vars1, estimator = "FIML", standardize = "z")
  }
  
# Use different optimizer since default ("nlminb") gives error (code from Alexis)
mod2 <<- setoptimizer(mod2, optimizer = "nlminb") # note Alexis: #This optimizer is dependent on OS

# Run model and modelsearch (this takes a while):
# mod <- mod %>% prune %>% modelsearch # Error not positive semi-definite... different optimizer with setoptimizer(...)
mod2 <<- mod2 %>% runmodel

#Let's look at the true network (Does it look like I expect it?) *Important step
#Select only the fast_variables? ####

temporal2 <<- getmatrix(mod2, "PDC")
beta2 <<- getmatrix(mod2, "beta")
contemporaneous2 <<- getmatrix(mod2, "omega_zeta")
kappa2 <<- contemporaneous2


true_net <<- qgraph(temporal2, layout = Layout, theme = "colorblind", 
                 directed = TRUE, diag = F, 
               title = plot_title, labels = Labels, 
               vsize = 20, vsize2 = 20, asize = 6, mar = rep(6,4),
               label.scale = TRUE, label.fill.horizontal = 2, label.scale.equal = TRUE)


# return(plot(true2))
}



#Estimate true network with optimism
estimateNet2(data = mood_optimism, vars1 = xvars2, plot_title = "True Network 2 w Optimism",
            Layout = Layout[1:5,], Labels = labels_short2)
beta <- beta2
kappa <- kappa2
true2 <- true_net

Layout <- true2$layout
layout <- Layout

#Estimate true network without optimism
estimateNet2(data = mood_optimism, vars1 = xvars2[1:4], plot_title = "True Network 2 w/o Optimism",
            Layout = Layout[1:4,], Labels = labels_short2[1:4])
beta2 <- beta
kappa2 <- kappa
true2_wo <- true_net


#Simulate data from true network
#Estimate network with optimism
sim2 <- graphicalVARsim_MultiTime(15000, beta2, kappa2, var_slow = 5, time_slow = 5)
head(sim2)
colnames(sim2) <- xvars2
nrow(sim2)

vector <- sort(c(seq(2,15000, 5), seq(3,15000, 5),
                 seq(4,15000, 5), seq(5,15000, 5)))

sim2[vector, 5] <- NA
head(sim2)

#Estimate network of simulated data with optimism
estimateNet2(data = sim2, vars1 = xvars2, plot_title = "Sim from True Network 2 w Optimism",
            Layout = Layout[1:5,], Labels = labels_short2)
beta2 <- beta
kappa2 <- kappa
sim2_plot1 <- true_net



#Estimate network of simulated data without optimism
estimateNet2(data = sim2, vars1 = xvars2[1:4], plot_title = "Sim from True Network 2 w/o Optimism",
            Layout = Layout[1:4,], Labels = labels_short2[1:4])
beta2 <- beta
kappa2 <- kappa
sim2_plot1_wo <- true_net








############## CONDITION 1 - LOW OPTIMISM ###########

#Establish lower connection in/out optimism
beta_low_2 <- beta2
beta_low_2[,5] <- beta_low_2[,5] * 0.5
beta_low_2[5,1:4] <- beta_low_2[5,1:4] * 0.5
beta_low_2 == beta2

# Simulate data with modifying 'mean' argument
low_sim_gvar_2 <- graphicalVARsim_MultiTime(15000, beta_low_2, kappa2, mean = c(0,0,0,0,0),
                                       var_slow = 5, time_slow = c(5))  #MultiTime simulation

#Delete "repeated measurements"
low_sim_gvar_2[vector, 5] <- NA

# View(low_sim_gvar_2)
colnames(low_sim_gvar_2) <- xvars2
head(low_sim_gvar_2)

#Estimate network WITH optimism
estimateNet2(data = low_sim_gvar_2, vars1 = xvars2, plot_title = "Sim from modified Network 2 w low Optimism",
            Layout = Layout[1:5,], Labels = labels_short2)
beta2 <- beta
kappa2 <- kappa
temp_low_sim_gvar_2_w <- true_net

#Estimate network WITHOUT optimism
estimateNet2(data = low_sim_gvar_2, vars1 = xvars2[1:4], plot_title = "Sim from modified Network 2 w/o low Optimism",
            Layout = Layout[1:4,], Labels = labels_short2[1:4])
beta2 <- beta
kappa2 <- kappa
temp_low_sim_gvar_2_wo <- true_net







############## CONDITION 2 - HIGH OPTIMISM ###########

#Example with slow variable set as constantly low

beta_high_2 <- beta2
beta_high_2[,5] <- beta_high_2[,5] * 1.5
beta_high_2[5,1:4] <- beta_high_2[5,1:4] * 1.5
beta_high_2 == beta2


#Simulate data with new beta matrix
high_sim_gvar_2 <- graphicalVARsim_MultiTime(15000, beta_high_2, kappa2, mean = c(0,0,0,0,0),
                                        var_slow = 5, time_slow = c(5))  #MultiTime simulation
colnames(high_sim_gvar_2) <- xvars2
#View(high_sim_gvar_2)
high_sim_gvar_2[vector, 5] <- NA

#Estimate network WITH optimism
estimateNet2(data = high_sim_gvar_2, vars1 = xvars2, plot_title = "Sim from modified Network 2 w high Optimism",
            Layout = Layout[1:5,], Labels = labels_short2)
beta2 <- beta
kappa2 <- kappa
temp_high_sim_gvar_2_w <- true_net


#Estimate network WITHOUT optimism
estimateNet2(data = high_sim_gvar_2, vars1 = xvars2[1:4], plot_title = "Sim from modified Network 2 w/o high Optimism",
            Layout = Layout[1:4,], Labels = labels_short2[1:4])
beta2 <- beta
kappa2 <- kappa
temp_high_sim_gvar_2_wo <- true_net


par(mfrow = c(4, 2))

plot(true2)
plot(true2_wo)
plot(sim2_plot1)
plot(sim2_plot1_wo)
plot(temp_low_sim_gvar_2_w)
plot(temp_low_sim_gvar_2_wo)
plot(temp_high_sim_gvar_2_w)
plot(temp_high_sim_gvar_2_wo)
