#### GVAR ONLY WITH DELETIONS####
#NETWORK 3 - optimism -> restless-agitated-worry-concentration

####  NETWORK 3 - 2 CONDITIONS) ####
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
data <- pg_full

pathology <- grep("pat", colnames(data))
pathology_optimism <- pg_full[,c(5,6,pathology,71)]
pathology_optimism <- na.omit(pathology_optimism) 

pathology_xvars <- colnames(pathology_optimism)[c(3:7)]

xbeepvar <- 'beepno'
xdayvar <- 'dayno'

colnames(is.na(pathology_optimism))
head(pathology_optimism)
nrow(pathology_optimism)

# View(pathology_optimism)

colMeans(pathology_optimism, na.rm = TRUE) #Optimism is the highest mean

#Assumption checks: Multivariate normality and variance 
assumptionCheck(pathology_optimism[,c(3:7)], type = "network", plot = TRUE)

#Delete repeated measurements
i <- 1
for (i in 1:nrow(pathology_optimism)) {
  if(pathology_optimism[i, colnames(pathology_optimism) == "dayno"] == 
     pathology_optimism[(i+1), colnames(pathology_optimism) == "dayno"]) {
    pathology_optimism[(i+1), c(7)] <- NA
  }
  else {
    pathology_optimism[(i+1), c(7)] <- pathology_optimism[(i+1), c(7)]
  }
  if (i == nrow(pathology_optimism)-1) {
    stop()
  }
}

# View(pathology_optimism)
#Disregard the error: it states that the last iteration couldn't be completed



########### TRUE NETWORK ##########


#Form model
xvars3 <- pathology_xvars

labels_short3 <- c(
  'rest',
  'agita',
  'worry',
  'concen',
  'optim')

estimateNet3 <- function(data = pathology_optimism, vars1, plot_title = "Network", 
                        Layout = Layout[1:5,], Labels = labels_short3) {
  
  tryCatch({
  
#Estimate model with FIML to account for missing data
  if (any(grepl(pattern = "day", x = colnames(data)))) {
    mod3 <<- gvar(data,  vars = vars1, beepvar = xbeepvar, dayvar = xdayvar, estimator = "FIML",
             standardize = "z")
  } else {
    mod3 <<- gvar(data,  vars = vars1, estimator = "FIML", standardize = "z")
  }
  
# Use different optimizer since default ("nlminb") gives error (code from Alexis)
mod3 <<- setoptimizer(mod3, optimizer = "nlminb") # note Alexis: #This optimizer is dependent on OS

# Run model and modelsearch (this takes a while):
# mod <- mod %>% prune %>% modelsearch # Error not positive semi-definite... different optimizer with setoptimizer(...)
mod3 <<- mod3 %>% runmodel

#Let's look at the true network (Does it look like I expect it?) *Important step
#Select only the fast_variables? ####

temporal3 <<- getmatrix(mod3, "PDC")
beta3 <<- getmatrix(mod3, "beta")
contemporaneous3 <<- getmatrix(mod3, "omega_zeta")
kappa3 <<- contemporaneous3


true_net <<- qgraph(temporal3, layout = Layout, theme = "colorblind", 
                 directed = TRUE, diag = F, 
               title = plot_title, labels = Labels, 
               vsize = 20, vsize2 = 20, asize = 6, mar = rep(6,4),
               label.scale = TRUE, label.fill.horizontal = 2)

  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
# return(plot(true3))
}



#Estimate true network with optimism
estimateNet3(data = pathology_optimism, vars1 = xvars3, plot_title = "True Network 3 w Optimism",
            Layout = Layout[1:5,], Labels = labels_short3)
beta <- beta3
kappa <- kappa3
true3 <- true_net

Layout <- true3$layout
layout <- Layout

#Estimate true network without optimism
estimateNet3(data = pathology_optimism, vars1 = xvars3[1:4], plot_title = "True Network 3 w/o Optimism",
            Layout = Layout[1:4,], Labels = labels_short3[1:4])
beta3 <- beta
kappa3 <- kappa
true3_wo <- true_net


#Simulate data from true network
#Estimate network with optimism
sim3 <- graphicalVARsim_MultiTime(15000, beta3, kappa3, var_slow = 5, time_slow = 5)
head(sim3)
colnames(sim3) <- xvars3
nrow(sim3)

vector <- sort(c(seq(2,15000, 5), seq(3,15000, 5),
                 seq(4,15000, 5), seq(5,15000, 5)))

sim3[vector, 5] <- NA
head(sim3)

#Estimate network of simulated data with optimism
estimateNet3(data = sim3, vars1 = xvars3, plot_title = "Sim from True Network 3 w Optimism",
            Layout = Layout[1:5,], Labels = labels_short3)
beta3 <- beta
kappa3 <- kappa
sim3_plot1 <- true_net



#Estimate network of simulated data without optimism
estimateNet3(data = sim3, vars1 = xvars3[1:4], plot_title = "Sim from True Network 3 w/o Optimism",
            Layout = Layout[1:4,], Labels = labels_short3[1:4])
beta3 <- beta
kappa3 <- kappa
sim3_plot1_wo <- true_net








############## CONDITION 1 - LOW OPTIMISM ###########

#Establish lower connection in/out optimism
beta_low_3 <- beta3
beta_low_3[,5] <- beta_low_3[,5] * 0.5
beta_low_3[5,1:4] <- beta_low_3[5,1:4] * 0.5
beta_low_3 == beta3

# Simulate data with modifying 'mean' argument
low_sim_gvar_3 <- graphicalVARsim_MultiTime(15000, beta_low_3, kappa3, mean = c(0,0,0,0,0),
                                       var_slow = 5, time_slow = c(5))  #MultiTime simulation

#Delete "repeated measurements"
low_sim_gvar_3[vector, 5] <- NA

# View(low_sim_gvar_3)
colnames(low_sim_gvar_3) <- xvars3
head(low_sim_gvar_3)

#Estimate network WITH optimism
estimateNet3(data = low_sim_gvar_3, vars1 = xvars3, plot_title = "Sim from modified Network 3 w low Optimism",
            Layout = Layout[1:5,], Labels = labels_short3)
beta3 <- beta
kappa3 <- kappa
temp_low_sim_gvar_3_w <- true_net

#Estimate network WITHOUT optimism
estimateNet3(data = low_sim_gvar_3, vars1 = xvars3[1:4], plot_title = "Sim from modified Network 3 w/o low Optimism",
            Layout = Layout[1:4,], Labels = labels_short3[1:4])
beta3 <- beta
kappa3 <- kappa
temp_low_sim_gvar_3_wo <- true_net







############## CONDITION 2 - HIGH OPTIMISM ###########

#Example with slow variable set as constantly low

beta_high_3 <- beta3
beta_high_3[,5] <- beta_high_3[,5] * 1.5
beta_high_3[5,1:4] <- beta_high_3[5,1:4] * 1.5
beta_high_3 == beta3


#Simulate data with new beta matrix
high_sim_gvar_3 <- graphicalVARsim_MultiTime(15000, beta_high_3, kappa3, mean = c(0,0,0,0,0),
                                        var_slow = 5, time_slow = c(5))  #MultiTime simulation
colnames(high_sim_gvar_3) <- xvars3
#View(high_sim_gvar_3)
high_sim_gvar_3[vector, 5] <- NA

#Estimate network WITH optimism
estimateNet3(data = high_sim_gvar_3, vars1 = xvars3, plot_title = "Sim from modified Network 3 w high Optimism",
            Layout = Layout[1:5,], Labels = labels_short3)
beta3 <- beta
kappa3 <- kappa
temp_high_sim_gvar_3_w <- true_net


#Estimate network WITHOUT optimism
estimateNet3(data = high_sim_gvar_3, vars1 = xvars3[1:4], plot_title = "Sim from modified Network 3 w/o high Optimism",
            Layout = Layout[1:4,], Labels = labels_short3[1:4])
beta3 <- beta
kappa3 <- kappa
temp_high_sim_gvar_3_wo <- true_net


par(mfrow = c(4, 2))

plot(true3)
plot(true3_wo)
plot(sim3_plot1)
plot(sim3_plot1_wo)
plot(temp_low_sim_gvar_3_w)
plot(temp_low_sim_gvar_3_wo)
plot(temp_high_sim_gvar_3_w)
plot(temp_high_sim_gvar_3_wo)
