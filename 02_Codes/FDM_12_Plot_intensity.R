print("# -------------------------------------------------------------------")
print("#                   Plot intensity map                               ")
print("# -------------------------------------------------------------------")
flush.console()

library(sf)
library(maptools)
library(raster)
library(spatstat)
library(RandomFieldsUtils)
library(RandomFields)
library(doParallel)
library(GET)
library(ggplot2)



# This script is not yet part of the workflow, but contains code to plot graphs for evaluating model performance.

setwd("C:/Users/MC/Dropbox/OneHealthPoultry")

#Simulate patterns
P_ValNum = 40
P_Cores = 1
edge_corr = "none"
r_dist = seq(0,200000,500)

P_Model_Code = "Broiler_IN.GJ"
P_Sim_Code   = "Broiler_IN.GJ"
P_ModelFolder = paste("FDM/03_Validation/Train_model/",P_Model_Code,sep="")
P_CRS_Model = "+proj=utm +zone=45 +datum=WGS84 +units=m +no_defs"

### load simulations 
Sim_Path =  paste("FDM/03_Validation/Train_model/",P_Model_Code, "/Sim/Sim_8000_",P_Sim_Code,".ppp", sep="")
load(Sim_Path)

