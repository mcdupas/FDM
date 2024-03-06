# Libraries
library(raster)
library(rgdal)
#library(dplyr)
#library(mapview)
#library(mapedit)
library(sf)
#library(readr)
library(ggplot2)

### relative directories
mypath <- getwd()            ## current folder
dir    <- dirname(mypath)    ## parent 1
dir2   <- dirname(dir)       ## parent 2
dir3   <- dirname(dir2)      ## parent 3 

## parameters to change
P_OutName = "India"
P_AdmCode_pred = "IND" #India
# Absolute path of FDM
P_GenPath = paste(dir2,"/", sep="")
# Model folder
P_ModelPath = paste(P_GenPath,"02_Models/",sep="")
# General folder of the run
P_OutData = paste(P_ModelPath,P_OutName,"/",sep="")

P_ProcessingFolder = paste(P_OutData,"2_Processing",sep="")
P_PredictorFolder = paste(P_OutData,"1_Predictors",sep="")

## read shape or tif files 
pShape = paste(P_ProcessingFolder,"/","FarmData_sim.shp", sep="")                   ## simulated farm
pHpop  = paste(P_PredictorFolder,"/Hpop.tif",sep="")    ## covariate Hpop

FarmData_sim = st_read(pShape, stringsAsFactors = FALSE)
Hpop_raster  = raster(pHpop)


## plot map
plot(Hpop_raster,
     main = "Hpop")

## plot histogram
hist(Hpop_raster,
     main = "Distribution of population density",
     xlab = "Hpop", ylab = "Frequency",
     col = "springgreen")