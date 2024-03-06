###################################################################
## Data pre-processing of prediction rasters
###################################################################

# This script crops the covariate rasters and projects them to the set coordinate system.


### load libraries 
library(sf)
library(rgdal)
library(spatstat)
library(maptools)
library(raster)
library(exactextractr)
library(RandomFieldsUtils)
library(RandomFields)
library(ranger)
library(gdalUtils)



print("# -------------------------------------------------------------------")
print("#                   Pre-processing prediction rasters")
print("# -------------------------------------------------------------------")
flush.console()

## folders
P_PredictorFolder = "FDM/00_Predictors"

# CRS for geographic coordinates
P_CRS_Model = "+proj=utm +zone=45 +datum=WGS84 +units=m +no_defs"


### list of accessibility predictors 
P_PredNames = c("Inaccess_MC1","Inaccess_MC11","Inaccess_MC2","Inaccess_Port12","Inaccess_proxim_roads")
P_PredNames_New = c("Access_MC1","Access_MC11","Access_MC2","Access_Port12","Proxim_roads")

for (i in 1:length(P_PredNames )){
  print(P_PredNames[i])
  im.pred = raster(paste(P_PredictorFolder, "/", P_PredNames[i], ".tif", sep=""))
  values(im.pred) = 1/(values(im.pred)+1)
  writeRaster(im.pred, paste(P_PredictorFolder, "/", P_PredNames_New[i], ".tif", sep=""), overwrite = TRUE)
}
