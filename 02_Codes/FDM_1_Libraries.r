
##########################################################
# Libraries
##########################################################

print("# -------------------------------------------------------------------")
print("#                   Loading required packages")
print("# -------------------------------------------------------------------")
flush.console()

library(sf)
library(spatstat)
library(exactextractr)
library(ranger)
library(gdalUtils)
library(doParallel)
library(ggplot2)
library(foreach)
library(maps)
library(mapproj)
library(terra)