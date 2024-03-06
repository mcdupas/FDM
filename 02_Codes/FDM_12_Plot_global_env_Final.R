print("# -------------------------------------------------------------------")
print("#                   Plot global rank envelope                          ")
print("# -------------------------------------------------------------------")
flush.console()

# this code plot the envelope that are calculated in the code "FDM_8_SaveEnv.R"

library(sf)
library(maptools)
library(raster)
library(spatstat)
library(RandomFieldsUtils)
library(RandomFields)
library(doParallel)
library(GET)
library(ggplot2)
library(spatstat.core)


setwd("C:/Users/MC/Dropbox/OneHealthPoultry")

