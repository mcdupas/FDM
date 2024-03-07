# Spatial distribution of poultry farms using point pattern modelling: a method to address livestock environmental impacts and disease transmission risks

This repository is the official implementation of Spatial distribution of poultry farms using point pattern modelling: a method to address livestock environmental impacts and disease transmission risks  (add URL ). 


## Abstract

The distribution of farm locations and sizes is paramount to characterize patterns of disease spread. With some regions undergoing rapid intensification of livestock production, resulting in increased clustering of farms in peri-urban areas, measuring changes in the spatial distribution of farms is crucial to design effective interventions. However, those data are not available in many countries, their generation being resource-consuming. Here, we develop a farm distribution model (FDM), which allows the prediction of locations and sizes of poultry farms in countries with scarce data. The model combines (i) a Log-Gaussian Cox process model to simulate the farm distribution as a spatial Poisson point process, and (ii) a random forest model to simulate farm sizes (i.e. the number of animals per farm). Spatial predictors were used to calibrate the FDM on intensive broiler and layer farm distributions in Bangladesh, Gujarat (Indian state) and Thailand. We found that the FDM yielded realistic farm distributions in terms of spatial clustering, farm locations and sizes, while providing insights on the factors influencing these distributions. Finally, we illustrate the relevance of modelling realistic farm distributions in the context of epidemic spread by simulating pathogen transmission on an array of spatial distributions of farms. We found that farm distributions generated from the FDM yielded spreading patterns consistent with simulations using observed data, while random point patterns underestimated vulnerability to epidemics. Indeed, spatial clustering increases vulnerability to epidemics, highlighting the relevance of spatial clustering and farm sizes to study epidemic spread. As the FDM maintains a realistic distribution of farms and their size, its use to inform mathematical models of disease transmission is very relevant for regions where these data are not available.

## Requirements

To install requirements:

```R
install.packages("spatstat"); library(spatstat)
install.packages("rgdal"); library(rgdal)
install.packages("rgeos"); library(rgeos)
install.packages("raster"); library(raster)
```

## Data

To run the workflow, users can test the model using Gujarat only. 

The farm locations and size of Gujarat are located in the folder: "01/Data/01_Farm distribution". The data for Thailand and Bangladesh used in the paper are not published. 




