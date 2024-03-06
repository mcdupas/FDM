library(sf)
library(maptools)
library(raster)
library(spatstat)
library(RandomFieldsUtils)
library(RandomFields)



#####################################################################################
###____________________________INTERNAL VALIDATION _____________________________
####################################################################################
### relative directories
mypath <- getwd()         ## current folder
dir  <- dirname(mypath)    ## parent 1
dir2 <- dirname(dir)      ## parent 2
dir3 <- dirname(dir2)     ## parent 3 


P_ProcessingFolder = paste(mypath,"/2_Processing/",sep="")
nSim = 1000
P_Train_Code = "BGD"
P_Sim_Code   = "BGD"

# Projected coordinate system used for model data
P_CRS_Model = "+proj=utm +zone=45 +datum=WGS84 +units=m +no_defs"

### read data training 
pFarm_train = paste(P_ProcessingFolder,"/","FarmData_train.shp", sep="")
Dtrain_sf <- st_read(pFarm_train)                     ### D for Data
Dtrain_sf = st_transform(Dtrain_sf, crs=P_CRS_Model)  
Dtrain_ppp  <- as.ppp(Dtrain_sf)                      ### ppp object


#############################
#Read and pre-process country layer of training data 
#############################
# Shapefile containing country borders
P_CountryShape = paste(dir3,"/Data/Admin/Admin_level_0/ne_50m_admin_0_countries_lakes.shp",sep="")
# Column in the country border file that has the country code
P_CodeColumn = "adm0_a3"


cborder =  st_read(P_CountryShape, stringsAsFactors = FALSE)
cborder = cborder[ ,(colnames(cborder) %in% c(P_CodeColumn,"geometry"))] # This assumes specific column names
names(cborder)[1] = "CODE"

aborder = cborder[cborder$CODE %in% P_Train_Code,]
aborder = st_transform(aborder, crs=P_CRS_Model)
aborder = st_cast(aborder,"POLYGON")
aborder = aborder[as.numeric(st_area(aborder)) > 1000000000,] #Remove smaller "island" polygons
aborder = as(aborder, "Spatial")


win_border = as(aborder, "owin")
win_border = simplify.owin(win_border, dmin = 1000) #Simplify the polygon geometry
Window(Dtrain_ppp) = win_border


#############################
### load model
#############################
kppmPath = paste(P_ProcessingFolder,"/","model.kppm", sep="")
load(kppmPath)


#### compute quadra
Q <- quadratcount(Dtrain_ppp, nx= 8, ny=8)

#### plot Data training with window (border of the country) and with quadra
plot(Dtrain_ppp, pch=20, cols="grey70", main=NULL)  # Plot points
plot(win_border, add=TRUE)
plot(Q, add=TRUE)  # Add quadrat grid


#### rescale Dtrain_ppp in km instead of meters
Dtrainppp.km <- rescale(Dtrain_ppp, 1000, "km")

# Compute the density for each quadrat (in counts per km2)
Q   <- quadratcount(Dtrainppp.km, nx= 8, ny=8)
Q.d <- intensity(Q)

# Plot the density
plot(intensity(Q, image=TRUE), main=NULL, las=1)  # Plot density raster
plot(Dtrainppp.km, pch=20, cex=0.6, col=rgb(0,0,0,.5), add=TRUE)  # Add points


#############################
#Read and pre-process country layer to simulate data
#############################
cborder =  st_read(P_CountryShape, stringsAsFactors = FALSE)
cborder = cborder[ ,(colnames(cborder) %in% c(P_CodeColumn,"geometry"))] # This assumes specific column names
names(cborder)[1] = "CODE"

aborder = cborder[cborder$CODE %in% P_Sim_Code,]
aborder = st_transform(aborder, crs=P_CRS_Model)
aborder = st_cast(aborder,"POLYGON")
aborder = aborder[as.numeric(st_area(aborder)) > 1000000000,] #Remove smaller "island" polygons
aborder = as(aborder, "Spatial")

win_border = as(aborder, "owin")
win_border = simplify.owin(win_border, dmin = 1000) #Simplify the polygon geometry
Window(Dtrain_ppp) = win_border



######## quadra test
corrcoef_list <- c()
for (i in 1:nSim) {
  Sim_ppp = Sim_ppplist[[1]]
  Q3 <- quadratcount(Sim_ppp, nx=8, ny=8)
  Q1 <-  quadratcount(Dtrain_ppp, nx=8, ny=8)
  
  vector_Q1 = as.vector(t(Q1))
  vector_Q3 = as.vector(t(Q3))
  corrcoef_list[i] = cor(vector_Q1, vector_Q3)
}

# save values
df <- data.frame(corrcoef_list)
path_csv = paste(dir2,"/03_Validation/Recap/corrcoeff_train_",P_Train_Code,"_sim_",P_Sim_Code,".csv", sep="")
write.csv(df,path_csv, row.names = FALSE)



#####################################################################################
###____________________________EXTERNAL VALIDATION _____________________________
####################################################################################
### relative directories
mypath <- getwd()         ## current folder
dir  <- dirname(mypath)   ## parent 1
dir2 <- dirname(dir)      ## parent 2
dir3 <- dirname(dir2)     ## parent 3 

P_Model_Code = "BGD"
P_Sim_Code   = "BGD"
P_ModelFolder = paste(dir2,"/03_Validation/Train_model/",P_Model_Code,sep="")
P_TrainFolder = paste(dir2,"/03_Validation/Train_model/",P_Sim_Code,sep="")
nSim = 1000

# Projected coordinate system used for model data
P_CRS_Model = "+proj=utm +zone=45 +datum=WGS84 +units=m +no_defs"

### read data training 
pFarm_train = paste(P_TrainFolder,"/","FarmData_train.shp", sep="")
Dtrain_sf <- st_read(pFarm_train)                     ### D for Data
Dtrain_sf = st_transform(Dtrain_sf, crs=P_CRS_Model)  
Dtrain_ppp  <- as.ppp(Dtrain_sf)                      ### ppp object


#############################
#Read and pre-process country layer of training/simulating data 
#############################


# Shapefile containing country borders
if (P_Sim_Code=="IN.GJ"){
  P_WindowFolder = paste(P_TrainFolder,"/win_border.shp",sep="")
  win_border_sf = st_read(P_WindowFolder)
  win_border = as.owin(as_Spatial(win_border_sf))
  rm(win_border_sf)
}else{
  # Shapefile containing provinces
  P_CountryShape = paste(dir3,"/Data/Admin/Admin_level_0/ne_50m_admin_0_countries_lakes.shp",sep="")
  # Column in the province border file that has the country code
  P_CodeColumn = "adm0_a3"
  
  cborder =   st_read(P_CountryShape, stringsAsFactors = FALSE)
  
  cborder = cborder[ ,(colnames(cborder) %in% c(P_CodeColumn,"geometry"))]
  names(cborder)[1] = "CODE"
  aborder = cborder[cborder$CODE %in% P_Sim_Code,]
  aborder = st_transform(aborder, crs=P_CRS_Model)
  aborder = st_cast(aborder,"POLYGON")
  aborder = aborder[as.numeric(st_area(aborder)) > 1000000000,] #Remove smaller "island" polygons
  aborder = as(aborder, "Spatial")
  
  
  win_border = as(aborder, "owin")
  win_border = simplify.owin(win_border, dmin = 1000) #Simplify the polygon geometry
  rm(cborder,aborder)
}


Window(Dtrain_ppp) = win_border

rm(Dtrain_sf)

#############################
### load model
#############################
kppmPath = paste(P_ModelFolder,"/","model.kppm", sep="")
load(kppmPath)


#############################
### quadra test
#############################

######## function to change the number of farms 
change_number_farms <- function(Sim_ppplist, i, nFarms_obj){
  nPoints_sim = npoints(Sim_ppplist[[i]])
  if (nPoints_sim >= nFarms_obj){
    samp = sample(x=nPoints_sim, size=nFarms_obj)
    Sim_ppp = Sim_ppplist[[i]][samp]
  }else{
    nDiff = nFarms_obj - nPoints_sim
    samp = sample(x=npoints(Sim_ppplist[[i+1]]), size=nDiff)
    Sim_ppp = superimpose(Sim_ppplist[[i]], Sim_ppplist[[i+1]][samp])
  }
  return(Sim_ppp)
}


####### simulate farms pattern



require(svMisc)
corrcoef_list <- c()
for (i in 1:nSim) {
  progress(i)
  Sim_ppp = Sim_ppplist[[i]]
  Q3 <- quadratcount(Sim_ppp, nx=8, ny=8)
  Q1 <-  quadratcount(Dtrain_ppp, nx=8, ny=8)
  
  vector_Q1 = as.vector(t(Q1))
  vector_Q3 = as.vector(t(Q3))
  
  corrcoef_list[i] = cor(vector_Q1, vector_Q3)
  rm(Q3, Q1, vector_Q1, vector_Q3)
}

# save values
df <- data.frame(corrcoef_list)
path_csv = paste(dir2,"/03_Validation/Recap/fixed number of farms/corrcoeff_model_",P_Model_Code,"_sim_",P_Sim_Code,"_nSim_",nSim,".csv", sep="")
write.csv(df,path_csv, row.names = FALSE)


###### plot intensity 
quadra_count = quadratcount(Sim_ppplist[[1]], nx=8, ny=8)
for (i in 2:nSim){
  quadra_temp = quadratcount(Sim_ppplist[[i]],nx=8, ny=8)
  for (j in 1:length(quadra_count)){
    quadra_count[j] = (quadra_count[j]+quadra_temp[j])/2
  }
  
}

plot(intensity(quadra_count, image=TRUE), main=NULL, las=1)  # Plot density raster
plot(Dtrain_ppp, pch=20, cex=0.6, add=TRUE)  # Add points


###### plot intensity 
quadra_count_test = quadratcount(Sim_ppplist[[1]], nx=8, ny=8)
nSim = 2
for (i in 2:nSim){
  quadra_temp = quadratcount(Sim_ppplist[[i]],nx=8, ny=8)
  for (j in 1:length(quadra_count_test )){
    quadra_count_test[j] = (quadra_count_test[j]+quadra_temp[j])/2
  }
  
}





