#####################################################################################
###____________________________QUADRAT COUNT TEST _____________________________
####################################################################################

print("# -------------------------------------------------------------------")
print("#           Calculate quadrat count test                             ")
print("# -------------------------------------------------------------------")

library(sf)
library(maptools)
library(raster)
library(spatstat)
library(RandomFieldsUtils)
library(RandomFields)

quadrat_size = 10 

change_matrix_to_list <- function(matrix){
  list_to_return <- c() 
  for (kk in 1:length(matrix)){
    list_to_return[kk] = matrix[kk]
  }
  return (list_to_return)
}

### load simulations and model
kppmPath = paste(P_SaveModelFolder,"/","model_LGCP.kppm", sep="")
load(kppmPath)
pppPath = paste(P_SaveSimulFolder,"/",type_farm, "_",P_AdmCode_pred,"_simulated_SPP.ppp", sep="")
load(pppPath)

 #### training dataset
### Load real farms locations 
FD = st_read(P_FarmFile, stringsAsFactors = FALSE)
FD = st_transform(FD, crs=P_CRS_Model)  

# load country data border   (use "window_border.R" to create border files)
win_border_sf = st_read(P_WindowFolder_Train)
win_border_sf = st_transform(win_border_sf, crs=P_CRS_Model)
win_border = as.owin(as_Spatial(win_border_sf))

FD_sp = as(FD, 'Spatial')
FD_spoints = as(FD_sp, 'SpatialPoints')
FD_ppp_train = as.ppp(FD_spoints)
Window(FD_ppp_train) = win_border


 #### simulation dataset
### Load real farms locations 
FD = st_read(P_FarmFile_pred, stringsAsFactors = FALSE)
FD = st_transform(FD, crs=P_CRS_Model)  

# load country data border   (use "window_border.R" to create border files)
win_border_sf = st_read(P_WindowFolder_Sim)
win_border_sf = st_transform(win_border_sf, crs=P_CRS_Model)
win_border = as.owin(as_Spatial(win_border_sf))

FD_sp = as(FD, 'Spatial')
FD_spoints = as(FD_sp, 'SpatialPoints')
FD_ppp_pred = as.ppp(FD_spoints)
Window(FD_ppp_pred) = win_border

## calculate quadrat of observed data 
qobs_x = quadrat.test(FD_ppp_train, ny = quadrat_size, nx = quadrat_size)


for (l in 1:length(attr(attr(qobs_x, which = "quadratcount"), which = "tess")$tiles)) {
    # Extract the current tile
    tile <- attr(attr(qobs_x, which = "quadratcount"), which = "tess")$tiles[[l]]
    
    # Calculate its area
    area_temp = area.owin(tile)
    
    # Append the area to the myarea object
    myarea = rbind(myarea, area_temp)
}


# Provided list of quadrats and variables
list_quadrats = attr(attr(qobs_x, which = "quadratcount"),which = "tess")$tiles
list_quad_final = list()  # Use list to store final quadrats
num_quadrat_x = numeric()  # Use numeric vector to store indices

# Only keep quadrats that have a sufficient area (80% of the maximum area)
# create num_quadrat_x which records the number of the kept quadrats 
for (ll in 1:length(list_quadrats)) {
    current_area = area.owin(list_quadrats[[ll]])  # Use area.owin for "owin" objects
    # Check conditions based on type and area
    if(list_quadrats[[ll]]$type == "polygonal") {
        if (current_area >= max(myarea) * 0.80) {
            list_quad_final = c(list_quad_final, list(list_quadrats[[ll]]))
            num_quadrat_x = c(num_quadrat_x, ll)
        }
    } else if(list_quadrats[[ll]]$type == "rectangle") {
        # For rectangles, keep without checking area
        list_quad_final = c(list_quad_final, list(list_quadrats[[ll]]))
        num_quadrat_x = c(num_quadrat_x, ll)
    }
}


 # Coefficient of correlation 
# ----------------------------------------------------
# create a dataframe to store the number of points/quadrat for the observed pattern (qobs)
# and each simulated pattern (qsim1, qsim2,...qsim100)
nameSim = NULL
for (ii in 1:nSim){
  nameSim = c(nameSim, paste("sim",seq(1:nSim)[ii]))
}

num_quadrat = num_quadrat_x
qobs = qobs_x
nb_quadrat = as.data.frame(cbind(num_quadrat, matrix(0,nrow=length(num_quadrat),ncol=nSim+1)))
colnames(nb_quadrat) = c("num_quadrat", "nb_points", nameSim)

# store the number of points/quadrat for the kept quadrats, for the simulated patterns + observed
# compute the correlation coefficient
registerDoParallel(cores=nb_cores)  # choose the number of of cores to use while paralleling
cor_list = foreach(ii=1:nSim, .combine=rbind, .packages='spatstat') %dopar% {
  qsim = quadrat.test(Sim_ppp[[ii]], nx = quadrat_size, ny = quadrat_size)
  for (jj in 1:length(num_quadrat)){
    z = num_quadrat[jj]
    nb_quadrat[jj,2] = qobs$observed[z]
    nb_quadrat[jj,ii+2] = qsim$observed[z]
  }
  #rbind(cor_vec_CSR_Tr,cor(log10(nb_quadrat[2]+1), log10(nb_quadrat[2+i]+1)))
  cor(log10(nb_quadrat[2]+1), log10(nb_quadrat[2+ii]+1))
}   

cor_list_to_csv = change_matrix_to_list(cor_list)

# save values
File_csv = paste(P_SaveQuadFolder, "/corrected_quadratest_log10_",type_farm,"_", P_AdmCode_pred,"_nSim_", nSim,"_nbquad_",length(nb_quadrat[2]),".csv", sep="") 
df <- data.frame(cor_list_to_csv)
write.table(df,File_csv, row.names = FALSE, dec = ".", sep = ",")



