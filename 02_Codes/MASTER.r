###################################################################
## Master script - for Farm Distribution Model
###################################################################
#
###################################################################
## Country specific settings
###################################################################

# change path directory
setwd("C:/Users/Admin/Dropbox/OneHealthPoultry/Projects/01_FDM")

# type of farms
type_farm <- "Broiler"  # "Broiler" or "Layer"
# Code for country used for training model
code_adm_train <- "IN.GJ" # "BGD" or "IN.GJ" or "THA"
# Code for country to apply model - simulate farm pattern.
code_adm_sim <- "IN.GJ" # "BGD" or "IN.GJ" or "THA"

# List of countries to train the Random Forest model (farm size) ;
# select all countries or some specific country
# in this tutorial, only Gujarat is selected,
# because data of Bangladesh and Thailand are not published.
list_country_rf <- c("Broiler_IN.GJ")

# Farm data used for training model (LGCP)
farm_file_train <- paste("01_Data/01_Farm distribution/02_Processed data/",
                         type_farm, "_", code_adm_train,
                         "/FarmData_train.shp", sep = "")

# Farm data of the external area to simulate farm pattern
farm_file_sim <- paste("01_Data/01_Farm distribution/02_Processed data/",
                       type_farm, "_", code_adm_sim,
                       "/FarmData_train.shp", sep = "")

# Column with stock number in farm data file
stock_column <- "Stock"

# CRS for geographic coordinates
crs_latlon <- "+proj=longlat +datum=WGS84 +no_defs"

# Projected coordinate system used for model data
crs_model <- "+proj=utm +zone=45 +datum=WGS84 +units=m +no_defs"

# Number of farms to simulate - if NA no adjustment for farm number will be made
n_farms <- NA
# Total stock in country to adjust to. If NA no adjustment will be made.
total_stock <- NA               ##L663300000


###################################################################
## General Settings - LGCP model
###################################################################

# window borders of area for training and area for simulation
window_folder_train <- paste0("01_Data/01_Farm distribution/02_Processed data/",
                              type_farm, "_", code_adm_train,
                              "/win_border.shp")

window_folder_sim <- paste0("01_Data/01_Farm distribution/02_Processed data/",
                            type_farm, "_", code_adm_sim, "/win_border.shp")

# Directory containing the predictor raster files
predictor_folder <- "01_Data/02_Predictors"

# number of simulations
n_sim <- 100


#Covariates used in the analysis and their corresponding paths to global rasters
pred_names <- c("Hpop", "Crop", "Tree", "Access_MC1", "Access_MC11",
                "Access_MC2", "Access_Port12", "Proxim_roads",
                "Slope", "nChicken")

min_stock <- 500  # minimum 500 chickens/farm

###################################################################
## Random Forest parameters - size farm modelling
###################################################################

# random forest parameters - size farm modelling
# Buffer distance around farms for which to extract covariate data
buffer_distance <- 5000 #in meters






######################################################################
###               FOLDER TO SAVE RESULTS / OUPUTS etc
######################################################################
save_model_folder <- paste0("03_Results/01_SPP/", type_farm,
                            "_", code_adm_train, "/01_Models")
save_simul_folder <- paste0("03_Results/01_SPP/", type_farm,
                            "_", code_adm_train, "/02_Simulations")
save_env_folder   <- paste0("03_Results/01_SPP/", type_farm,
                            "_", code_adm_train, "/03_Envelope")
save_quad_folder  <- paste0("03_Results/01_SPP/", type_farm,
                            "_", code_adm_train, "/04_QuadratCountTest")
save_size_farm_folder <- paste0("03_Results/02_FarmSize/")


###################################################################
## Launching the run
###################################################################

start_time <- Sys.time()
source("02_Codes/FDM_1_Libraries.r")
print(Sys.time() - start_time)

source("01_Codes/FDM_2_CreateModel.r")
print(Sys.time() - start_time)

source(paste(P_GenPath, "01_Codes/FDM_3_SimulateFarmPattern.r", sep = ""))
print(Sys.time() - start_time)

source(paste(P_GenPath, "01_Codes/fdm_4_extract_pred_val_for_rf.R", sep = ""))
print(Sys.time() - start_time)

source(paste(P_GenPath, "01_Codes/FDM_5_Linhom_Envelope.r", sep = ""))
print(Sys.time() - start_time)

source(paste(P_GenPath, "01_Codes/FDM_6_Validation_quadra_kppm.r", sep = ""))
print(Sys.time() - start_time)