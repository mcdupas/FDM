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
type_farm = "Layer"  # "Broiler" or "Layer"
# Code for country used for training model
P_AdmCode_train = "IN.GJ" # "BGD" or "IN.GJ" or "THA"
# Code for country to apply model - simulate farm pattern.
P_AdmCode_pred = "IN.GJ" # "BGD" or "IN.GJ" or "THA"


# Farm data used for training model (LGCP)
	P_FarmFile = paste( "01_Data/01_Farm distribution/02_Processed data/",type_farm,"_",P_AdmCode_train,"/FarmData_train.shp", sep="") 
        P_FarmFile_pred = paste( "01_Data/01_Farm distribution/02_Processed data/",type_farm,"_",P_AdmCode_pred,"/FarmData_train.shp", sep="") 

# Column with stock number in farm data file
	P_StockColumn = "Stock" 

	# CRS for geographic coordinates
	CRS_latlon = "+proj=longlat +datum=WGS84 +no_defs"
	# Projected coordinate system used for model data
	P_CRS_Model = "+proj=utm +zone=45 +datum=WGS84 +units=m +no_defs"
	
# Number of farms to simulate - if NA no adjustment for farm number will be made
	P_nFarms = NA
# Total stock in country to adjust to. If NA no adjustment will be made.
	P_TotalStock = NA						##L663300000
# Use country level or province level borders for modeling farms
	#P_ModelLevel = "Country"

###################################################################
## General Settings - LGCP model 
###################################################################

# window borders of area for training and area for simulation
	P_WindowFolder_Train = paste0("01_Data/01_Farm distribution/02_Processed data/",type_farm,"_",P_AdmCode_train,"/win_border.shp")
	P_WindowFolder_Sim = paste0("01_Data/01_Farm distribution/02_Processed data/",type_farm,"_",P_AdmCode_pred,"/win_border.shp")
	
# Directory containing the predictor raster files
	P_PredictorFolder = "01_Data/02_Predictors"

# number of simulations 
nSim = 99
P_nFarms = NA               # adjust the number of farms of simulated spatial points patterns (optional) - NA by default

#Covariates used in the analysis and their corresponding paths to global rasters
P_PredNames = c("Hpop","Crop","Tree","Access_MC1","Access_MC11","Access_MC2","Access_Port12","Proxim_roads","Slope","nChicken")

P_MinStock = 500  # minimum 500 chickens/farm 

###################################################################
## Random Forest parameters - size farm modelling
###################################################################

# random forest parameters - size farm modelling
# Buffer distance around farms for which to extract covariate data
P_BufferDist = 5000 #in meters



# List of countries to train the Random Forest model (farm size) ; select all countries or some specific country
list_country_RF = c("BGD", "THA","IN.GJ")
	
###################################################################################
###               FOLDER TO SAVE RESULTS / OUPUTS etc
###################################################################################
	P_SaveModelFolder = paste0("03_Results/", type_farm, "_",P_AdmCode_train, "/01_SPP/01_Models") 
	P_SaveSimulFolder = paste0("03_Results/", type_farm, "_",P_AdmCode_train, "/01_SPP/02_Simulations")
	P_SaveEnvFolder   = paste0("03_Results/", type_farm, "_",P_AdmCode_train, "/01_SPP/03_Envelope") 
	P_SaveQuadFolder  = paste0("03_Results/", type_farm, "_",P_AdmCode_train, "/01_SPP/04_QuadratCountTest") 
	P_SaveSizeFarmFolder = paste0("03_Results/", type_farm, "_",P_AdmCode_train, "/02_FarmSize")
	
	
	
###################################################################
## Launching the run
###################################################################

start_time = Sys.time()
source("02_Codes/FDM_1_Libraries.r") 
print(Sys.time() - start_time)

source("01_Codes/FDM_2_CreateModel.r")
print(Sys.time() - start_time)

source(paste(P_GenPath, "01_Codes/FDM_3_SimulateFarmPattern.r", sep = ""))
print(Sys.time() - start_time)

source(paste(P_GenPath, "01_Codes/FDM_4_ExtractValues_forRFmodel.r", sep = ""))
print(Sys.time() - start_time)

source(paste(P_GenPath, "01_Codes/FDM_5_Linhom_Envelope.r", sep = ""))
print(Sys.time() - start_time)

source(paste(P_GenPath, "01_Codes/FDM_6_Validation_quadra_kppm.r", sep = ""))
print(Sys.time() - start_time)










