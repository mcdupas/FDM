###################################################################
## FDM model file India
###################################################################
#
###################################################################
## Country specific settings
###################################################################

# change path directory 
setwd("C:/Users/Admin/Dropbox/OneHealthPoultry/Projects/01_FDM")

# type of farms 
type_farm = "Broiler"  # "Broiler" or "Layer"
# Code for country used for training model
P_AdmCode_train = "BGD" # "BGD" or "IN.GJ" or "THA"
# Code for country to apply model - simulate farm pattern.
	P_AdmCode_pred = "BGD" # "BGD" or "IN.GJ" or "THA"

# Farm data used for training model 
	P_FarmFile = paste( "01_Data/01_Farm distribution/02_Processed data/",type_farm,"_",P_AdmCode_train,"/FarmData_train.shp", sep="") 
# Column with stock number in farm data file
	P_StockColumn = "Stock" 
# Extent of study area, both for training data and predictions - c(minX, maxX, minY, maxY)
	P_Extent = c(66, 109, 4, 37) ## BGD, Gujarat and Thailand are within this area
  
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
## General Settings
###################################################################

# window borders of training area 
	P_WindowFolder_Train = paste0("01_Data/01_Farm distribution/02_Processed data/",type_farm,"_",P_AdmCode_train,"/win_border.shp")
	
	
	##### OPTIONNEL POUR LE MOMENT
# Shapefile containing country borders
	P_CountryShape = "01_Data/03_Admin Borders/ne_50m_admin_0_countries_lakes.shp"
# Column in the country border file that has the country code
	P_CodeColumn = "adm0_a3"
# Shapefile containing provinces
	P_ProvinceShape = "01_Data/03_Admin Borders/gadm28_adm1.shp"
# Column in the province border file that has the country code
	P_CodeColumn2 = "ISO"
	# Column in the province border file that has the province code	
	P_CodeColumn3 = "HASC_1"
	
	
	
# Directory containing the predictor raster files
	P_PredictorFolder = "01_Data/02_Predictors"


#Covariates used in the analysis and their corresponding paths to global rasters
P_PredNames = c("Hpop","Crop","Tree","Access_MC1","Access_MC11","Access_MC2","Access_Port12","Proxim_roads","Slope","nChicken")
P_PredNames = c("Hpop","Tree","Proxim_roads","Slope","nChicken")

#	random forest parameters 
# Buffer distance around farms for which to extract covariate data
	P_BufferDist = 5000 #in meters

	P_MinStock = 500  # minimum 500 chickens/farm 
	
###################################################################################
###               FOLDER TO SAVE RESULTS / OUPUTS etc
###################################################################################
	P_SaveModelFolder = paste0("03_Results/", type_farm, "_",P_AdmCode_train, "/01_SPP/01_Models") 
	P_SaveSimulFolder = paste0("03_Results/", type_farm, "_",P_AdmCode_train, "/01_SPP/02_Simulations")
	P_SaveEnvFolder   = paste0("03_Results/", type_farm, "_",P_AdmCode_train, "/01_SPP/03_Envelope") 
	P_SaveQuadFolder  = paste0("03_Results/", type_farm, "_",P_AdmCode_train, "/01_SPP/04_QuadratCountTest") 
	
	
	
###################################################################
## Launching the run
###################################################################

start_time = Sys.time()
source("02_Codes/FDM_1_Libraries.r") 
print(Sys.time() - start_time)


source("01_Codes/FDM_3_CreateModel_v2023.r")
print(Sys.time() - start_time)

source(paste(P_GenPath, "01_Codes/FDM_4_SimulateFarmPattern.r", sep = ""))
print(Sys.time() - start_time)

source(paste(P_GenPath, "01_Codes/FDM_5_ExtractValues.r", sep = ""))
print(Sys.time() - start_time)

source(paste(P_GenPath, "01_Codes/FDM_6_PredictFarmSizes.r", sep = ""))
print(Sys.time() - start_time)

source(paste(P_GenPath, "01_Codes/FDM_7_AdjustFarmSizes.r", sep = ""))
print(Sys.time() - start_time)










