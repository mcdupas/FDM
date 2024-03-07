
print("# -------------------------------------------------------------------")
print("#           Extracting predictor values for farms                    ")
print("# -------------------------------------------------------------------")
flush.console()

# This script extracts covariate values for both training farms and simulated farms. Covariate values are extracted from a round buffer zone around each farm.


# Extract covariate values for training data
for (j in 1:length(list_country_RF){
	# Read farm training data
	P_FarmFile = paste( "01_Data/01_Farm distribution/02_Processed data/",type_farm,"_", list_country_RF[j],"/FarmData_train.shp", sep="") 
	FarmData_train = st_read(P_FarmFile, stringsAsFactors = FALSE)
	
	# Create buffer around farms 
	FD_buf = st_buffer(FarmData_train, dist=P_BufferDist, endCapStyle = "ROUND", joinStyle = "ROUND", mitreLimit = 1)
	FD_buf = st_transform(FD_buf, crs=CRS_latlon)

	# Create dataframe for storing extracted values
	Extr_train = as.data.frame(FarmData_train$Stock, stringsAsFactors = FALSE)
	names(Extr_train) = "Stock"

	for (i in 1:length(P_PredNames)){
		print(paste("## Extracting covariate", P_PredNames[i]))
		flush.console()
		P_PredPath = paste(P_PredictorFolder,"/", P_PredNames[i], ".tif", sep="")
		extractval = exactextractr::exact_extract(raster(P_PredPath), FD_buf, fun=weighted.mean, na.rm=TRUE)
		eval(parse(text=paste("Extr_train$", P_PredNames[i],"=extractval",sep="")))
	}
	# Save data to disk
	pExtractedVals = paste(P_SaveSizeFarmFolder,"/01_TrainData/",type_farm, "_", list_country_RF[j],"_ExtractTrain.csv",sep="")
	write.csv(Extr_train, file=pExtractedVals, row.names=FALSE)
}	




print("# -------------------------------------------------------------------")
print("#           Extracting predictor values for simulated farms          ")
print("# -------------------------------------------------------------------")
flush.console()

# Read simulated farms
# Load point pattern from disk
pppPath = paste(P_SaveSimulFolder,"/",type_farm, "_",P_AdmCode_pred,"_simulated_SPP.ppp", sep="")
load(pppPath)

for (i in 1:nSim){
	Sim_pp_sf = st_as_sf(Sim_ppp)
	Sim_pp_sf = Sim_pp_sf[st_is(Sim_pp_sf, "POINT"),] #Only keep points. Polygons may have been included
	
	st_crs(Sim_pp_sf) = P_CRS_Model
	
	Sim_pp_buf = st_buffer(Sim_pp_sf, dist=P_BufferDist, endCapStyle = "ROUND", joinStyle = "ROUND", mitreLimit = 1)
	st_crs(Sim_pp_buf) = P_CRS_Model
	Sim_pp_buf = st_transform(Sim_pp_buf, crs=CRS_latlon)
	
	#Create dataframe for storing extracted values
	Extr_pred = as.data.frame(1:nrow(Sim_pp_sf), stringsAsFactors = FALSE)
	colnames(Extr_pred)[1] = "ID"
	
	# Extract covariate data for simulated farms
	for (i in 1:length(P_PredNames)){
		print(paste("## Extracting covariate", P_PredNames[i]))
		flush.console()
		extractval = exact_extract(raster(P_PredPaths[i]), Sim_pp_buf, fun=weighted.mean, na.rm=TRUE)
		#extractval = exact_extract(raster(P_PredPaths[i]), Sim_pp_buf, fun=mean)
		eval(parse(text=paste("Extr_pred$",P_PredNames[i],"=extractval",sep="")))
	}
	
	# Save data to disk
	pExtractedVals = paste(P_SaveSimulFolder,"/ExtractValues/ExtractPred_sim_", type_farm,"_", P_AdmCode_pred, ".csv",sep="")
	write.csv(Extr_pred, file=pExtractedVals, row.names=FALSE)

}








