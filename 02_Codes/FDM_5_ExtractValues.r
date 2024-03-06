
print("# -------------------------------------------------------------------")
print("#           Extracting predictor values for farms                    ")
print("# -------------------------------------------------------------------")
flush.console()

# This script extracts covariate values for both training farms and simulated farms. Covariate values are extracted from a round buffer zone around each farm.


# CRS for geographic coordinates
CRS_latlon = "+proj=longlat +datum=WGS84 +no_defs"

# Read farm training data
pShape = paste(P_ProcessingFolder,"/","FarmData_train.shp", sep="")
FarmData_train = st_read(pShape, stringsAsFactors = FALSE)
#FarmData_train = FarmData_train[ ,(colnames(FarmData_train) %in% c(P_StockColumn, "geometry"))] # This assumes specific column names

# Create buffer around farms 
FD_buf = st_buffer(FarmData_train, dist=P_BufferDist, endCapStyle = "ROUND", joinStyle = "ROUND", mitreLimit = 1)
FD_buf = st_transform(FD_buf, crs=CRS_latlon)



# Create dataframe for storing extracted values
Extr_train = as.data.frame(FarmData_train$Stock, stringsAsFactors = FALSE)
names(Extr_train) = "Stock"

# Extract covariate values for training data
for (i in 1:length(P_PredNames)){
	print(paste("## Extracting covariate", P_PredNames[i]))
	flush.console()
	extractval = exact_extract(raster(P_PredPaths[i]), FD_buf, fun=weighted.mean, na.rm=TRUE)
	eval(parse(text=paste("Extr_train$", P_PredNames[i],"=extractval",sep="")))
}

### test MC 19/03/2021
# flush.console()
# extractval = exact_extract(raster(P_PredPaths[10]), FD_buf, fun=sum, na.rm=TRUE) 
# index_na = which(is.na(extractval))
# FD_na = FD_buf$geometry[index_na]
# pShape = paste(P_ProcessingFolder,"/","FD_bul_nanchicken.shp", sep="")
# st_write(FD_na, pShape, delete_layer=TRUE)


# Save data to disk
pExtractedVals = paste(P_ProcessingFolder,"/","ExtractTrain.csv",sep="")
write.csv(Extr_train, file=pExtractedVals, row.names=FALSE)

print("# -------------------------------------------------------------------")
print("#           Extracting predictor values for simulated farms          ")
print("# -------------------------------------------------------------------")
flush.console()

# Read simulated farms
# Load point pattern from disk
pppPath = paste(P_ProcessingFolder,"/","simulated_fixedN.ppp", sep="")
load(pppPath)

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
pExtractedVals = paste(P_ProcessingFolder,"/","ExtractPred_fixedN.csv",sep="")
write.csv(Extr_pred, file=pExtractedVals, row.names=FALSE)







