
print("# -------------------------------------------------------------------")
print("#           Training random forest model                             ")
print("# -------------------------------------------------------------------")
flush.console()

# This script trains a random forest model to predict farm stock size based on the extracted covariate values. The model is then applied on the simulated farms.

# Read farm training data
pExtractedVals = paste(P_ProcessingFolder,"/","ExtractTrain.csv",sep="")
Extr_train = read.csv(pExtractedVals, header=TRUE, stringsAsFactors=FALSE)
Extr_train = Extr_train[complete.cases(Extr_train),] #Remove data with missing values

### add lognchicken
#Extr_train$LognChicken = log10(Extr_train$nChicken)
#P_PredNames = c("Hpop","Crop","Tree","Access_MC1","Access_MC11","Access_MC2","Access_Port12","Proxim_roads","Slope","LognChicken")

if (P_RF_transform=="Log"){
  Y = log10(Extr_train$Stock) #Extr_train$ALL_C
  X = as.data.frame(Extr_train)
  X = X[ ,(colnames(X) %in% P_PredNames)]
}


### robust scalar transformation 
if (P_RF_transform=="RobustScalar"){
  Stock = Extr_train$Stock
  robust_scalar<- function(x){(x- median(x)) /(quantile(x,probs = .75)-quantile(x,probs = .25))}
  Y <- as.data.frame(robust_scalar(Stock))
  X = as.data.frame(Extr_train)
  X = X[ ,(colnames(X) %in% P_PredNames)]
}



#Sets the seed
set.seed(2002)

farmRF = ranger(Y ~ ., cbind(Y,X), num.trees=P_RF_nTrees, min.node.size=P_RF_NodeSize_Min, mtry=P_RF_mtry, importance='impurity')

#RFPath = paste(Model_folder,"/","farms.RF", sep="")
#save(farmRF,file = RFPath)
	
print("# -------------------------------------------------------------------")
print("#           Predicting farm sizes using the model                    ")
print("# -------------------------------------------------------------------")
flush.console()

#################################
##  Apply model to make farm size predictions
#################################

# Read farm simulated data
pExtractedVals = paste(P_ProcessingFolder,"/","ExtractPred.csv",sep="")
Extr_pred = read.csv(pExtractedVals, header=TRUE, stringsAsFactors=FALSE)

CompleteX_ind = complete.cases(Extr_pred)
Extr_pred = Extr_pred[CompleteX_ind,] #Remove data with missing values
X = as.data.frame(Extr_pred)
X = X[ ,(colnames(X) %in% P_PredNames)]

farmPredLg = predict(farmRF, X, type = 'response')$predictions
farmPred = (10^(farmPredLg))

FarmData_sim = cbind.data.frame(Extr_pred$ID, farmPred)
names(FarmData_sim) = c("ID","Stock")

# -------------------------------------------------------------------
#           Combine with coordinate data
# -------------------------------------------------------------------
pppPath = paste(P_ProcessingFolder,"/","simulated.ppp", sep="")
load(pppPath)

Sim_pp_sf = st_as_sf(Sim_ppp)
Sim_pp_sf = Sim_pp_sf[st_is(Sim_pp_sf, "POINT"),] 	#Only keep points. Polygons may have been included
Sim_pp_sf = Sim_pp_sf[CompleteX_ind,] 				#if there were missing predictor data, those farms are excluded

FarmData_sim = st_as_sf(cbind(FarmData_sim,Sim_pp_sf$geom))
st_crs(FarmData_sim) = P_CRS_Model

#Save as shapefile
pShape = paste(P_ProcessingFolder,"/","FarmData_sim.shp", sep="")
st_write(FarmData_sim, pShape, delete_layer=TRUE)

#Save importance values to disk
imp = round(importance(farmRF))
imp = rbind(names(imp),imp)
impfile = paste(P_ProcessingFolder,"/","FarmsizeVarImportance.csv", sep = "")
write.csv(imp, file=impfile, row.names=FALSE)

rm(farmRF)




