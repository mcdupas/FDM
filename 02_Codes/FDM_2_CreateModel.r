
###################################################################
## Prepare farm data and create model
###################################################################

# This script pre-processes point and polygon data, and then creates a kppm model.

print("# -------------------------------------------------------------------")
print("#                   Creating model                                   ")
print("# -------------------------------------------------------------------")
flush.console()


# Read and pre-process farm training data
FD = st_read(P_FarmFile, stringsAsFactors = FALSE)
FD = st_transform(FD, crs=P_CRS_Model)    

#Change to ppp format used by the spatstat package
FD_ppp = as.ppp(FD)


#Extract data in order to exclude points with NA values in the covariates.
ExtrTab = as.data.frame(x=1:nrow(FD_sp), stringsAsFactors = FALSE)

for (i in 1:length(P_PredNames)){
	extractval = extract(x=raster(paste(P_PredictorFolder,"/",P_PredNames[i],".tif", sep = "")), y=FD_sp)
	eval(parse(text=paste("ExtrTab$", P_PredNames[i],"=extractval",sep="")))
}

FD_sp = FD_sp[complete.cases(ExtrTab),]
FD = st_as_sf(FD_sp)
FD$Stock = NULL
FD_ppp = as.ppp(FD)

# # load country data border 
win_border_sf = st_read(P_WindowFolder_Train)
win_border_sf = st_transform(win_border_sf, crs=P_CRS_Model)
win_border = as.owin(as_Spatial(win_border_sf))
Window(FD_ppp) = win_border


# Read the predictor rasters as image objects	
for (i in 1:length(P_PredNames)){
	Pred_file = paste(P_PredictorFolder,"/",P_PredNames[i],".tif", sep = "")	
	eval(parse(text=paste(P_PredNames[i],"=as.im(raster('",Pred_file,"'))",sep="")))
}

# -------------------------------------------------------------------
#           LGCP inhomogeneous + Covariates - Train the model
# -------------------------------------------------------------------

#Form syntax for creating model and run it
text = "fit_kppm = kppm(FD_ppp, ~ "
for (i in 1:length(P_PredNames)){
	#text = paste(text,P_PredNames[i]," + I(",P_PredNames[i],"^2)",sep="") #Can be activated if we want to include the squared covariates to better capture non-linear relationships.
	text = paste(text,P_PredNames[i],sep="")
	if (i !=length(P_PredNames)){text = paste(text, "+ ")}
}


text = paste(text, ", clusters = 'LGCP', method = 'palm')", sep="")

eval(parse(text=text))


#Save model to disk
kppmPath = paste(P_SaveModelFolder,"/","model_LGCP.kppm", sep="")
save(fit_kppm,file = kppmPath)


# -------------------------------------------------------------------
#           Calculate the importance of each covariate in the LGCP model
# -------------------------------------------------------------------


#### coefs table 
coefs = fit_kppm$po$coef


### table initialization
imp_covar <- rep(0, length(P_PredNames))
### loop to calculate importance of covariate
for (i in 1:length(P_PredNames)){
  text = "imp_covar[i] = log10(exp(max("
  text = paste(text,P_PredNames[i],")*coefs[1+i])+1)",sep="")
  eval(parse(text=text))
}

### create a dataframe (with colmun 1: name of predictors, column 2: value of the importance)
df <- data.frame(x=P_PredNames, y=imp_covar)
colnames(df) <- c("PredNames","ImportanceCovariate")

write.csv(df,paste(P_SaveModelFolder,"/","importance_covariates_LGCP.csv", sep=""), row.names = FALSE)


rm(fit_kppm,FD_spoints,FD_ppp,FD,FD_sp)

