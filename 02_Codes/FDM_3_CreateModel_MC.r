
###################################################################
## Prepare farm data and create model
###################################################################

# This script pre-processes point and polygon data, and then creates a kppm model.

print("# -------------------------------------------------------------------")
print("#                   Creating model                                   ")
print("# -------------------------------------------------------------------")
flush.console()

# CRS for geographic coordinates
CRS_latlon = "+proj=longlat +datum=WGS84 +no_defs"

# Read and pre-process farm training data
FD = st_read(P_FarmFile, stringsAsFactors = FALSE)
FD = st_transform(FD, crs=P_CRS_Model)    

FD = FD[ ,(colnames(FD) %in% c(P_StockColumn, "geometry"))]
FD_sp = as(FD, "Spatial")
names(FD_sp)[1] = "Stock"

# Exclude farms smaller than the set minimum.
FD_sp = FD_sp[FD_sp$Stock >= P_MinStock,]

#Extract data in order to exclude points with NA values in the covariates.
#Otherwise creating the model could throw an error.
ExtrTab = as.data.frame(x=1:nrow(FD_sp), stringsAsFactors = FALSE)

for (i in 1:length(P_PredNames)){
	extractval = extract(x=raster(paste(P_PredictorFolder,"/",P_PredNames[i],".tif", sep = "")), y=FD_sp)
	eval(parse(text=paste("ExtrTab$", P_PredNames[i],"=extractval",sep="")))
}

FD_sp = FD_sp[complete.cases(ExtrTab),]
FD = st_as_sf(FD_sp)

#Save the training data to shapefile
pShape = paste(P_ProcessingFolder,"/","FarmData_train.shp", sep="")
st_write(FD, pShape, delete_layer=TRUE)

### multiply by a factor the number of stock to plot farms on the map (doesn't work MC 26 01 2021)
#FD_w_sp = FD_sp[complete.cases(ExtrTab*0.001),]
#FD_w = st_as_sf(FD_w_sp)
#FD_weighted = st_as_sf(FD_w)
#pShape2 = paste(P_ProcessingFolder,"/","FarmData_train_weighted.shp", sep="")
#st_write(FD_weighted, pShape2, delete_layer=TRUE)

#Change to ppp format used by the spatstat package
FD_spoints = as(FD_sp, 'SpatialPoints')
FD_ppp = as.ppp(FD_spoints)

#Read and pre-process country layer
cborder =  st_read(P_CountryShape, stringsAsFactors = FALSE)
cborder = cborder[ ,(colnames(cborder) %in% c(P_CodeColumn,"geometry"))] # This assumes specific column names
names(cborder)[1] = "CODE"

aborder = cborder[cborder$CODE %in% P_AdmCode_train,]
aborder = st_transform(aborder, crs=P_CRS_Model)
aborder = st_cast(aborder,"POLYGON")
aborder = aborder[as.numeric(st_area(aborder)) > 1000000000,] #Remove smaller "island" polygons
aborder = as(aborder, "Spatial")

win_border = as(aborder, "owin")
win_border = simplify.owin(win_border, dmin = 1000) #Simplify the polygon geometry
Window(FD_ppp) = win_border

#Calculate Linhom stat
#Linhom_stat = Linhom(FD_ppp, correction = "non")

# Read the predictor rasters as image objects	
for (i in 1:length(P_PredNames)){
	Pred_file = paste(P_PredictorFolder,"/",P_PredNames[i],".tif", sep = "")	
	eval(parse(text=paste(P_PredNames[i],"=as.im(raster('",Pred_file,"'))",sep="")))
}

# -------------------------------------------------------------------
#           LGCP inhomogeneous + Covariates
# -------------------------------------------------------------------

#Form syntax for creating model and run it
text = "fit_kppm = kppm(FD_ppp, ~ "
for (i in 1:length(P_PredNames)){
	#text = paste(text,P_PredNames[i]," + I(",P_PredNames[i],"^2)",sep="") #Can be activated if we want to include the squared covariates to better capture non-linear relationships.
	text = paste(text,P_PredNames[i],sep="")
	if (i !=length(P_PredNames)){text = paste(text, "+ ")}
}


text = paste(text, ", clusters = 'LGCP')", sep="")

eval(parse(text=text))

fit_kppm = kppm(FD_ppp, ~ Hpop + I(Hpop^2) +
		   Crop + I(Crop^2) +
		   Tree + I(Tree^2) + 
		   Access_MC1 + I(Access_MC1^2)+ 
		   Access_Port12 + I(Access_Port12^2)+
			nChicken + I(nChicken^2)+
			Slope+I(Slope^2),
		   clusters = "LGCP")

summary(fit_kppm)

#Save model to disk
kppmPath = paste(P_ProcessingFolder,"/","model_w_quad_term.kppm", sep="")
save(fit_kppm,file = kppmPath)

rm(fit_kppm,FD_spoints,FD_ppp,FD,FD_sp,cborder)

# -------------------------------------------------------------------
#           LGCP inhomogeneous + Covariates + multitype pattern 
# -------------------------------------------------------------------

# training data
FD_spoints = as(FD_sp, 'SpatialPoints')
FD_ppp = as.ppp(FD_spoints)
stock  <- FD_sp$Stock             ## farm size of training data 
marker_farm <-  rep(0, length(stock))
for (i in 1:length(stock)){
	if (stock[i] >= median(stock)){
		marker_farm[i] = "Large"
	}else{
		marker_farm[i] = "Medium"
	}
}


FD_ppp_m <- setmarks(FD_ppp, stock)
FD_multi <- cut(FD_ppp_m, breaks = c(0,median(stock),max(stock)))

### model fit with ppm function multi pattern
text = "fit_kppm = ppm(FD_multi ~ "
for (i in 1:length(P_PredNames)){
	#text = paste(text,P_PredNames[i]," + I(",P_PredNames[i],"^2)",sep="") #Can be activated if we want to include the squared covariates to better capture non-linear relationships.
	text = paste(text,P_PredNames[i],sep="")
	if (i !=length(P_PredNames)){text = paste(text, "+ ")}
}
text = paste(text, ")", sep="")
eval(parse(text=text))


### model fit with kppm function 
text = "fit_kppm = kppm(FD_ppp_m, ~ "
for (i in 1:length(P_PredNames)){
	#text = paste(text,P_PredNames[i]," + I(",P_PredNames[i],"^2)",sep="") #Can be activated if we want to include the squared covariates to better capture non-linear relationships.
	text = paste(text,P_PredNames[i],sep="")
	if (i !=length(P_PredNames)){text = paste(text, "+ ")}
}
text = paste(text, ", clusters = 'LGCP')", sep="")


eval(parse(text=text))

# -------------------------------------------------------------------
#           iCSR - Inhomogeneous poisson process + Covariates
# -------------------------------------------------------------------

model_name = "iCSR"

text = "fit_kppm = ppm(FD_ppp, ~ "
for (i in 1:length(P_PredNames)){
	#text = paste(text,P_PredNames[i]," + I(",P_PredNames[i],"^2)",sep="") #Can be activated if we want to include the squared covariates to better capture non-linear relationships.
	text = paste(text,P_PredNames[i],sep="")
	if (i !=length(P_PredNames)){text = paste(text, "+ ")}
}
text = paste(text, ",)", sep="")


eval(parse(text=text))

summary(fit_ppm)

