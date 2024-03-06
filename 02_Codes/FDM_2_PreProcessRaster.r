
###################################################################
## Data pre-processing of prediction rasters
###################################################################

# This script crops the covariate rasters and projects them to the set coordinate system.

print("# -------------------------------------------------------------------")
print("#                   Pre-processing prediction rasters")
print("# -------------------------------------------------------------------")
flush.console()

# CRS for geographic coordinates
CRS_latlon = "+proj=longlat +datum=WGS84 +no_defs"
CRS_proj = 27700
new_projection <-"+proj=utm +zone=45 +datum=WGS84 +units=m +no_defs"

# Save extent to shapefile
ext_latlon = as(extent(P_Extent), 'SpatialPolygons')
crs(ext_latlon) = CRS_latlon
ext_latlon_sfc = st_as_sfc(ext_latlon) ### convert to sfc


pOutShape = paste(P_ProcessingFolder,"/extent_latlon.shp", sep="")

st_write(ext_latlon_sfc, pOutShape, delete_layer=TRUE)

	
#Process covariate rasters
INPUT_shp = paste(P_ProcessingFolder,"/","extent_latlon.shp", sep="")


#for (i in 1:length(P_PredNames)){
#	print(paste("Cropping/transforming predictor variable",P_PredNames[i]))
#	flush.console()
#	INPUT_tif = P_PredPaths[i]
#	OUTPUT_tif = paste(P_PredictorFolder,"/",P_PredNames[i],".tif", sep = "")
#	r <- raster(INPUT_tif)
#	gdalcall = paste("gdalwarp -cutline", INPUT_shp, "-crop_to_cutline", INPUT_tif, OUTPUT_tif, "-t_srs \"",  P_CRS_Model, "\" -overwrite")
#	eval(parse(text=gdalcall))
#	#crop(r, ext_latlon, filename=OUTPUT_tif, overwrite = TRUE)
	#gdalwarp(srcfile=INPUT_tif, dstfile=OUTPUT_tif,cutline=INPUT_shp, dstalpha=T,of = "GTiff", crop_to_cutline=T, t_srs = P_CRS_Model, overwrite=T)
#}
# P_PredictorFolder = "C:/Users/Admin/Dropbox/OneHealthPoultry/FDM/00_Predictors"
# #### code christoffer replace by lines 31-39
# for (i in 1:length(P_PredNames)){
# 	print(paste("Cropping/transforming predictor variable",P_PredNames[i]))
# 	flush.console()
# 	INPUT_tif = P_PredPaths[i]
# 	OUTPUT_tif = paste(P_PredictorFolder,"/",P_PredNames[i],".tif", sep = "")
# 	gdalcall = paste("gdalwarp -cutline", INPUT_shp, "-crop_to_cutline", INPUT_tif, OUTPUT_tif, "-t_srs \"",  CRS_latlon, "\" -overwrite")
# 	system2('C:\\OSGeo4W64\\OSGeo4W.bat', args=gdalcall)
# }




P_PredictorFolder = "C:/Users/Admin/Dropbox/OneHealthPoultry/FDM/00_Predictors"
#### code christoffer replace by lines 31-39
for (i in 1:length(P_PredNames)){
  print(paste("Cropping/transforming predictor variable",P_PredNames[i]))
  flush.console()
  shapefile <- readOGR(INPUT_shp)
  INPUT_tif = raster(P_PredPaths[i])
  
  shapefile <- spTransform(shapefile, crs(INPUT_tif))
  raster_cropped <- crop(INPUT_tif, extent(shapefile), snap = "out")
  raster_masked <- mask(raster_cropped, shapefile)
  OUTPUT_tif = paste(P_PredictorFolder,"/",P_PredNames[i],"_v2.tif", sep = "")
  writeRaster(raster_masked, OUTPUT_tif, format = "GTiff", overwrite = TRUE)
}


