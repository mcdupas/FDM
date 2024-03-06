
print("# -------------------------------------------------------------------")
print("#                   Simulating farm pattern                          ")
print("# -------------------------------------------------------------------")
flush.console()

# This script loads a kppm model and applies it on an area to simulate a point pattern of farms.

#Load model from disk
kppmPath = paste(P_ProcessingFolder,"/","model.kppm", sep="")
load(kppmPath)

# A reason for modeling provinces is because the output gets finer detail if the area to model has smaller area.
# I haven't managed to change the output resolution in by any other way than to adjust the area to model.
# When the modelled area is large, the pattern of farms looks very blocky. 
if (P_ModelLevel == "Country"){
	#Prepare country data
	cborder =  st_read(P_CountryShape, stringsAsFactors = FALSE)
	cborder = cborder[ ,(colnames(cborder) %in% c(P_CodeColumn,"geometry"))] # This assumes specific column names
}else if (P_ModelLevel == "Province"){
	cborder =  st_read(P_ProvinceShape, stringsAsFactors = FALSE)
	cborder = cborder[ ,(colnames(cborder) %in% c(P_CodeColumn2,"geometry"))] # This assumes specific column names
}


names(cborder)[1] = "CODE"
aborder = cborder[cborder$CODE == P_AdmCode_pred,]
#aborder = cborder[cborder$CODE == "BGD",]
#aborder = st_crop(x=aborder, xmin=90, ymin=6, xmax=98, ymax=37)Sim_ppp
#aborder = st_crop(x=aborder, xmin=66, ymin=6, xmax=98, ymax=25)
#aborder = st_crop(x=aborder, xmin=66, ymin=25, xmax=98, ymax=28)
#P_Extent = c(66, 98, 6, 37)


aborder = st_transform(aborder, crs=P_CRS_Model)
aborder = st_cast(aborder,"POLYGON")
aborder = aborder[as.numeric(st_area(aborder)) > 1000000000,] #Remove smaller "island" polygons #1000000000
aborder = as(aborder, "Spatial")

if (P_ModelLevel == "Country"){


	win_border = as(aborder, "owin")
	win_border = simplify.owin(win_border, dmin = 1000) #Simplify the polygon geometry #1000
	#Window(farm_P) = Wind_owin

	#Sim_ppp = simulate.kppm(fit_kppm, nsim = 1, window=win_border, drop=TRUE)  

	Sim_ppplist = simulate.kppm(fit_kppm, nsim = 2, window=win_border)
	#npoints(Sim_ppp)

	# -------------------------------------------------------------------
	#           Adjust number of farms. It may be that the output contains fewer farms than desired. 
	# -------------------------------------------------------------------
	nPoints = npoints(Sim_ppplist[[1]])
	print (paste("farm number = ,", nPoints))
	if (is.na(P_nFarms)){
		Sim_ppp = Sim_ppplist[[1]]
	}else if (nPoints >= P_nFarms){
		samp = sample(x=nPoints, size=P_nFarms)
		Sim_ppp = Sim_ppplist[[1]][samp]
	}else{
		nDiff = P_nFarms - nPoints 
		samp = sample(x=npoints(Sim_ppplist[[2]]), size=nDiff)
		Sim_ppp = superimpose(Sim_ppplist[[1]], Sim_ppplist[[2]][samp])
	}

}else if (P_ModelLevel == "Province"){
	nProvinces = nrow(aborder)
	win_border = as(aborder[1,], "owin")
	Sim_ppp = simulate.kppm(fit_kppm, nsim = 1, window=win_border)[[1]]

	for (i in 2:nProvinces){
		print(paste("Processing",i,"of",nProvinces,"provinces"))
		flush.console()
		win_border = as(aborder[i,], "owin")
		Sim_ppp_temp = simulate.kppm(fit_kppm, nsim = 1, window=win_border)[[1]]
		Sim_ppp = superimpose(Sim_ppp, Sim_ppp_temp)
	}
}

#Save point pattern to disk
pppPath = paste(P_ProcessingFolder,"/","simulated.ppp", sep="")
save(Sim_ppp,file = pppPath)

Sim_pp_sf = st_as_sf(Sim_ppp)
Sim_pp_sf = Sim_pp_sf[st_is(Sim_pp_sf, "POINT"),] #Only keep points. Polygons may have been included
st_crs(Sim_pp_sf) = P_CRS_Model

#Save as shapefile
pShape = paste(P_ProcessingFolder,"/","FarmData_sim_ppp_raw.shp", sep="")
st_write(Sim_pp_sf, pShape, delete_layer=TRUE)

rm(fit_kppm,Sim_ppplist,Sim_pp_sf)







