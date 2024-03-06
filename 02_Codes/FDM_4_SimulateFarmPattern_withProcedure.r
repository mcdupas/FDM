
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
	cborder = cborder[ ,(colnames(cborder) %in% c(P_CodeColumn2,"geometry",P_CodeColumn3,"NAME_1")) ] # This assumes specific column names
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

### read poultry stats per province for India 
if (P_ModelLevel == "Province"){
   stats_pronvince_file = paste(dir3,)
  }

#### save number of farms per province 
median = 5000    #### gujarat data 
P_nFarms = as.integer(P_TotalStock*0.681/ median)
Province_names =  aborder$HASC_1
Province_names_str = aborder$NAME_1
nFarms_province = c()
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
	nProvinces = length(Province_names)
	win_border = as(aborder[1,], "owin")
	Sim_ppp = simulate.kppm(fit_kppm, nsim = 1000, window=win_border)
	list_npoints = c()
	for (k in 1:1000){
	  list_npoints[k] = npoints(Sim_ppp[[k]])
	}
	nFarms_province[1] = sum(list_npoints)/1000
	for (i in 2:nProvinces){
		print(paste("Processing",i,"of",nProvinces,"provinces"))
		flush.console()
		win_border = as(aborder[i,], "owin")
		Sim_ppp_temp = simulate.kppm(fit_kppm, nsim = 1000, window=win_border)
		list_npoints = c()
		for (k in 1:1000){
		  list_npoints[k] = npoints(Sim_ppp_temp[[k]])
		}
		nFarms_province[i] = sum(list_npoints)/1000
		#Sim_ppp = superimpose(Sim_ppp, Sim_ppp_temp)
	}
}


##### plot pie chart 
nFarms_plot =c()
names_province_plot = c("West Bengal","Andhra Pradesh","Tamil Nadu","Kerala","Karnataka","Maharashtra","Gujarat","Bihar","Others")
index = c()
for (i in 1:length(names_province_plot)){
  for (j in 1:length(Province_names_str)){
    if(names_province_plot[i] == Province_names_str[j]){
      nFarms_plot[i] = nFarms_province[j]
      index = append(index, j)
    }
}
}
nFarms_plot[9] = sum(nFarms_province[-index])
pie(nFarms_plot, labels = names_province_plot)



################################# PROCEDURE 
change_number_farms <- function(Sim_ppplist, i, nFarms_obj){
  nPoints_sim = npoints(Sim_ppplist[[i]])
  if (nPoints_sim >= nFarms_obj){
    samp = sample(x=nPoints_sim, size=nFarms_obj)
    Sim_ppp = Sim_ppplist[[i]][samp]
  }else{
    nDiff = nFarms_obj - nPoints_sim
    samp = sample(x=npoints(Sim_ppplist[[i+1]]), size=nDiff)
    Sim_ppp = superimpose(Sim_ppplist[[i]], Sim_ppplist[[i+1]][samp])
  }
  return(Sim_ppp)
}


P_nFarms_sim = sum(nFarms_province)
ratio_Nfarms = nFarms_province/sum(nFarms_province)
if (P_ModelLevel == "Province"){
  win_border = as(aborder[1,], "owin")
  Sim_ppplist = simulate.kppm(fit_kppm, nsim = 2, window=win_border)
  nFarms_obj = as.integer(ratio_Nfarms[1]*P_nFarms)
  Sim_ppp = change_number_farms(Sim_ppplist, 1, nFarms_obj)
  for (i in 2:nProvinces){
    print(paste("Processing",i,"of",nProvinces,"provinces"))
    win_border = as(aborder[i,], "owin")
    Sim_ppplist = simulate.kppm(fit_kppm, nsim = 2, window=win_border)
    nFarms_obj = as.integer(ratio_Nfarms[i]*P_nFarms)
    Sim_ppp_temp = change_number_farms(Sim_ppplist, 1, nFarms_obj)
    Sim_ppp = superimpose(Sim_ppp, Sim_ppp_temp)
    }
}

#Save point pattern to disk
pppPath = paste(P_ProcessingFolder,"/","simulated_fixedN.ppp", sep="")
save(Sim_ppp,file = pppPath)

Sim_pp_sf = st_as_sf(Sim_ppp)
Sim_pp_sf = Sim_pp_sf[st_is(Sim_pp_sf, "POINT"),] #Only keep points. Polygons may have been included
st_crs(Sim_pp_sf) = P_CRS_Model

#Save as shapefile
pShape = paste(P_ProcessingFolder,"/","FarmData_sim_ppp_raw_fixedN.shp", sep="")
st_write(Sim_pp_sf, pShape, delete_layer=TRUE)

rm(fit_kppm,Sim_ppplist,Sim_pp_sf)







