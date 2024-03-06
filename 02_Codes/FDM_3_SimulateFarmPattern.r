
print("# -------------------------------------------------------------------")
print("#                   Simulating farm pattern                          ")
print("# -------------------------------------------------------------------")
flush.console()

# This script loads a kppm model and applies it on an area to simulate a point pattern of farms.

#Load model from disk
kppmPath = paste(P_SaveModelFolder,"/","model_LGCP.kppm", sep="")
load(kppmPath)


# # load country data border 
win_border_sf = st_read(P_WindowFolder_Sim)
win_border_sf = st_transform(win_border_sf, crs=P_CRS_Model)
win_border = as.owin(as_Spatial(win_border_sf))

Sim_ppp = simulate.kppm(fit_kppm, nsim = nSim+1, window=win_border)  


# -------------------------------------------------------------------
#           Adjust number of farms. It may be that the output contains fewer farms than desired. 
# -------------------------------------------------------------------
for (i in 1:nSim){
	nPoints = npoints(Sim_ppp[[i]])
	if (is.na(P_nFarms)){
		Sim_ppp[[i]] = Sim_ppp[[i]]
	}else if (nPoints >= P_nFarms){
		samp = sample(x=nPoints, size=P_nFarms)
		Sim_ppp[[i]] = Sim_ppp[[i]][samp]
	}else{
		nDiff = P_nFarms - nPoints 
		samp = sample(x=npoints(Sim_ppp[[i+1]]), size=nDiff)
		Sim_ppp[[i]] = superimpose(Sim_ppp[[i]], Sim_ppp[[i+1]][samp])
	}
}


#Save point pattern to disk
pppPath = paste(P_SaveSimulFolder,"/",type_farm, "_",P_AdmCode_pred,"_simulated_SPP.ppp", sep="")
save(Sim_ppp, file = pppPath)


rm(fit_kppm,Sim_ppplist,Sim_pp_sf)







