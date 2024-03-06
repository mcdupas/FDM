

print("# -------------------------------------------------------------------")
print("#                   Calculate global rank envelope                          ")
print("# -------------------------------------------------------------------")
flush.console()

# This script is not yet part of the workflow, but contains code to plot graphs for evaluating model performance.
P_Cores = 8
edge_corr = "iso"


#Load model from disk
kppmPath = paste(P_SaveModelFolder,"/","model_LGCP.kppm", sep="")
load(kppmPath)


if (P_AdmCode_pred== "BGD"){
r_dist = seq(0,80000,500)
}

if (P_AdmCode_pred]== "THA"){
r_dist = seq(0,235000,500)
}

if (P_AdmCode_pred == "IN.GJ"){
r_dist = seq(0,140000,500)
}

RankEnv = data.frame(matrix(ncol = 12, nrow = length(P_Sim_Code)))
  
colnames(RankEnv) = c("Model", "Sim", "Cluster", "Method", "correction", "dclf$p", "mad$p","dclf$rank", "mad$rank","dclf$u", "mad$mad", "Nsim")

### load simulations and model
pppPath = paste(P_SaveSimulFolder,"/",type_farm, "_",P_AdmCode_pred,"_simulated_SPP.ppp", sep="")
load(pppPath)

 #### training dataset
### Load real farms locations 
P_TrainFolder = paste("FDM/03_Validation/Train_model/",P_Model_Code[i],sep="")
FD = st_read(P_FarmFile, stringsAsFactors = FALSE)
FD = st_transform(FD, crs=P_CRS_Model)  

# load country data border   (use "window_border.R" to create border files)
win_border_sf = st_read(P_WindowFolder_Train)
win_border_sf = st_transform(win_border_sf, crs=P_CRS_Model)
win_border = as.owin(as_Spatial(win_border_sf))

FD_sp = as(FD, 'Spatial')
FD_spoints = as(FD_sp, 'SpatialPoints')
FD_ppp_train = as.ppp(FD_spoints)
Window(FD_ppp_train) = win_border


env = envelope(fit_kppm, Linhom, simulate = Sim_ppp, nsim = nSim, correction=edge_corr, r= r_dist, savefuns = TRUE, savepatterns = TRUE)
SaveFile = paste(P_SaveEnvFolder,'test_env_',type_farm, "_", P_AdmCode_pred,sep="")
save(env,file = SaveFile)
rm(env)



