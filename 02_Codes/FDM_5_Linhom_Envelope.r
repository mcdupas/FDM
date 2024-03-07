

print("# -------------------------------------------------------------------")
print("#                   Calculate global rank envelope                   ")
print("# -------------------------------------------------------------------")
flush.console()

# This script is not yet part of the workflow, but contains code to plot graphs for evaluating model performance.
P_Cores = 8
edge_corr = "iso"


if (P_AdmCode_pred== "BGD"){
r_dist = seq(0,80000,500)
}

if (P_AdmCode_pred]== "THA"){
r_dist = seq(0,235000,500)
}

if (P_AdmCode_pred == "IN.GJ"){
r_dist = seq(0,140000,500)
}


### load simulations and model
kppmPath = paste(P_SaveModelFolder,"/","model_LGCP.kppm", sep="")
load(kppmPath)
pppPath = paste(P_SaveSimulFolder,"/",type_farm, "_",P_AdmCode_pred,"_simulated_SPP.ppp", sep="")
load(pppPath)

 #### training dataset
### Load real farms locations 
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


 #### simulation dataset
### Load real farms locations 
FD = st_read(P_FarmFile_pred, stringsAsFactors = FALSE)
FD = st_transform(FD, crs=P_CRS_Model)  

# load country data border   (use "window_border.R" to create border files)
win_border_sf = st_read(P_WindowFolder_Sim)
win_border_sf = st_transform(win_border_sf, crs=P_CRS_Model)
win_border = as.owin(as_Spatial(win_border_sf))

FD_sp = as(FD, 'Spatial')
FD_spoints = as(FD_sp, 'SpatialPoints')
FD_ppp_pred = as.ppp(FD_spoints)
Window(FD_ppp_pred) = win_border

env = envelope(fit_kppm, Linhom, simulate = Sim_ppp, nsim = nSim, correction=edge_corr, r= r_dist, savefuns = TRUE, savepatterns = TRUE)
SaveFile = paste(P_SaveEnvFolder,'/test_env_',type_farm, "_", P_AdmCode_pred,sep="")
save(env,file = SaveFile)


 ## calculate linhom function of trained pattern
  if (P_AdmCode_train != P_AdmCode_pred){
    Lhat_train = Linhom(FD_ppp_train, correction = edge_corr, r=env$r)
  }
    
  ### calculate linhom function of observed pattern
  Lhat_sim_obs = Linhom(FD_ppp_pred, correction = edge_corr, r=env$r)
    
  env$obs = Lhat_sim_obs$iso 

RankEnv = data.frame(matrix(ncol = 12, nrow = 1))
  
colnames(RankEnv) = c("Model", "Sim", "Cluster", "Method", "correction", "dclf$p", "mad$p","dclf$rank", "mad$rank","dclf$u", "mad$mad", "Nsim")

DB_line = 1    
RankEnv[DB_line,1] = paste(type_farm, "_", P_AdmCode_train)
RankEnv[DB_line,2] = paste(type_farm, "_", P_AdmCode_pred)
RankEnv[DB_line,3] = "LGCP"
RankEnv[DB_line,4] = "palm"
RankEnv[DB_line,5] = edge_corr

dclf.result <- dclf.test(env)
mad.result  <- mad.test(env)


# Save p-value and type of GET 
RankEnv[DB_line,6]  = dclf.result$p # p-value
RankEnv[DB_line,7]  = mad.result$p # erl, rank, etc.

RankEnv[DB_line,8]  = dclf.result$statistic[2]
RankEnv[DB_line,9]  = mad.result$statistic[2] 

RankEnv[DB_line,10]  = dclf.result$statistic[1]
RankEnv[DB_line,11] = mad.result$statistic[1] 

RankEnv[DB_line,12]  = nSim # number of simulation

P_Env_Folder = paste(P_SaveEnvFolder, "/stats_env_",type_farm, "_", P_AdmCode_pred, sep="")
save(env, file = P_Env_Folder)



  ####################################################################################################  
  ### code for plotting enveloppe 
  ####################################################################################################  

  env_sim = env
  r_out_env_sim = c() 
  L_out_env_sim = c() 
  index = 1
  for (j in 1:length(env_sim$r)){
    if(env_sim$obs[j]>env_sim$hi[j] | env_sim$obs[j]<env_sim$lo[j]){
      r_out_env_sim[index] = env_sim$r[j]
      L_out_env_sim[index] = env_sim$obs[j]
      index = index +1
    }
  }
  data_out_sim = data.frame(r_out_env_sim, L_out_env_sim)
    
  if (P_AdmCode_train != P_AdmCode_pred){
    env_train = env_sim
    env_train$obs = Lhat_train$iso
    r_out_env_train = c() 
    L_out_env_train = c() 
    index = 1
    for (j in 1:length(env_train$r)){
      if(env_train$obs[j]>env_train$hi[j] | env_train$obs[j]<env_train$lo[j]){
        r_out_env_train[index] = env_train$r[j]
        L_out_env_train[index] = env_train$obs[j]
        index = index +1
      }
    }
    data_out_train = data.frame(r_out_env_train, L_out_env_train)
    
  }

myplot <- ggplot() + geom_ribbon(data=env_sim, aes(x=r*1e-3, ymin=lo*1e-3, ymax=hi*1e-3),fill="gray78", alpha=1) + 
  geom_line(data=env_sim, aes(x=r*1e-3, y=theo*1e-3 ),linetype = "dashed", size =0.8)  +
  geom_line( data=env_sim, aes(x=r*1e-3, y= obs*1e-3),color = "turquoise4", size =0.8)+
  geom_point(data = data_out_sim[seq(1, nrow(data_out_sim), 4), ], aes(x= r_out_env_sim*1e-3, y = L_out_env_sim*1e-3),color = "turquoise4", size = 2) +
  ylab(bquote(L[inhom]~"(r)")) + xlab("r (km)") +
  theme(axis.title=element_text(size=20, colour = "black"), text = element_text(size=18, colour = "black"), axis.text.y=element_text(colour = "black"), 
        axis.text.x=element_text(colour = "black")) +
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),  panel.background = element_blank())

if (P_AdmCode_train != P_AdmCode_pred){
  myplot <- myplot + geom_line( data=env_train, aes(x=r, y= obs), colour = "tan2") +
    geom_point(data = data_out_train[seq(1, nrow(data_out_train), 4), ], aes(x= r_out_env_train, y = L_out_env_train),color = "tan2", size = 1) 
}
ggsave(paste(P_SaveEnvFolder,"/Plot_env_",type_farm, "_", P_AdmCode_pred,".png", sep=""), width = 5,
       height = 4, plot = myplot, dpi = 300)
     




