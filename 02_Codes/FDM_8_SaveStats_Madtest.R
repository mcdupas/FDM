print("# -------------------------------------------------------------------")
print("#           Calculate global rank envelope with spatstat             ")
print("# -------------------------------------------------------------------")
flush.console()

library(sf)
library(maptools)
library(raster)
library(spatstat)
library(RandomFieldsUtils)
library(RandomFields)
library(doParallel)
library(ggplot2)
library(foreach)



mypath <- getwd()

#Simulate patterns
P_ValNum = 1000
P_Cores = 8
edge_corr = "iso"

# P_Model_Code  = c("Broiler_BGD","Broiler_THA","Broiler_IN.GJ")
#P_Sim_Code   = c("Broiler_BGD","Broiler_THA","Broiler_IN.GJ")

P_Model_Code = c("Layer_BGD","Layer_THA","Layer_IN.GJ")
P_Sim_Code   = c("Layer_BGD","Layer_THA","Layer_IN.GJ")



P_CRS_Model = "+proj=utm +zone=45 +datum=WGS84 +units=m +no_defs"


for (i in 1:length(P_Model_Code)){
  
  print ("Model")
  print (P_Model_Code[i])
  
  RankEnv = data.frame(matrix(ncol = 12, nrow = length(P_Sim_Code)))
  
  colnames(RankEnv) = c("Model", "Sim", "Cluster", "Method", "correction", "dclf$p", "mad$p","dclf$rank", "mad$rank","dclf$u", "mad$mad", "Nsim")
  
  P_ModelFolder = paste("FDM/03_Validation/Train_model/",P_Model_Code[i],sep="")
  
  Model_Path = paste(P_ModelFolder, "/model.kppm", sep="")
  load(Model_Path)
  
  
  for(j in 1:length(P_Sim_Code)){
    
    print ("Sim")
    print (P_Sim_Code[j])
    
    DB_line = j
    
    if (P_Sim_Code[j]== "Broiler_BGD"){
      r_dist = seq(0,80000,500)
    }
    
    if (P_Sim_Code[j]== "Broiler_THA"){
      r_dist = seq(0,235000,500)
    }
    
    if (P_Sim_Code[j]== "Broiler_IN.GJ"){
      r_dist = seq(0,140000,500)
    }
    
  
    
    #### training dataset of TRAINING dataset
    ### Load real farms locations 
    P_TrainFolder = paste("FDM/03_Validation/Train_model/",P_Model_Code[i],sep="")
    
    # load country data border   (use "window_border.R" to create border files)
    P_WindowFolder = paste(P_TrainFolder,"/win_border.shp",sep="")
    win_border_sf = st_read(P_WindowFolder)
    win_border_sf = st_transform(win_border_sf, crs=P_CRS_Model)
    win_border = as.owin(as_Spatial(win_border_sf))
    
    pShape = paste(P_TrainFolder,"/","FarmData_train.shp", sep="")
    FD = st_read(pShape, stringsAsFactors = FALSE)
    FD = st_transform(FD, crs=P_CRS_Model)  
    
    FD_sp = as(FD, 'Spatial')
    FD_spoints = as(FD_sp, 'SpatialPoints')
    FD_ppp_train = as.ppp(FD_spoints)
    Window(FD_ppp_train) = win_border


    #### training dataset of SIMULATIONS AREA dataset
    ### Load real farms locations 
    P_SimFolder = paste("FDM/03_Validation/Train_model/",P_Sim_Code[j],sep="")
    
    # load country data border   (use "window_border.R" to create border files)
    P_WindowFolder = paste(P_SimFolder,"/win_border.shp",sep="")
    win_border_sf = st_read(P_WindowFolder)
    win_border_sf = st_transform(win_border_sf, crs=P_CRS_Model)
    win_border = as.owin(as_Spatial(win_border_sf))
    
    pShape = paste(P_SimFolder,"/","FarmData_train.shp", sep="")
    FD_obs = st_read(pShape, stringsAsFactors = FALSE)
    FD_obs = st_transform(FD, crs=P_CRS_Model)  
    
    FD_obs_sp = as(FD_obs, 'Spatial')
    FD_obs_spoints = as(FD_obs_sp, 'SpatialPoints')
    FD_ppp_obs = as.ppp(FD_obs_spoints)
    Window(FD_ppp_obs) = win_border
    
    
    
    ## load envelope tests of simulations
    SaveFile = paste('FDM/03_Validation/Train_model/',P_Model_Code[i],'/Env/test_env_',P_Sim_Code[j],'_',P_ValNum,sep="")
    load(SaveFile)
    
    ## calculate linhom function of trained pattern
    if (P_Model_Code[i] != P_Sim_Code[j]){
      Lhat_train = Linhom(FD_ppp_train, correction = edge_corr, r=envLT$r)
    }
    
    ### calculate linhom function of observed pattern
    Lhat_sim_obs = Linhom(FD_ppp_obs, correction = edge_corr, r=envLT$r)
    
    #envLT$obs = Lhat_train$un
    envLT$obs = Lhat_sim_obs$iso 
    envLT$theo = Lhat_sim_obs$theo
    
    ### code for plotting enveloppe 
    env_sim = envLT
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
    
    if (P_Model_Code[i] != P_Sim_Code[j]){
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
    
    if (P_Model_Code[i] != P_Sim_Code[j]){
      myplot <- myplot + geom_line( data=env_train, aes(x=r, y= obs), colour = "tan2") +
        geom_point(data = data_out_train[seq(1, nrow(data_out_train), 4), ], aes(x= r_out_env_train, y = L_out_env_train),color = "tan2", size = 1) 
    }
    #+geom_line(data=env_sim, aes(x=r, y=central ),linetype = "dashed")  
    ggsave(paste("FDM/03_Validation/Train_model/",P_Model_Code, "/env_",P_Sim_Code,".png", sep=""), width = 5,
           height = 4, plot = myplot, dpi = 300)
     
    
    
    RankEnv[DB_line,1] = P_Model_Code[i]
    RankEnv[DB_line,2] = P_Sim_Code[j]
    RankEnv[DB_line,3] = "LGCP"
    RankEnv[DB_line,4] = "palm"
    RankEnv[DB_line,5] = edge_corr
    
    dclf.result <- dclf.test(envLT)
    mad.result  <- mad.test(envLT)
    
    
    # Save p-value and type of GET 
    RankEnv[DB_line,6]  = dclf.result$p # p-value
    RankEnv[DB_line,7]  = mad.result$p # erl, rank, etc.
    
    RankEnv[DB_line,8]  = dclf.result$statistic[2]
    RankEnv[DB_line,9]  = mad.result$statistic[2] 
    
    RankEnv[DB_line,10]  = dclf.result$statistic[1]
    RankEnv[DB_line,11] = mad.result$statistic[1] 
    
    RankEnv[DB_line,12]  = P_ValNum # number of simulation
    
    P_Env_Folder = paste("FDM/03_Validation/Train_model/",P_Model_Code[i], "/env_",P_Sim_Code[j],"_", edge_corr , sep="")
    save(envLT, file = P_Env_Folder)
    rm(envLT, dclf.result, mad.result)
    
  }
  # write table to save the p-values 
  P_Out_Folder = paste("FDM/03_Validation/Train_model/",P_Model_Code[i], "/", sep="")
  #write.csv2(RankEnv, paste(P_Out_Folder,P_Model_Code[i],"_",P_ValNum,"_sim_madtest_Linhom_Sim_",edge_corr,".csv",sep=""),row.names = TRUE)
  rm(RankEnv)
  
  
}


