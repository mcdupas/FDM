print("# -------------------------------------------------------------------")
print("#                   Calculate global rank envelope                          ")
print("# -------------------------------------------------------------------")
flush.console()

library(sf)
library(maptools)
library(raster)
library(spatstat)
library(RandomFieldsUtils)
library(RandomFields)
library(doParallel)
library(GET)
library(ggplot2)
library(spatstat.core)

# This script is not yet part of the workflow, but contains code to plot graphs for evaluating model performance.
mypath <- getwd()
dir    <- dirname(mypath)    ## parent 1
PathFolder <- paste(dir, "/Dropbox/OneHealthPoultry", sep = "")
setwd(PathFolder)

#Simulate patterns
P_ValNum = 8000
P_Cores = 8
edge_corr = "none"
r_dist = seq(0,200000,500)

P_Model_Code = "Broiler_BGD"
P_Sim_Code   = "Broiler_IN.GJ"
P_ModelFolder = paste("FDM/03_Validation/Train_model/",P_Model_Code,sep="")
P_CRS_Model = "+proj=utm +zone=45 +datum=WGS84 +units=m +no_defs"

### load simulations 
Sim_Path =  paste("FDM/03_Validation/Train_model/",P_Model_Code, "/Sim/Sim_8000_",P_Sim_Code,".ppp", sep="")
load(Sim_Path)

Model_Path = paste("FDM/03_Validation/Train_model/",P_Model_Code, "/model.kppm", sep="")
load(Model_Path)



# Sim_ppp = Sim_ppplist[[1]]
# Sim_pp_sf = st_as_sf(Sim_ppp)
# Sim_pp_sf = Sim_pp_sf[st_is(Sim_pp_sf, "POINT"),] #Only keep points. Polygons may have been included
# st_crs(Sim_pp_sf) = P_CRS_Model
# 
# #Save as shapefile
# pShape = paste("FDM/03_Validation/Train_model/",P_Model_Code, "/Sim/",P_Sim_Code,"FarmData_sim_ppp_raw.shp", sep="")
# st_write(Sim_pp_sf, pShape, delete_layer=TRUE)

#### training dataset
### Load real farms locations 
P_TrainFolder = paste("FDM/03_Validation/Train_model/",P_Model_Code,sep="")

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

#### sim data set
### Load real farms locations 
P_SimFolder = paste("FDM/03_Validation/Train_model/",P_Sim_Code,sep="")

# load country data border   (use "window_border.R" to create border files)
P_WindowFolder_sim = paste(P_SimFolder,"/win_border.shp",sep="")
win_border_sf_sim = st_read(P_WindowFolder_sim)
win_border_sf_sim = st_transform(win_border_sf_sim, crs=P_CRS_Model)
win_border_sim = as.owin(as_Spatial(win_border_sf_sim))

pShape = paste(P_SimFolder,"/","FarmData_train.shp", sep="")
FD_sim = st_read(pShape, stringsAsFactors = FALSE)
FD_sim = st_transform(FD_sim, crs=P_CRS_Model)  

FD_sp_sim = as(FD_sim, 'Spatial')
FD_spoints_sim = as(FD_sp_sim, 'SpatialPoints')
FD_ppp_sim = as.ppp(FD_spoints_sim)
Window(FD_ppp_sim) = win_border_sim



### compute linhom funtcion of sim patterns
registerDoParallel(cores=P_Cores)  # choose the number of of cores to use while paralleling
Sim_Linhom = foreach(j=1:P_ValNum, .combine=cbind) %dopar% { 
  Linhom(Sim_ppplist[[j]], correction = edge_corr, r=r_dist)[[3]]-r_dist}


### compute linhom function of obs data of SIM area
Lhat_sim = Linhom(FD_ppp_sim, correction = "none", r=r_dist)
Lhat_sim$diff = Lhat_sim[[3]]-Lhat_sim$r  

### compute linhom function of obs data of TRAIN area
Lhat_train = Linhom(FD_ppp_train, correction = "none", r=r_dist)
Lhat_train$diff = Lhat_train[[3]]-Lhat_train$r 

### env with obs data is training data
my_summary_stat_inhom = Linhom(FD_ppp_train, correction = "non")
my_summary_stat = my_summary_stat_inhom
env_train = global_envelope_test(create_curve_set( list(
  "r"=r_dist, "obs"= Lhat_train$diff, "sim_m" = Sim_Linhom)), 
  type = "erl", alpha = 0.05)

### env with obs data is SIM data 
my_summary_stat_inhom = Linhom(FD_ppp_sim, correction = "non")
my_summary_stat = my_summary_stat_inhom
env_sim = global_envelope_test(create_curve_set( list(
  "r"=r_dist, "obs"= Lhat_sim$diff, "sim_m" = Sim_Linhom)), 
  type = "erl", alpha = 0.05)


#rm(Sim_ppplist)






### code for plotting enveloppe 
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

myplot <- ggplot() + geom_ribbon(data=env_sim, aes(x=r, ymin=lo, ymax=hi),fill="gray78", alpha=1) + 
  geom_line( data=env_sim, aes(x=r, y= obs))+geom_line(data=env_sim, aes(x=r, y=central ),linetype = "dashed")+
  geom_point(data = data_out_sim[seq(1, nrow(data_out_sim), 4), ], aes(x= r_out_env_sim, y = L_out_env_sim),color = "orchid", size = 1) +
  geom_line( data=env_train, aes(x=r, y= obs), colour = "palegreen4")+
  geom_point(data = data_out_train[seq(1, nrow(data_out_train), 4), ], aes(x= r_out_env_train, y = L_out_env_train),color = "palegreen4", size = 1) +
  ylab(bquote(L[inhom]~"(r)")) + xlab("r (meters)") +
 theme(axis.title=element_text(size=20), text = element_text(size=18)) +
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),  panel.background = element_blank())



ggsave(paste("FDM/03_Validation/Train_model/",P_Model_Code, "/env_",P_Sim_Code,".png", sep=""), width = 5,
       height = 4, plot = myplot, dpi = 300)

#### test example (monte carlo test) https://arxiv.org/pdf/1911.06583.pdf
nsim <- nsimsub <- 199
Sim_Linhom1 = foreach(j=1:nsim, .combine=cbind) %dopar% { 
  Linhom(Sim_ppplist[[j]], correction = edge_corr, r=r_dist)[[3]]-r_dist}

cset <- create_curve_set(list( "r"=r_dist, "obs"= Lhat_sim$diff, "sim_m" = Sim_Linhom1))

adjenvL <- GET.composite(X = cset, type = "erl", verbose = FALSE)

cset.ls <- list()
for(rep in 1:nsim) {
  Sim_Linhom2 = foreach(nsim=1:nsimsub, .combine=cbind) %dopar% { 
    Linhom(Sim_ppplist[[j]], correction = edge_corr, r=r_dist)[[3]]-r_dist}
  cset.ls[[rep]] <- create_curve_set(list("r" = r_dist, "obs" = Lhat_sim$diff, "sim_m" = Sim_Linhom2))
  }

res <- GET.composite(X=cset, X.ls=cset.ls, type='erl')
plot(res, xlab="NOx", ylab="Ecdf")


#### test from example of the documentation 
##https://arxiv.org/pdf/1911.06583v1.pdf
obs.L <- Linhom(FD_ppp_train, correction = "translate")
r <- obs.L[['r']]
obs <- obs.L[['trans']] - r
sim <- matrix(nrow = length(r), ncol = P_ValNum)

for(i in 1:P_ValNum) {sim[, i] <- Lest(Sim_ppplist[[i]], correction = "translate", r = r)[['trans']] - r}

cset <- create_curve_set(list(r = r, obs = obs, sim_m = sim))

