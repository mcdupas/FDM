print("# -------------------------------------------------------------------")
print("#                   Calculate global rank envelope                   ")
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
library(ggspatial)
library(splancs)


# This script is not yet part of the workflow, but contains code to plot graphs for evaluating model performance.

#setwd("C:/Users/Marie Cecile Dupas/Dropbox/OneHealthPoultry")
setwd("C:/Users/Admin/Dropbox/OneHealthPoultry")

P_Cores = 1
edge_corr = "iso"
r_dist = seq(0,200000,500)

P_Model_Code = c("Broiler_BGD", "Broiler_IN.GJ", "Broiler_THA", "Layer_BGD", "Layer_IN.GJ", "Layer_THA")
P_Code_Name  = c("BGD", "IN.GJ", "THA", "L_BGD", "L_IN.GJ", "L_THA")
P_CRS_Model = "+proj=utm +zone=45 +datum=WGS84 +units=m +no_defs"




#### training dataset

### Load real farms locations 
for (i in 1:length(P_Model_Code)){
  if (P_Model_Code[i]== "Broiler_BGD" | P_Model_Code[i]== "Layer_BGD"){
    r_dist = seq(0,90000,500)
  }
  
  if (P_Model_Code[i]== "Broiler_THA" | P_Model_Code[i]== "Layer_THA"){
    r_dist = seq(0,240000,500)
  }
  
  if (P_Model_Code[i]== "Broiler_IN.GJ" | P_Model_Code[i]== "Layer_IN.GJ"){
    r_dist = seq(0,150000,500)
  }
  
  
  
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
  
  ### compute linhom function of obs data of TRAIN area
  Lhat_train = Linhom(FD_ppp_train, r = r_dist, correction = "iso")
  print (Lhat_train)
  text = paste("Lhat_", P_Code_Name[i], " = Lhat_train$iso", sep="" )
  eval(parse(text=text))
  Lhat_train$diff = Lhat_train$iso-Lhat_train$r 
  text = paste("Lhat_train_", P_Code_Name[i], " = Lhat_train$iso - Lhat_train$r", sep="" )
  eval(parse(text=text))
  text = paste("Lhat_r_", P_Code_Name[i], " = Lhat_train$r", sep="" )
  eval(parse(text=text))
}

### dataframe broiler
data_BGD = data.frame(Lhat_train_BGD, Lhat_r_BGD, Lhat_BGD)
colnames(data_BGD) = c("diff", "r", "Lhat")
data_IN.GJ = data.frame(Lhat_train_IN.GJ, Lhat_r_IN.GJ, Lhat_IN.GJ)
colnames(data_IN.GJ) = c("diff", "r", "Lhat")
data_THA = data.frame(Lhat_train_THA, Lhat_r_THA, Lhat_THA)
colnames(data_THA) = c("diff", "r", "Lhat")

data_theo = data.frame(Lhat_train$r, Lhat_train$theo)
colnames(data_theo) = c("r", "theo")


### dataframe layer
data_L_BGD = data.frame(Lhat_train_L_BGD, Lhat_r_L_BGD, Lhat_L_BGD)
colnames(data_L_BGD) = c("diff", "r", "Lhat")
data_L_IN.GJ = data.frame(Lhat_train_L_IN.GJ, Lhat_r_L_IN.GJ, Lhat_L_IN.GJ)
colnames(data_L_IN.GJ) = c("diff", "r", "Lhat")
data_L_THA = data.frame(Lhat_train_L_THA, Lhat_r_L_THA, Lhat_L_THA)
colnames(data_L_THA) = c("diff", "r", "Lhat")


### maximum distance of clustered
max_BGD = Lhat_r_L_BGD[which.max(Lhat_train_BGD)]*1e-3
max_L_BGD = Lhat_r_L_BGD[which.max(Lhat_train_L_BGD)]*1e-3
max_THA = Lhat_r_L_THA[which.max(Lhat_train_THA)]*1e-3
max_L_THA = Lhat_r_L_THA[which.max(Lhat_train_L_THA)]*1e-3
max_IN.GJ = Lhat_r_L_IN.GJ[which.max(Lhat_train_IN.GJ)]*1e-3
max_L_IN.GJ = Lhat_r_L_IN.GJ[which.max(Lhat_train_L_IN.GJ)]*1e-3

basic_theme <- theme(axis.title=element_text(size=20, colour = "black"), axis.text.y = element_text(color = "black"),
                     axis.text.x = element_text(color = "black"), text = element_text(size=18, colour = "black")) +
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),  panel.background = element_blank())



col_bgd = "gray50"
col_guj = "deepskyblue4"
col_tha = "gold3"

#geom_vline(xintercept = max_BGD, colour = col_bgd,size =1) + 
#geom_vline(xintercept = max_L_BGD,linetype = "dashed", colour = col_bgd,size =1) +
#geom_vline(xintercept = max_IN.GJ, colour = col_guj,size =1) + 
#geom_vline(xintercept = max_L_IN.GJ,linetype = "dashed", colour = col_guj,size =1) +
#geom_vline(xintercept = max_THA, colour = col_tha,size =1) + 
#geom_vline(xintercept = max_L_THA, linetype = "dashed", colour = col_tha,size =1) +

myplot <- ggplot() + geom_line(data=data_BGD, aes(x=r*1e-3, y=Lhat ), colour = col_bgd,size =1.5) +
  geom_line(data=data_L_BGD, aes(x=r*1e-3, y=Lhat ),linetype = "dashed", colour = col_bgd,size =1.5) +

  geom_line(data=data_IN.GJ, aes(x=r*1e-3, y=Lhat ),colour = col_guj, size =1.5) +
  geom_line(data=data_L_IN.GJ, aes(x=r*1e-3, y=Lhat ),linetype = "dashed",colour = col_guj, size =1.5) +
  
  geom_line(data=data_THA, aes(x=r*1e-3, y=Lhat ), colour = col_tha,size =1.5)+  
  geom_line(data=data_L_THA, aes(x=r*1e-3, y=Lhat ),linetype = "dashed", colour = col_tha,size =1.5) +
  

  ylab(bquote(L[inhom]~"(r)")) + xlab("r (km)") + 
  geom_line(data=data_theo, aes(x=r*1e-3, y=theo ),linetype = "dashed", colour = "black",size =1.5) +
   basic_theme 



#scale_color_manual(name = " ", values = c("BGD" = "gray50", "IN.GJ" = "orchid1", "THA" = "palegreen2")) + 
#geom_line(data=data_theo, aes(x=x*1e-3, y=y ),linetype = "dashed", colour = col_tha,size =1.5)
ggsave("Writting/paper1/figs_final/linhom_obs_data_layer_broiler_iso_corr.png", width = 6, height = 5, plot= myplot, dpi = 300)

###########################################################################

##### cross correlation between layer and broiler points patterns 

###########################################################################


P_Model_Code = c("Broiler_BGD", "Broiler_IN.GJ", "Broiler_THA", "Layer_BGD", "Layer_IN.GJ", "Layer_THA")
P_Code_Name  = c("BGD", "IN.GJ", "THA", "L_BGD", "L_IN.GJ", "L_THA")
P_CRS_Model = "+proj=utm +zone=45 +datum=WGS84 +units=m +no_defs"

i = 1
j = 4
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

FD_broiler = FD_ppp_train

P_TrainFolder = paste("FDM/03_Validation/Train_model/",P_Model_Code[j],sep="")
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

FD_layer = FD_ppp_train
marks(FD_broiler) <- factor("B")
marks(FD_layer) <- factor("L")
#FD_layer <- setmarks(FD_layer, rep("L", FD_layer$n))
FD_ppp <- superimpose(FD_broiler, FD_layer)

## calculate Kcross
sigmaB <- with(split(FD_ppp), bw.scott(B))
sigmaL <- with(split(FD_ppp), bw.scott(L))

lambdaL <- with(split(FD_ppp), density(L, sigmaL))
lambdaB <- with(split(FD_ppp), density(B, sigmaB))

Lcross.func.bgd <- Lcross.inhom(FD_ppp, "B", "L", lambdaB, lambdaL)
Lcross.func.bgd <- Lcross.inhom(FD_ppp, "B", "L")

Lcross.func.tha <- Lcross.inhom(FD_ppp, "B", "L", lambdaB, lambdaL)
plot(Lcross.func, main = "Broiler against Layer", legend = TRUE)


Kfunc.inhom = Kcross.inhom(FD_ppp, "B", "L")
Kall <- alltypes(rescale(FD_ppp), Kcross.inhom)
Lfunc.inhom = Lcross.inhom(FD_ppp, "B", "L")
Lall <- alltypes(rescale(FD_ppp), Lcross.inhom)
Lall <- alltypes(rescale(FD_ppp), Lcross.func.bgd)
nncorr.val <- nncorr(FD_ppp)


nncorr.k1 <- nncorr(FD_ppp)
nncorr.k2 <- nncorr(FD_ppp, k = 2)
nncorr.k3 <- nncorr(FD_ppp, k = 3)
nncorr.k4 <- nncorr(FD_ppp, k = 4)
plot(c(1, 2, 3, 4), c(nncorr.k1[[2]], nncorr.k2[[2]],nncorr.k3[[2]], nncorr.k4[[2]])  )

###########################################################################

##### importance of covariate 

###########################################################################
P_PredNames = c("Hpop","Crop","Tree","Access_MC1","Access_MC11","Access_MC2","Access_Port12","Proxim_roads","Slope","nChicken")

mypath <- getwd()         ## current folder

P_PredictorFolder = paste(mypath,"/FDM/00_Predictors",sep="")

# Read the predictor rasters as image objects	
for (i in 1:length(P_PredNames)){
  Pred_file = paste(P_PredictorFolder,"/",P_PredNames[i],".tif", sep = "")	
  eval(parse(text=paste(P_PredNames[i],"=as.im(raster('",Pred_file,"'))",sep="")))
}

for (i in 1:length(P_Model_Code)){
  P_TrainFolder = paste("FDM/03_Validation/Train_model/",P_Model_Code[i],sep="")
  
  # load country data border   (use "window_border.R" to create border files)
  P_ModelFolder = paste(P_TrainFolder,"/model.kppm",sep="")
  load(P_ModelFolder)
  
  #### coefs table 
  coefs = coef(summary(fit_kppm))
  
  ### table initialization
  imp_covar <- rep(0, length(P_PredNames))
  
  ### loop to calculate importance of covariate
  for (i in 1:length(P_PredNames)){
    text = "imp_covar[i] = log10(exp(max("
    text = paste(text,P_PredNames[i],")*coefs[1+i,1])+1)",sep="")
    eval(parse(text=text))
  }
  
  ### create a dataframe (with colmun 1: name of predictors, column 2: value of the importance)
  df <- data.frame(x=P_PredNames, y=imp_covar)
  colnames(df) <- c("PredNames","ImportanceCovariate")
  data2  <- df[order(df[,2],decreasing=TRUE),]
  #p<-ggplot(data=data2, aes(x=PredNames, y=ImportanceCovariate)) +geom_bar(stat="identity")
  par(mar=c(8,6,1,1))
  p <- barplot(data2[,2],names.arg=data2[,1],las=2, ylab="Log10(Importance Covariate)", las=2 )
  
  ggsave(paste("Presentation/Spell workshop figures/imp_cov_",P_Model_Code[i] ,".png",sep=""), width = 6, height = 5, plot= p, dpi = 300)
  
  rm(fit_kppm)
}






#### Plot patterns 
P_Model_Code = "Broiler_THA"
P_TrainFolder = paste("FDM/03_Validation/Train_model/",P_Model_Code,sep="")

# load country data border   (use "window_border.R" to create border files)
P_WindowFolder = paste(P_TrainFolder,"/win_border.shp",sep="")
win_border_sf = st_read(P_WindowFolder)
win_border_sf = st_transform(win_border_sf, crs=P_CRS_Model)
#win_border = simplify.owin(win_border, dmin = 1000)
win_border = as.owin(as_Spatial(win_border_sf))

pShape = paste(P_TrainFolder,"/","FarmData_train.shp", sep="")
FD = st_read(pShape, stringsAsFactors = FALSE)
FD = st_transform(FD, crs=P_CRS_Model)  

FD_sp = as(FD, 'Spatial')
FD_spoints = as(FD_sp, 'SpatialPoints')
FD_ppp_train = as.ppp(FD_spoints)
Window(FD_ppp_train) = win_border


FD_spoints = as(FD_sp, 'SpatialPoints')
FD_ppp = as.ppp(FD_spoints)
stock  <- FD_sp$Stock             ## farm size of training data 

intensity(FD_ppp)


FD_ppp_m <- setmarks(FD_ppp, stock)



df_plot = data.frame(x = FD_ppp_m$x, y = FD_ppp_m$y, s = FD_ppp_m$marks)

empty_theme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     panel.border = element_blank(),  panel.background = element_blank(),
                     axis.title.x=element_blank(),
                     axis.text.x=element_blank(),
                     axis.ticks.x=element_blank(),
                     axis.title.y=element_blank(),
                     axis.text.y=element_blank(),
                     axis.ticks.y=element_blank())

myplot <- ggplot() +
 geom_point(aes(x = x, y = y, colour = log10(s)), data = df_plot) +
  scale_color_gradient(low = "lightgreen", high = "darkgreen") +
  empty_theme +
 geom_sf(data = win_border_sf, size = 1, color ="gray26", alpha= 0) +
  annotation_scale()

# low = "thistle1", high = "orchid4"  GUJARAT 
# low = "gray80", high = "gray26"   BANGLADESH 
# low = "lightgreen", high = "darkgreen" THAILAND 
  
ggsave(paste("Presentation/Spell workshop figures/pattern_",P_Model_Code ,".png",sep=""), width = 5, height = 5, plot= myplot, dpi = 300)




###### spatial clustering 
P_ModelFolder = paste(P_TrainFolder,"/model.kppm",sep="")
load(P_ModelFolder)



