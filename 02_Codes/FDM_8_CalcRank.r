

print("# -------------------------------------------------------------------")
print("#                   Calculate global rank envelope                          ")
print("# -------------------------------------------------------------------")
flush.console()

# This script is not yet part of the workflow, but contains code to plot graphs for evaluating model performance.

#Load model from disk
kppmPath = paste(P_ModelFolder,"/","model.kppm", sep="")
#kppmPath = paste(P_ProcessingFolder,"/","model.kppm", sep="")
#kppmPath = paste(P_ProcessingFolder,"/","model.kppm", sep="")
#kppmPath = paste(P_ProcessingFolder,"/","model_old.kppm", sep="")

load(kppmPath)


#Prepare country data border
cborder =  st_read(P_CountryShape, stringsAsFactors = FALSE)
cborder = cborder[ ,(colnames(cborder) %in% c(P_CodeColumn,"geometry"))] # This assumes specific column names
names(cborder)[1] = "CODE"
aborder = cborder[cborder$CODE == P_AdmCode_train,]
aborder = st_transform(aborder, crs=P_CRS_Model)
aborder = st_cast(aborder,"POLYGON")
aborder = aborder[as.numeric(st_area(aborder)) > 1000000000,] # Remove smaller "island" polygons #1000000000
aborder = as(aborder, "Spatial")
win_border = as(aborder, "owin")
win_border = simplify.owin(win_border, dmin = 1000) # Simplify the polygon geometry #1000

#Simulate patterns
P_ValNum = 100
P_Cores = 1
edge_corr = "none"

#Sim_pp = simulate(fit, nsim = N)
Sim_ppplist = simulate.kppm(fit_kppm, nsim = P_ValNum, window=win_border)

#Read the training data from shapefile
pShape = paste(P_ProcessingFolder,"/","FarmData_train.shp", sep="")
FD = st_read(pShape, stringsAsFactors = FALSE)
FD_sp = as(FD, 'Spatial')
FD_spoints = as(FD_sp, 'SpatialPoints')
FD_ppp = as.ppp(FD_spoints)
Window(FD_ppp) = win_border


# write table to save the p-values 
#RankEnv = data.frame(matrix(ncol = 12, nrow = 4))
#colnames(RankEnv) = c("SA_fitted", "Cluster_model","method", "Cov", "p-value", 
#                      "alternative", "type", "Nsim", "Validation", "SA_predicted", "AIC", "Convergence")


# -------------------------------------------------------------------
#           4/ LGCP inhomogeneous + Covariates
# -------------------------------------------------------------------
#DB_line = 1
#model_name = "iLGCP"
#my_summary_stat = my_summary_stat_inhom

library(doParallel)
library(GET)

# compute the summary statistics on each simulated pattern
#registerDoParallel(cores=P_Cores)  # choose the number of of cores to use while paralleling

#Sim_Lest = foreach(j=1:P_ValNum, .combine=cbind, .packages="spatstat") %dopar% {
#  Lest(as.ppp(Sim_ppplist[[j]]), correction = "none")$un }   # Lest !!!!! 

#Lest(as.ppp(Sim_pp[[j]]), correction = "none")
#Lest = Lest(Sim_ppplist[[1]], correction = "none")$un

# compute the summary statistics on each simulated pattern
r_dist = seq(0,200000,500)

registerDoParallel(cores=P_Cores)  # choose the number of of cores to use while paralleling
Sim_Linhom = foreach(j=1:P_ValNum, .combine=cbind) %dopar% { 
  spatstat::Linhom(Sim_ppplist[[j]], correction = edge_corr, r=r_dist)[[3]]-r_dist}


Lhat_train = Linhom(FD_ppp, correction = "none", r=r_dist)
Lhat_train$diff = Lhat_train[[3]]-Lhat_train$r

plot(Lhat_train$r, Lhat_train$diff, type="l")


#x$diff = x$bord.modif-x$r

#plot(x$r, x$diff, type="l")


#my_summary_stat_inhom = Linhom(Sim_ppplist[[1]], correction = "non")
#my_summary_stat_hom = Lest(Sim_ppplist[[1]], correction = "non")

#my_summary_stat_inhom$bord.modif


# Create global envelope
env = global_envelope_test(create_curve_set( list(
  "r"=r_dist, "obs"= Lhat_train$diff, "sim_m" = Sim_Linhom)), 
  type = "erl", alpha = 0.05)
print("GET done")

plot(env)


### Test clustering and number of farms per province
#Prepare province data border
cborder =  st_read(P_ProvinceShape, stringsAsFactors = FALSE)
cborder = cborder[ ,(colnames(cborder) %in% c(P_CodeColumn2,"geometry"))] # This assumes specific column names
names(cborder)[1] = "CODE"
aborder = cborder[cborder$CODE == P_AdmCode_train,]
aborder = st_transform(aborder, crs=P_CRS_Model)
aborder = st_cast(aborder,"POLYGON")
aborder = aborder[as.numeric(st_area(aborder)) > 1000000000,] #Remove smaller "island" polygons #1000000000
aborder = as(aborder, "Spatial")


res = data.frame(nFarms=integer(),
                 Lhat50=double(),
				 Lhat100=double(),
				 Lhat150=double(),
				 nFarms_sim=integer(),
                 Lhat_sim20=double(),
				 Lhat_sim50=double(),
				 Lhat_sim150=double(),
                 stringsAsFactors=FALSE)

r_dist=c(0,50000,100000,150000)


Sim_ppp = Sim_ppplist[[1]]
nProvinces = nrow(aborder)

for (i in 1:nProvinces){
	
	#Original data
	FD_Prov = FD_ppp
	win_border = as(aborder[i,], "owin")
	Window(FD_Prov) = win_border
	#FD_Prov$n
	Lhat = Linhom(FD_Prov, correction = edge_corr, r=r_dist)
	Lhat_diff50 = Lhat[[3]][2]-Lhat$r[2]
	Lhat_diff100 = Lhat[[3]][3]-Lhat$r[3]
	Lhat_diff150 = Lhat[[3]][4]-Lhat$r[4]
	
	#Simulated data
	Sim_prov_ppp = Sim_ppp
	Window(Sim_prov_ppp) = win_border
	Lhat_sim = Linhom(Sim_prov_ppp, correction = edge_corr, r=r_dist)
	Lhat_sim_diff50 = Lhat_sim[[3]][2]-Lhat_sim$r[2]
	Lhat_sim_diff100 = Lhat_sim[[3]][3]-Lhat_sim$r[3]
	Lhat_sim_diff150 = Lhat_sim[[3]][4]-Lhat_sim$r[4]
	res_tmp = cbind.data.frame(nFarms=FD_Prov$n, Lhat50=Lhat_diff50, Lhat100=Lhat_diff100, Lhat150=Lhat_diff150, 
		nFarms_sim=Sim_prov_ppp$n, Lhat_sim50=Lhat_sim_diff50, Lhat_sim100=Lhat_sim_diff100, Lhat_sim150=Lhat_sim_diff150, stringsAsFactors=FALSE)
	res = rbind.data.frame(res, res_tmp, stringsAsFactors=FALSE) 
}

plot(res$nFarms, res$nFarms_sim)
abline(lm(res$nFarms ~ res$nFarms_sim))
r2 = cor(res$nFarms, res$nFarms_sim)^2
mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
legend("topleft", legend=mylabel, bty="n")

res = res[complete.cases(res),]

plot(res$Lhat50, res$Lhat_sim50)
abline(lm(res$Lhat50 ~ res$Lhat_sim50))
r2 = cor(res$Lhat50, res$Lhat_sim50)^2
mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
coords = par('usr')
legend("topleft", legend=mylabel, bty="n")

plot(res$Lhat100, res$Lhat_sim100)
abline(lm(res$Lhat100 ~ res$Lhat_sim100))
r2 = cor(res$Lhat100, res$Lhat_sim100)^2
mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
coords = par('usr')
legend("topleft", legend=mylabel, bty="n")

plot(res$Lhat150, res$Lhat_sim150)
abline(lm(res$Lhat150 ~ res$Lhat_sim150))
r2 = cor(res$Lhat150, res$Lhat_sim150)^2
mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
coords = par('usr')
legend("topleft", legend=mylabel, bty="n")


win_border = as(aborder, "owin")
win_border = simplify.owin(win_border, dmin = 1000) #Simplify the polygon geometry #1000


###########################################################################

##### importance of covariate 

###########################################################################

#### coefs table 
coefs = coef(summary(fit_kppm))

# Read the predictor rasters as image objects	
for (i in 1:length(P_PredNames)){
  Pred_file = paste(P_PredictorFolder,"/",P_PredNames[i],".tif", sep = "")	
  eval(parse(text=paste(P_PredNames[i],"=as.im(raster('",Pred_file,"'))",sep="")))
}

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
par(mar=c(7,6,1,1))
p<-barplot(data2[,2],names.arg=data2[,1],las=2, ylab="Log10(Importance Covariate)", las=2 )






