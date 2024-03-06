print("# -------------------------------------------------------------------")
print("#                   Plot final importance                           ")
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
library(viridis)

library(reshape2)

setwd("C:/Users/MC/Dropbox/OneHealthPoultry")
mydir = getwd() 

P_Folder = paste( "FDM/03_Validation/Importance_covariates", sep="")

P_PredNames = c("Hpop","Crop","Tree","Access_MC1","Access_MC11","Access_MC2","Access_Port12","Proxim_roads","Slope","nChicken")

P_Model = c("Broiler_BGD", "Layer_BGD", "Broiler_THA", "Layer_THA", "Broiler_IN.GJ", "Layer_IN.GJ")

df = data.frame(P_PredNames) 
for(i in 1:length(P_Model)){
  file_csv <- read.csv(file = paste(P_Folder, "/importance_covariates_Access_", P_Model[i], ".csv", sep=""))
  text = paste("df$", P_Model[i], " = file_csv$ImportanceCovariate", sep="")
  eval(parse(text=text))
}

df_order = df[c(1, 10, 8, 3, 2, 4, 5, 6, 7, 9),]
write.csv(df_order, file = paste(P_Folder, "/importance_covariates_Access_All.csv", sep=""))

rownames(df_order) = df_order$P_PredNames
df_order$P_PredNames = NULL

melted_data <- melt(data)


ggplot(data = melted_data, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()




