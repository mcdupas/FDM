library(sf)
library(spatstat)
library(raster)
library(doParallel)
library(GET)
library(tidyverse)

# change path directory
setwd("C:/Users/Admin/Dropbox/OneHealthPoultry/Projects/01_FDM")


# type of farms
type_farm <- "Broiler"  # "Broiler" or "Layer"
# Code for country used for training model
code_adm_train <- "BGD" # "BGD" or "IN.GJ" or "THA"
model_code <- paste(type_farm, "_", code_adm_train, sep = "")

# edge correction for envelope calculation
edge_corr <- "none"

# number of cores to use for parallel processing
nb_cores <- 6

# CRS for geographic coordinates
crs_latlon <- "+proj=longlat +datum=WGS84 +no_defs"

# Projected coordinate system used for model data
crs_model <- "+proj=utm +zone=45 +datum=WGS84 +units=m +no_defs"

# define the different level of clustering - scale and variance factors
scale_factor_list <- c(0.5, 1, 2)
variance_factor_list <- c(0.5, 1, 2)

# create a grid of scale and variance factors combinations
combinations <- expand.grid(scale_factor_list, variance_factor_list)
colnames(combinations) <- c("scale", "variance")

# load lgcp model
save_model_folder <- paste0("03_Results/01_SPP/", type_farm,
                            "_", code_adm_train, "/01_Models")
kppm_path <- paste(save_model_folder, "/", "model_LGCP.kppm", sep = "")
load(kppm_path)

# calculate the linhom function of observed data
fd_ppp <- as.ppp(fit_kppm$X)
linhom_obs <- Linhom(fd_ppp, correction = edge_corr)
r_dist <- linhom_obs$r

# Load the simulated spp
simulation_folder <- paste0("03_Results/01_SPP/", model_code,
                            "/02_Simulations/02_Scenario1/")


# calculate linhom functions and envelope for simulations
# depending on the scale and variance factors
for (i in seq_along(combinations$scale)){
  scale <- combinations$scale[i]
  variance <- combinations$variance[i]

  # check if linhom functions of simulations are already calculated
  linhom_simulations_file <- paste0(simulation_folder,
                                    "Simu_fixed_var",
                                    variance,
                                    "_sc",
                                    scale,
                                    "/linhom_simulations.csv")

  if (file.exists(linhom_simulations_file)) {
    linhom_simulations_df <- read.csv(linhom_simulations_file)
    r_dist <- linhom_simulations_df$r_dist
    linhom_simulations_df$r_dist <- NULL
    linhom_simulations <- as.matrix(linhom_simulations_df)

  } else {
    # load simulated spp
    spp_file <- paste0(simulation_folder, "Simu_fixed_var",
                       variance, "_sc", scale,
                       "/Simufixed_fit_kppm_var",
                       variance, "_sc", scale, ".ppp")
    load(spp_file)

    # calculate the envelope for the simulation with Linhom function
    registerDoParallel(cores = nb_cores)
    linhom_simulations <- foreach(k = 1:100, .combine = cbind,
                                  .packages = "spatstat") %dopar% {
      Linhom(simulated_ppp_new[[k]],
             correction = edge_corr,
             r = r_dist)[[3]]
    }

    # plot linhom functions of each points pattern (all columns of env2)
    linhom_simulations_df <- as.data.frame(linhom_simulations)

    # save linhom function in a csv file
    linhom_simulations_df$r_dist <- r_dist
    write.table(linhom_simulations_df,
                linhom_simulations_file,
                sep = ",",
                row.names = FALSE)
  }

  # Compute the mean Linhom function at each distance r
  mean_linhom <- rowMeans(linhom_simulations_df)

  # Function to calculate Euclidean distance
  calc_distance <- function(simulation, mean_linhom) {
    sqrt(sum((simulation - mean_linhom)^2))
  }

  # Calculate the distances for each simulation
  distances <- apply(linhom_simulations_df, 2, calc_distance, mean_linhom)

  # Get the indices of the 40 closest simulations
  closest_indices <- order(distances)[1:40]

  # Subset the dataframe to include only these 40 simulations
  selected_simulations <- linhom_simulations_df[, closest_indices]


}
