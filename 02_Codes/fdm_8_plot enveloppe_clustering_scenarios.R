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

# load lgcp model
save_model_folder <- paste0("03_Results/01_SPP/", type_farm,
                            "_", code_adm_train, "/01_Models")
kppm_path <- paste(save_model_folder, "/", "model_LGCP.kppm", sep = "")
load(kppm_path)

# Load the simulated spp
simulation_folder <- paste0("03_Results/01_SPP/", model_code,
                            "/02_Simulations/02_Scenario1/")

# combinations where only one parameter is varied
combinations_scale <- expand.grid(scale_factor = scale_factor_list,
                                  variance_factor = 1)
combinations_scale$Cluster_param <- "scale"

combinations_variance <- expand.grid(scale_factor = 1,
                                     variance_factor = variance_factor_list)
combinations_variance$Cluster_param <- "variance"

# combine the two dataframes
combinations <- rbind(combinations_scale, combinations_variance)

# personalisation of plot
color <- c("#d703f3", "#838383", "#79b500")
alpha_list <- c(0.15, 0.4, 0.15)
theme_ggplot <- theme(axis.title = element_text(size = 20, colour = "black"),
                      text = element_text(size = 18, colour = "black"),
                      axis.text.y = element_text(colour = "black"),
                      axis.text.x = element_text(colour = "black")) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

# calculate the linhom function of observed data
fd_ppp <- as.ppp(fit_kppm$X)
linhom_obs <- Linhom(fd_ppp, correction = edge_corr)
r_dist <- linhom_obs$r

# loop over combinations of scale and variance factors
for (i in seq_along(unique(combinations$Cluster_param))){
  plot_name <- unique(combinations$Cluster_param)[i]
  combinations_temp <- combinations[combinations$Cluster_param == plot_name, ]

  p_r <- ggplot() +
    geom_line(data = linhom_obs,
              aes(x = r, y = un - r),
              color = "black") +
    labs(title = "Linhom function of observed pattern",
         x = "r",
         y = "Linhom - r") +
    theme_ggplot

  p <- ggplot() +
    geom_line(data = linhom_obs,
              aes(x = r, y = un),
              color = "black") +
    labs(title = "Linhom function of observed pattern",
         x = "r",
         y = "Linhom") +
    theme_ggplot

  p_env <- ggplot() +
    theme_ggplot

  p_env_r <- ggplot() +
    theme_ggplot

  # loop over the unique combinations
  for (j in seq_along(combinations_temp$scale_factor)) {
    comb <- combinations_temp[j, ]

    # check if linhom functions of simulations are already calculated
    linhom_simulations_file <- paste0(simulation_folder,
                                      "Simu_fixed_var",
                                      comb$variance_factor,
                                      "_sc",
                                      comb$scale_factor,
                                      "/linhom_simulations.csv")
    if (file.exists(linhom_simulations_file)) {
      linhom_simulations_df <- read.csv(linhom_simulations_file)
      r_dist <- linhom_simulations_df$r_dist
      linhom_simulations_df$r_dist <- NULL
      linhom_simulations <- as.matrix(linhom_simulations_df)

    } else {
      # load simulated spp
      spp_file <- paste0(simulation_folder, "Simu_fixed_var",
                         comb$variance_factor, "_sc", comb$scale_factor,
                         "/Simufixed_fit_kppm_var",
                         comb$variance_factor, "_sc", comb$scale_factor, ".ppp")
      load(spp_file)

      # calculate the envelope for the simulation with Linhom function
      registerDoParallel(cores = nb_cores)
      linhom_simulations <- foreach(k = 1:100, .combine = cbind,
                                    .packages = "spatstat") %dopar% {
        Linhom(simulated_ppp_new[[k]],
               correction = edge_corr, r = r_dist)[[3]]
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

    # calculate envelope of the linhom functions of the simulations
    env <- global_envelope_test(
      create_curve_set(
        list(
          "r" = r_dist,
          "obs" = linhom_obs$un,
          "sim_m" = linhom_simulations
        )
      ),
      type = "erl", alpha = 0.05
    )

    linhom_simulations_mutated <- linhom_simulations_df %>%
      mutate(row = r_dist)

    # Reshape linhom functions data to long format
    linhom_simulations_long <- linhom_simulations_mutated %>%
      pivot_longer(cols = starts_with("result"),
                   names_to = "variable",
                   values_to = "value")
    linhom_simulations_long$value_r <- linhom_simulations_long$value -
      linhom_simulations_long$row

    # Plot using ggplot2
    p_r <- p_r +
      geom_line(
        data = linhom_simulations_long, aes(
          x = row, y = value_r,
          group = variable
        ),
        color = color[j], alpha = alpha_list[j]
      )

    p <- p +
      geom_line(
        data = linhom_simulations_long,
        aes(x = row,
            y = value,
            group = variable),
        color = color[j],
        alpha = alpha_list[j]
      )

    p_env <- p_env +
      geom_ribbon(data = env,
                  aes(x = r,
                      ymin = lo,
                      ymax = hi),
                  fill = color[j],
                  alpha = alpha_list[j]) +
      geom_line(data = env,
                aes(x = r, y = central),
                linetype = "dashed",
                color = color[j],
                size = 2.0)


    p_env_r <- p_env_r +
      geom_ribbon(data = env,
                  aes(x = r,
                      ymin = lo - r,
                      ymax = hi - r),
                  fill = color[j],
                  alpha = alpha_list[j]) +
      geom_line(data = env,
                aes(x = r, y = central - r),
                linetype = "dashed",
                color = color[j],
                size = 1.0) +
      geom_line(data = env,
                aes(x = r, y = lo - r),
                color = color[j],
                size = 0.5) +
      geom_line(data = env,
                aes(x = r, y = hi - r),
                color = color[j],
                size = 0.5)

  }

  p_env <- p_env +
    geom_line(data = linhom_obs, aes(x = r, y = un), color = "black") +
    labs(title = "Linhom function of observed pattern",
         x = "r",
         y = "Linhom")

  p_env_r <- p_env_r +
    geom_line(data = linhom_obs, aes(x = r, y = un - r), color = "black") +
    labs(x = "r",
         y = "Linhom - r")

  # Save the plots
  ggsave(p_env_r, filename = paste0("03_Results/01_SPP/",
                                    model_code,
                                    "/03_Envelope/scenario1/Envelope_linhom_r_",
                                    plot_name,
                                    ".pdf"),
         width = 6,
         height = 6,
         units = "in",
         dpi = 300)

}