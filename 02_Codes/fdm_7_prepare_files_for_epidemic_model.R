library(sf)
library(spatstat)

setwd("C:/Users/Admin/Dropbox/OneHealthPoultry/Projects/01_FDM")

# define the different level of clustering
scale_factor_list <- c(0.5, 1, 2)
variance_factor_list <- c(0.5, 1, 2)
nsim <- 40
model_code <- "Broiler_BGD"


# Load the simulated spp
simulation_folder <- paste0("03_Results/01_SPP/", model_code,
                            "/02_Simulations/02_Scenario1/")
# prepare unique combinations of scale and variance factors
combinations <- expand.grid(scale_factor = scale_factor_list,
                            variance_factor = variance_factor_list)

# loop over the unique combinations
for (i in seq_along(combinations$scale_factor)) {
  scale_factor <- combinations$scale_factor[i]
  variance_factor <- combinations$variance_factor[i]

  # load the simulated spp
  spp_file <- paste0(simulation_folder, "Simu_fixed_var",
                     variance_factor, "_sc", scale_factor,
                     "/Simufixed_fit_kppm_var",
                     variance_factor, "_sc", scale_factor, ".ppp")
  load(spp_file)
  # loop over the number of simulations
  for (j in 1:nsim) {
    # extract the simulation
    sim <- simulated_ppp_new[[j]]

    # convert the simulation to a dataframe with x any coordinates
    sim_df <- as.data.frame(sim)
    sim_df$ID <- seq_len(nrow(sim_df))

    # load dataframe containing the stock (number of chickens) for each farm
    stock_file <- paste0(simulation_folder,
                         "Simu_fixed_var",
                         variance_factor,
                         "_sc",
                         scale_factor,
                         "/Stock_",
                         variance_factor,
                         "_sc",
                         scale_factor,
                         "_sim_",
                         j,
                         ".csv")
    stock_df <- read.csv(stock_file)

    # merge the stock data with the simulation data based on ID column
    sim_df <- merge(sim_df, stock_df, by = "ID")
    sim_df$ID <- NULL
    colnames(sim_df) <- c("x", "y", "N")

    # save as a dataframe with x and y coordinates ; spaces to separate columns
    output_file <- paste0(simulation_folder,
                          "data_for_epidemic_model/",
                          model_code,
                          "_scale_",
                          scale_factor,
                          "_variance_",
                          variance_factor,
                          "_sim_", j, ".csv")
    write.table(sim_df, output_file,
                sep = " ",
                row.names = FALSE,
                col.names = TRUE)

  }
}

