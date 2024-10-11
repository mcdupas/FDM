
print("# -------------------------------------------------------------------")
print("#           Extracting predictor values for farms                    ")
print("# -------------------------------------------------------------------")
flush.console()

# This script extracts covariate values for both training farms
# and simulated farms.
# Covariate values are extracted from a round buffer zone around each farm.


# Extract covariate values for training data
for (j in seq_along(list_country_rf)) {
  # Read farm training data
  farm_file_train_rf <- paste0("01_Data/01_Farm distribution/",
                               "02_Processed data/",
                               list_country_rf[j], "/farm_data.shp")
  farm_data_train <- st_read(farm_file_train, stringsAsFactors = FALSE)
  # Create buffer around farms
  fd_buf <- st_buffer(farm_data_train, dist = buffer_distance,
                      endCapStyle = "ROUND", joinStyle = "ROUND",
                      mitreLimit = 1)

  # Create dataframe for storing extracted values
  extr_train <- as.data.frame(farm_data_train$Stock, stringsAsFactors = FALSE)
  names(extr_train) <- "Stock"

  for (i in seq_along(pred_names)){
    print(paste("## Extracting covariate", pred_names[i]))
    flush.console()
    pred_path <- paste(predictor_folder, "/", pred_names[i], ".tif", sep = "")
    extractval <- exactextractr::exact_extract(raster(pred_path), fd_buf,
                                               fun = weighted.mean,
                                               na.rm = TRUE)
    eval(parse(text = paste("extr_train$", pred_names[i],
                            "= extractval", sep = "")))
  }
  # Save data to disk
  extracted_vals_file <- paste(save_size_farm_folder, "/01_TrainData/",
                               list_country_rf[j],
                               "_ExtractTrain.csv", sep = "")
  write.csv(extr_train, file = extracted_vals_file, row.names = FALSE)
}




print("# -------------------------------------------------------------------")
print("#           Extracting predictor values for simulated farms          ")
print("# -------------------------------------------------------------------")
flush.console()

# Read simulated farms
# Load point pattern from disk
ppp_path <- paste(save_simul_folder, "/",
                  type_farm, "_", code_adm_sim,
                  "_simulated_SPP.ppp", sep = "")
load(ppp_path)

for (i in 1:n_sim){
  sim_pp_sf <- st_as_sf(Sim_ppp)
  # Only keep points. Polygons may have been included.
  sim_pp_sf <- sim_pp_sf[st_is(sim_pp_sf, "POINT"), ]

  st_crs(sim_pp_sf) <- crs_model

  sim_pp_buf <- st_buffer(sim_pp_sf, dist = buffer_distance,
                          endCapStyle = "ROUND", joinStyle = "ROUND",
                          mitreLimit = 1)
  st_crs(sim_pp_buf) <- crs_model

  #Create dataframe for storing extracted values
  extr_pred <- as.data.frame(seq_along(sim_pp_sf), stringsAsFactors = FALSE)
  colnames(extr_pred)[1] <- "ID"

  # Extract covariate data for simulated farms
  for (i in seq_along(pred_names)){
    print(paste("## Extracting covariate", pred_names[i]))
    flush.console()
    pred_path <- paste(predictor_folder, "/", pred_names[i], ".tif", sep = "")
    extractval <- exact_extract(raster(pred_path), sim_pp_buf,
                                fun = weighted.mean, na.rm = TRUE)
    eval(parse(text = paste("extr_pred$", pred_names[i],
                            " <- extractval", sep = "")))
  }

  # Save data to disk
  extracted_vals_file <- paste(save_simul_folder,
                               "/ExtractValues/ExtractPred_sim_",
                               type_farm, "_", code_adm_sim, ".csv", sep = "")
  write.csv(extr_pred, file = extracted_vals_file, row.names = FALSE)

}
