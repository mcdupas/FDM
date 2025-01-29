
print("# -------------------------------------------------------------------")
print("#                   Simulating farm pattern                          ")
print("# -------------------------------------------------------------------")
flush.console()

# This script loads a kppm model and applies it on an area to simulate
#  a point pattern of farms.

#Load model from disk
kppmpath <- paste(P_SaveModelFolder,
                  "/",
                  "model_LGCP.kppm", sep = "")
load(kppmpath)


# # load country data border
win_border_sf <- st_read(window_folder_sim)
win_border_sf <- st_transform(win_border_sf, crs = crs_model)
win_border <- as.owin(as_Spatial(win_border_sf))

sim_ppp <- simulate.kppm(fit_kppm, n_sim = n_sim + 1, window = win_border)


# -------------------------------------------------------------------
#           Adjust number of farms. It may be that the output contains
#                           fewer farms than desired.
# -------------------------------------------------------------------
for (i in 1:n_sim){
  n_points <- n_points(sim_ppp[[i]])
  if (is.na(n_farms)) {
    sim_ppp[[i]] <- sim_ppp[[i]]
  } else if (n_points >= n_farms) {
    samp <- sample(x = n_points, size = n_farms)
    sim_ppp[[i]] <- sim_ppp[[i]][samp]
  } else {
    ndiff <- n_farms - n_points
    samp <- sample(x = n_points(sim_ppp[[i + 1]]), size = ndiff)
    sim_ppp[[i]] <- superimpose(sim_ppp[[i]], sim_ppp[[i + 1]][samp])
  }
}


#Save point pattern to disk
ppp_path <- paste(save_simul_folder,
                  "/",
                  type_farm,
                  "_",
                  code_adm_sim,
                  "_simulated_SPP.ppp",
                  sep = "")
save(sim_ppp, file = ppp_path)


rm(fit_kppm, sim_ppp)
