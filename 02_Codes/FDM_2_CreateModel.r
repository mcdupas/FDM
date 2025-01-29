
###################################################################
## Prepare farm data and create model
###################################################################

# This script pre-processes point and polygon data,
# and then creates a kppm model.

print("# -------------------------------------------------------------------")
print("#                   Creating model                                   ")
print("# -------------------------------------------------------------------")
flush.console()


# Read and pre-process farm training data
fd <- st_read(farm_file_train, stringsAsFactors = FALSE)
fd <- st_transform(fd, crs = crs_model)

# Change to ppp format used by the spatstat package
fd_ppp <- as.ppp(fd)


#Extract data in order to exclude points with NA values in the covariates.
extr_tab <- as.data.frame(x = seq_len(nrow(fd)), stringsAsFactors = FALSE)

# rename first column by "ID"
colnames(extr_tab)[1] <- "ID"

for (i in seq_len(length(pred_names))){
  extractval <- terra::extract(terra::rast(paste(predictor_folder, "/",
                                                 pred_names[i], ".tif",
                                                 sep = "")),
                               fd)
  eval(parse(text = paste("extr_tab$",
                          pred_names[i],
                          " <- extractval$",
                          pred_names[i], sep = "")))
}

fd <- fd[complete.cases(extr_tab), ]
fd <- st_as_sf(fd)
fd$Stock <- NULL
fd_ppp <- as.ppp(fd)

# # load country data border
win_border_sf <- st_read(window_folder_train)
win_border_sf <- st_transform(win_border_sf, crs = crs_model)
win_border <- as.owin(win_border_sf)
Window(fd_ppp) <- win_border

as_im_spatraster <- function(x) {
  x <- x[[1]]
  g <- as.list(x, geom = TRUE)
  isfact <- is.factor(x)
  if (isfact) {
    v <- matrix(as.data.frame(x)[, 1], nrow=g$nrows, ncol=g$ncols, byrow=TRUE)
  } else {
    v <- as.matrix(x, wide = TRUE)
  }
  vtype <- if (isfact) "factor" else typeof(v)
  if (vtype == "double") vtype <- "real"
  tv <- v[g$nrows:1, ]
  if (isfact) tv <- factor(tv, levels = levels(x))
  out <- list(
    v = tv,
    dim = c(g$nrows, g$ncols),
    xrange = c(g$xmin, g$xmax),
    yrange = c(g$ymin, g$ymax),
    xstep = g$xres[1],
    ystep = g$yres[1],
    xcol = g$xmin + (1:g$ncols) * g$xres[1] + 0.5 * g$xres,
    yrow = g$ymax - (g$nrows:1) * g$yres[1] + 0.5 * g$yres,
    type = vtype,
    units  = list(singular = g$units, plural = g$units, multiplier = 1)
  )
  attr(out$units, "class") <- "unitname"
  attr(out, "class") <- "im"
  out
}

for (i in seq_len(length(pred_names))){
  pred_file <- paste(predictor_folder, "/", pred_names[i], ".tif", sep = "")
  r <- terra::rast(pred_file)
  # change crs
  r <- terra::project(r, crs_model)
  im_obj <- as_im_spatraster(r)
  # rename im_obj with pred_names[i]
  assign(pred_names[i], im_obj, envir = .GlobalEnv)
}


# -------------------------------------------------------------------
#           LGCP inhomogeneous + Covariates - Train the model
# -------------------------------------------------------------------

#Form syntax for creating model and run it
text <- "fit_kppm = kppm(fd_ppp, ~ "
for (i in seq_len(long(length(pred_names)))) {
  text <- paste(text, pred_names[i], sep = "")
  if (i != length(pred_names)) {
    text <- paste(text, "+ ")
  }
}


text <- paste(text, ", clusters = 'Thomas', method = 'palm')", sep = "")

eval(parse(text = text))


#Save model to disk
kppm_path <- paste(P_SaveModelFolder, "/", "model_LGCP.kppm", sep = "")
save(fit_kppm, file = kppm_path)


# -------------------------------------------------------------------
#           Calculate the importance of each covariate in the LGCP model
# -------------------------------------------------------------------


#### coefs table
coefs <- fit_kppm$po$coef


### table initialization
imp_covar <- rep(0, length(pred_names))
### loop to calculate importance of covariate
for (i in seq_len(length(pred_names))){
  text <- "imp_covar[i] = log10(exp(max("
  text <- paste(text, pred_names[i], ")*coefs[1+i])+1)", sep = "")
  eval(parse(text = text))
}

### create a dataframe (with colmun 1: name of predictors,
#                            column 2: value of the importance)
df <- data.frame(x = pred_names, y = imp_covar)
colnames(df) <- c("PredNames", "ImportanceCovariate")

write.csv(df, paste(P_SaveModelFolder, "/",
                    "importance_covariates_LGCP.csv",
                    sep = ""), row.names = FALSE)


rm(fit_kppm, fd_spoints, fd_ppp, fd, fd_sp)

