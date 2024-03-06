
print("# -------------------------------------------------------------------")
print("#           Adjusting farm sizes                             ")
print("# -------------------------------------------------------------------")
flush.console()

# This script adjusts the size of the farms so that the total stock is equal to the set variable P_TotalStock.

if (!is.na(P_TotalStock)){

	pShape = paste(P_ProcessingFolder,"/","FarmData_sim.shp", sep="")
	FarmData_sim = st_read(pShape, stringsAsFactors = FALSE)
	
	adjFactor = P_TotalStock/sum(FarmData_sim$Stock)
	FarmData_sim$Stock = adjFactor*FarmData_sim$Stock


	#Save to shapefile
	pShape = paste(P_ProcessingFolder,"/","FarmData_sim_adj.shp", sep="")
	st_write(FarmData_sim, pShape, delete_layer=TRUE)

	# Save a shapefile to plot farm size (with weighted dot on qgis)
	adjFactor = 0.000001
	FarmData_sim$Stock = adjFactor*FarmData_sim$Stock
	pShape = paste(P_ProcessingFolder,"/","FarmData_sim_adj_plot.shp", sep="")
	st_write(FarmData_sim, pShape, delete_layer=TRUE)

}
