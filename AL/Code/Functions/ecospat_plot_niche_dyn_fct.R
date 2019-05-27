####################################################################
# Audrey Lustig. May 2019  					   #
####################################################################
# modified function 'ecospat.plot.niche' from packge ecospat, to visualise the gridded environmental space (fundamental and realised niche)
# Note to Audrey: change name of variables to have more relevant names + document function

ecospat_plot_niche_dyn_fct <- function(z1, quant, title = "", name.axis1 = "Axis 1",  name.axis2 = "Axis 2", interest = 1, colz1 = "red", colz2 = "#FF000050",  colinter = "#0000FF50", colZ1 = "blue"){
	# Projection of climate into the environmental PCA space	
	z <- t(as.matrix(z1$w))[,nrow(as.matrix(z1$z.uncor)):1]
	# Distribution of the environmental density at all sites of the study area
	z1$Z <- t(as.matrix(z1$Z))[,nrow(as.matrix(z1$Z)):1]
	# Plotting
	# Need to change this function
	# We could just use a gradient of color for density plot as this what we are the most interested in.
	image(x=z1$x,y=z1$y,z=t(as.matrix(z1$z.uncor))[,nrow(as.matrix(z1$z.uncor)):1], col = gray(100:0/100), zlim = c(1e-05, cellStats(z1$z.uncor,"max")), xlab = name.axis1, ylab = name.axis2)
	image(x=z1$x,y=z1$y,z=z, col = c("#FFFFFF00", colinter), add = TRUE)
	title(title)
	contour(x=z1$x,y=z1$y,z1$Z, add = TRUE, levels = quantile(z1$Z[z1$Z > 0], c(0, quant)),drawlabels = FALSE, lty = c(1, 2), col = colZ1)
}


#tutu = as.data.frame(z1$Z)
#row.names(tutu) = as.character(z1$x)
#colnames(tutu) = as.character(z1$y)
#mymelt <- cbind(expand.grid(dimnames(tutu)), melt(tutu)) %>% select(-variable)
#mymelt$value[mymelt$value ==0] <- NA
#mymelt$Var1 <- as.numeric(as.character(mymelt$Var1 ))
#mymelt$Var2 <- as.numeric(as.character(mymelt$Var2 ))
#ggplot() +   geom_tile(data=mymelt, aes(x=Var1, y=Var2, fill=value), alpha=0.8) + theme_bw()  +
#			labs(title = paste0(species_name[species_id], "- envrionemntal density - ", type_analysis, "."), x = "Longitude", y = "Latitude", fill = "Environmental density" ) +
#			scale_fill_viridis_c()
	
