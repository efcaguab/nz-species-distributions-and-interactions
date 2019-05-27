# Audrey Lustig - May 2019
# Search the closest terrestrial polygons using a buffer search zone in the units of the current projection (introduce spatial biais)
# This functiom takes as entry parameters 
# 1. the data frame of presence points with documented ecoregion and realm
# 2. the selected bioclim data set as a stack raster
# 3. the choosen buffer size
# The function return a clean data frame with modified climatic data

extract_bioclimdata_withbuffer_fct <- function(species_climond_realm, climond_layers_stack, buffer_size) {

	nb_climatic_variables <- dim(climond_layers_stack)[3]
	species_climond_realm_na <- species_climond_realm[ !complete.cases( dist_clim[, 1: nb_climatic_variables ] ), ]
	
	# if there are presence points for which no climatic data were found 
	if(nrow(species_climond_realm_na) > 0) {

		# Return a list of possible climatic values for each presence points
		closest_terrestrial_climatic_values <- raster::extract(climond_layers_stack, species_climond_realm_na[ ,c("decimalLongitude", "decimalLatitude")], buffer = buffer_size)

		# Refomrat data from R list to R data.frame
		mean_closest_terrestrial_climatic_values <- matrix(NA,nrow=length(closest_terrestrial_climatic_values), ncol=dim(climond_layers_stack)[3])
		for (i in seq(1, length(closest_terrestrial_climatic_values))){
			non_null_climatic_values <- drop_na( as.data.frame( closest_terrestrial_climatic_values[[i]] ))
			if(nrow(non_null_climatic_values) > 0) {
				mean_closest_terrestrial_climatic_values[i, ] <- apply(non_null_climatic_values, 2, mean)
				}
		}
	
	species_climond_realm_na[1: nb_climatic_variables ] <- mean_closest_terrestrial_climatic_values
	}
	## Replace NA claimtic values with mean values from adjacent terrestrial polygon (within a spatial buffer)
	species_climond_realm[species_climond_realm$record_id %in% species_climond_realm_na$record_id, ] <- species_climond_realm_na
	species_climond_realm_cleaned <- species_climond_realm %>% filter( complete.cases( species_climond_realm[, 1: nb_climatic_variables ] ))
	return(species_climond_realm_cleaned)
}	
