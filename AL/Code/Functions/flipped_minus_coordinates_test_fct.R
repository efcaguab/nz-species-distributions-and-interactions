####################################################################
# Audrey Lustig. May 2019  					   #
####################################################################
# Mariona Roige initially wrote this function - December 2018 
# Modified by Audrey Lustig in May 2019 to use the same terminology as in CleanCoordiantes wrapper
####################################################################
# Description: 
# Coordinates of global country or regional boundaries (for terrestrial species) or transboundary river basin boundaries (for freshwater species), along with continents and ecoregion boundaries can be extracted from rnaturalearth package. 
# By overlaying these countries/river basin borders with records of occurrences, it is possible to, 1) identify records in areas outside the expected range of the taxon, for example records in oceans for a terrestrial species, 2) identify a possible mismatch between the reported coordinates and the locality given in the original data. 
# A frequent error in spatial databases includes the accidental omission of the ‘–‘(minus) sign in records from the southern or eastern hemispheres. 


# This function allow to overlay countries/river basin borders with records of occurrences
overlay_coordinates_fct <- function(datafreim_testcoordinates, longitude, latitude, country_shapefile_sf) { 
	# Assign to a record new coordinates - ex: flipped long / lat
	new_coordinates <- datafreim_testcoordinates %>% select(longitude, latitude)
	# convert the coordinates into spatial points 
	sp_new_coordinates <- st_as_sf(new_coordinates, coords = c(longitude, latitude), crs = 4326) 
	# Overaly new set of coordinates with natural earth world country polygons 
	o_temp <- st_join(sp_new_coordinates, country_shapefile_sf, join = st_intersects)
	# Select only relevant columns
	test_country <- data.frame(o_temp, st_coordinates(o_temp)) %>% select (iso_a3)  
	names(test_country) <- paste0("test_", longitude, "_", latitude)
	return(test_country)
}

# This function allow to test whether changing the sign of the coordinates or flipping long/lat coordinates allow to overaly the initial country given in GBIF
correct_coordinates_fct <- function(datafreim, country_shapefile_sf) {
	datafreim_testcoordinates <- datafreim %>% 
			select(-.val,  -.sea, -.summary) %>%  # remove initial summary
			mutate(minusLatitude = -decimalLatitude, # define new columns with different coordinates to test
				minusLongitude = -decimalLongitude,
				flippedLatitude = decimalLongitude,
				flippedLongitude = decimalLatitude)

	# Extract ISO country code for the four different combination of long/lat
	#test1 <- suppressMessages(overlay_coordinates_fct(datafreim_testcoordinates, 'minusLongitude', 'decimalLatitude', country_shapefile_sf)) 
	#test2 <- suppressMessages(overlay_coordinates_fct(datafreim_testcoordinates, 'decimalLongitude', 'minusLatitude', country_shapefile_sf)) 
	test3 <- suppressMessages(overlay_coordinates_fct(datafreim_testcoordinates, 'minusLongitude', 'minusLatitude', country_shapefile_sf))
	test4 <- suppressMessages(overlay_coordinates_fct(datafreim_testcoordinates, 'flippedLongitude', 'flippedLatitude', country_shapefile_sf))
	# Reformat data so that it follows terminology from coordinateClearner wrapper	
	test_dataframe <- #data.frame(datafreim_testcoordinates, test1, test2, test3, test4)  %>% mutate(
			#test_minusLongitude_decimalLatitude = ifelse(countryCode != test_minusLongitude_decimalLatitude | is.na(test_minusLongitude_decimalLatitude) , "FALSE", "TRUE"),
			#test_decimalLongitude_minusLatitude = ifelse(countryCode != test_decimalLongitude_minusLatitude | is.na(test_decimalLongitude_minusLatitude), "FALSE", "TRUE"),
			data.frame(datafreim_testcoordinates, test3, test4)  %>% mutate(test_minusLongitude_minusLatitude = ifelse(countryCode !=  test_minusLongitude_minusLatitude | is.na( test_minusLongitude_minusLatitude), "FALSE", "TRUE"),
			test_flippedLongitude_flippedLatitude = ifelse(countryCode != test_flippedLongitude_flippedLatitude | is.na(test_flippedLongitude_flippedLatitude), "FALSE", "TRUE"))  %>%	mutate(.summary = ifelse( test_minusLongitude_minusLatitude == TRUE | test_flippedLongitude_flippedLatitude == TRUE, "TRUE", "FALSE"))		
	# Return a data frame with suggested corrections to coordinates
	modified_coordinates <- test_dataframe %>% mutate(correctedLongitude = case_when(
								#test_minusLongitude_decimalLatitude == TRUE ~  minusLongitude, 
								#test_decimalLongitude_minusLatitude  == TRUE ~  decimalLongitude,
								test_minusLongitude_minusLatitude == TRUE ~  minusLongitude,
								test_flippedLongitude_flippedLatitude == TRUE ~ flippedLongitude),
				   			correctedLatitude = case_when(
								#test_minusLongitude_decimalLatitude == TRUE ~ decimalLatitude,
								#test_decimalLongitude_minusLatitude  == TRUE ~  minusLatitude, 
								test_minusLongitude_minusLatitude == TRUE ~ minusLatitude,
								test_flippedLongitude_flippedLatitude == TRUE ~ flippedLatitude))  %>%
				dplyr::select(-minusLatitude, -minusLongitude, -flippedLatitude, - flippedLongitude) 
		
	return(modified_coordinates)
}
