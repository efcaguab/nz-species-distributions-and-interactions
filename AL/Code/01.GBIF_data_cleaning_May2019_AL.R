####################################################################
# Audrey Lustig. May 2019  					   #
####################################################################
# This script :
# - loads GBIF occurrences data for the list of species give in Data/0.nb_rawoccurences_download_GBIF2019.rds,
# - cleans the data by performing a series of coordinate checks
# - saves cleaned data into folder Data/cleaned_GBIF_occurences
####################################################################

rm(list=ls())

####################################################################
# Call global libraries     					   #
####################################################################
library(maptools) #  Set of tools for manipulating geographic data.
library(sf) # Support for simple features, a standardized way to encode spatial vector data.
library(raster) #  Reading, writing, manipulating, analyzing and modeling of gridded spatial data.
library(tidyverse) # Data wrangling
# Pitfall: name clashes
# Just loading the tidyverse reveals a pitfall of using spatial data with the tidyverse that affects the raster package
# The final x shows that dplyr’s select() function has boshed (technically speaking, masked) raster’s select function. 
# This can cause issues. To avoid this pitfall I suggest using dplyr::select() and raster::select() rather than just select() when using this conflicted function name if you use raster and the tidyverse.
library(countrycode) # Standardize country names, convert them into one of eleven coding schemes, convert between coding schemes, and assign region descriptors - https://github.com/vincentarelbundock/countrycode
library(rnaturalearth) # access to a pre-downloaded subset of Natural Earth v4.1.0 (March 2018) vector data commonly used in world mapping - https://github.com/ropensci/rnaturalearth
#library(CoordinateCleaner) # Should be an automated code to clean GBIF data but had to modify it - you still need to install the package to access data - https://github.com/cran/CoordinateCleaner
library(foreach) # Support for the foreach looping construct.
library(iterators) # Support for iterators, which allow a programmer to traverse through all the elements of a vector, list, or other collection of data.
library(doParallel) #  Provides a parallel backend for the %dopar% function using the parallel package.

# To load customized functions
sourceDir <- function(path, trace = TRUE, ...) {
      for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
         source(file.path(path, nm))
      }
    }
    	

####################################################################
# Set directories						   #
####################################################################
# Set working directory - should be the same than the one defined in 0.Download_GBIF_Raw_Data.R
main_wd <- "/home/audrey/Documents/UC/Fernando"
setwd(main_wd)

# Set function directory
functions_wd <- paste0(main_wd, "/Code/Functions/")
sourceDir(path = paste0(functions_wd, "CoordinateCleaner-master/") )
sourceDir(path = paste0(functions_wd))

# Create output directory
output_figures <- 'cleaning_proccess/'
if (!dir.exists(paste0(main_wd, "/Figures/"))){ dir.create(paste0(main_wd, "/Figures/")) }
if (!dir.exists(paste0(main_wd, "/Figures/", output_figures))){ dir.create(paste0(main_wd,"/Figures/", output_figures)) }
if (!dir.exists(paste0(main_wd, "/Data/cleaned_GBIF_occurences/"))){ dir.create(paste0(main_wd, "/Data/cleaned_GBIF_occurences/")) }

rgbif_download <- FALSE	 

# Load metadata about rds object
if (!rgbif_download) { occurences_summary <- readRDS( file = paste0(main_wd, "/Data/GBIFmanualDownload/00.nbrawoccurences_manualdownload_may2019.rds") ) }
else { nb_limit <- 550000
	occurences_summary <- readRDS( file = paste0(main_wd, "/Data/rgbig_download/00.nbrawoccurences_rgbifdownload_may2019_limits", nb_limit, ".rds")) }
species_name <- as.character(occurences_summary$species_name) #sapply(occurences_downloaded$species_name, function(x) str_replace_all(as.character(x), fixed(" "), "_"))
nb_occurences_downloaded <- occurences_summary$nb_occurences_downloaded
nb_species <- nrow(occurences_summary) 


# Define cluster type and number of cores
cl = makeCluster(2, outfile='', type='FORK')
# Register cluster
registerDoParallel(cl)

# export vairables to cluster
clusterExport(cl, c('main_wd', 'species_name', 'nb_occurences_downloaded', 'nb_species')) 

 
#for( species_id in seq(1, nb_species) ) {
nb_clean_occurences <- foreach(species_id = icount(nb_species), .combine = c)  %dopar% {
	# Load R object 
	# if downloaded through occ_search() function from rgbif
	if (rgbif_download) {species_occurence_data <- readRDS(paste0(main_wd, "/Data/rgbif_download/", species_name[species_id], "_rgbif_rawdata_", nb_occurences_downloaded[species_id], "_may2019.rds")) }
	else{ species_occurence_data <- readRDS(paste0(main_wd, "/Data/GBIFmanualDownload/", species_name[species_id], "_manualdownload_rawdata_", nb_occurences_downloaded[species_id], "_may2019.rds")) }
	
	species_occurence_data <- species_occurence_data %>% mutate(decimalLongitude = as.numeric(as.character(decimalLongitude)), decimalLatitude = as.numeric(as.character(decimalLatitude)), coordinateUncertaintyInMeters  = as.numeric(as.character(coordinateUncertaintyInMeters)), year  = as.numeric(as.character(year)) )

	# Create a unique identifier for each record
	coord_data <- species_occurence_data %>% mutate( record_id = seq(1: nrow(species_occurence_data)) )

	# Remove records without coordinates or names of country
	coord_data <- coord_data  %>% filter(!is.na(decimalLongitude)) %>%  filter(!is.na(decimalLatitude)) %>% filter(!is.na(countryCode)) 

	# Visualize the data on a map  
	world_data <- suppressMessages(map_data('world'))
	#world_data <- subset(world_data , region != "Antarctica")
	worldmap <- ggplot() + geom_map(data=world_data , map=world_data , aes(x=long, y=lat, group=group, map_id=region), fill="gray", colour="#7f7f7f", size=0.3) +
		 theme_bw() + coord_fixed() +
		 labs(title=paste0(nrow(coord_data), "2 raw occurrences - ",species_name[species_id]," - GBIF - May 2019"), x="Longitude", y="Latitude") +
		 geom_point(data = coord_data ,aes(x = decimalLongitude, y = decimalLatitude), color = "darkred", size = 0.5)  

	ggsave(plot=worldmap, filename = paste0(main_wd,"/Figures/", output_figures, "00.rawdata", species_name[species_id], "_occurences_", nrow(coord_data) ,"_GBIF2019.png"), w = 30, h = 15, units = "cm", dpi = 600 )
	
	####################################################################
	# Step 1: Remove empty records and erroneous records    	   #
	####################################################################	
	# As a first step we will run the automatic cleaning algorithm of CoordinateCleaner. 
	# https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.13152
	# The clean_coordinates function is a wrapper around a large set of automated cleaning steps to flag errors that are common to biological collections, including: removing all duplicates (records of the same species sent to different collections) and incomplete datazero coordinates,  removing records with geographic coordinates of 0 longitude, 0 latitude, likely to represent an erroneous repetitive data entry which frequently appears in databases such as GBIF,  eliminating records in which the longitude and latitude values are identical and likely to represent an erroneous repetitive data entry, also a common error in the GBIF data set, removing coordinates assigned to country and province centroids, coordinates assigned to capitals, and coordinates assigned to biodiversity institutions or GBIF headquarters.
	# You can switch on each test individually using logical flags, modify the sensitivity of most individual tests using the “.rad” arguments, and provide custom gazetteers using the “.ref” arguments. For the purpose of this demonstration I used default parameters.
	
	# Backup original GBIF country code
	coord_data$countryCode_iso2c <- coord_data$countryCode
	# Convert country code from ISO2c to ISO3c
	coord_data$countryCode <-  countrycode(coord_data$countryCode_iso2c, origin =  'iso2c', destination = 'iso3c')
	
	# Flag coordinate issues
	coord_data <- data.frame(coord_data)
	# Warning clean_cordinates still work with sp packages is deprecated and will be replaced by sf. Functions like sp::over are likely to disappear! 
	flags <- clean_coordinates(x = coord_data, lon = "decimalLongitude", lat = "decimalLatitude",  countries = "countryCode", species = "scientificName", tests = c("capitals", "centroids", "equal","duplicates", "gbif", "institutions", "zeros") , verbose = TRUE, report = FALSE) # most test are on by default
	
	flagged_coordinates <-  flags %>% filter(.summary == FALSE)
	worldmap_flagged_coordinates <- worldmap +  geom_point(data = flagged_coordinates ,aes(x = decimalLongitude, y = decimalLatitude), color = "darkblue", size = 0.5) +
		 labs(title=paste0(nrow(flagged_coordinates), " occurrences removed - ",species_name[species_id]," - GBIF - May 2019"), x="Longitude", y="Latitude") 
	
	ggsave(plot=worldmap_flagged_coordinates, filename = paste0(main_wd,"/Figures/", output_figures, "01.flaged_coordinates_occurrences_", species_name[species_id], "_occurences_", nrow(flagged_coordinates) ,"_GBIF2019.png"), w = 30, h = 15, units = "cm", dpi = 600 )

	
	# Remove erroneous data and select relevant column
	if (rgbif_download) { data_fields <- c("record_id", "key" , "basisOfRecord", "scientificName", "decimalLongitude" ,  "decimalLatitude", "coordinateUncertaintyInMeters",  "year", "geodeticDatum", "countryCode", "country" , "individualCount", "countryCode_iso2c" ) }
	else{ data_fields <- c("record_id", "taxonKey" , "basisOfRecord", "scientificName", "decimalLongitude" ,  "decimalLatitude", "coordinateUncertaintyInMeters",  "year", "countryCode",  "countryCode_iso2c" ) }
	coord_first_clean <- flags %>% filter(.summary == TRUE) %>% select(data_fields)
	
	
	####################################################################
	# Step 2: Country test						   #
	####################################################################
	# As a first step we will run the automatic cleaning algorithm of CoordinateCleaner to test if given GBIF coordinates are from the country indicated in the GBIF country column.
	# We will also identified sea coordinates for terrestrial species.  
	# The second step comprises identifying erroneous records of occurrence. Coordinates of global country or regional boundaries (for terrestrial species) or transboundary river basin boundaries (for freshwater species), along with continents and ecoregion boundaries can be extracted from rnaturalearth package. By overlaying these countries/river basin borders with records of occurrences, it is possible to, 1) identify records in areas outside the expected range of the taxon, for example records in oceans for a terrestrial species, 2) identify a possible mismatch between the reported coordinates and the locality given in the original data. A frequent error in spatial databases includes the accidental omission of the ‘–‘(minus) sign in records from the southern or eastern hemispheres. Extreme care needs to be exercised with the cleaning process because when modifying the data it is possible that records outside the habitat zone are not errors. For example, a record may have been recorded on an island too small for mapping at the resolution used, or may simply result from an incorrect manipulation when extracting the original data. It is strongly recommended to always check the erroneous data against the original document to accurately identify the sources of errors.


	# Remove occurences from Antartica that are outside the range of the studied species
	# These occurences are also problematic when trying to swap coordinates (following tests)
	coord_first_clean <- coord_first_clean %>% filter(countryCode != 'ATA')

	# Flag records for which the coordinates are in the sea or there is a mismatch between the reported coordinates and the locality given in the original data
	# Warning clean_cordinates still work with sp packages is deprecated and will be replaced by sf.
	# Note to Audrey: this function could be optimised by downloading the sea_shapefile once oustide of the loop and setting it up at the entry parameter of the function
	# this would allow to get to a lower resolution = 10 m and therefore mapping smaller island or country boundaries with more precision
	flags_country_sea <- clean_coordinates(x = coord_first_clean, lon = "decimalLongitude", lat = "decimalLatitude",  countries = "countryCode", species = "scientificName", tests = c("ountry", "seas") , verbose = TRUE, report = FALSE, seas_scale = 50) 
	
	# If we identify a mismatch between the recorded country names and recorded coordinates or sea coordinates, test whether there was an issue with coordinates
	# Common issues include reversed x or y coordinates, or flipped coordinates
	# test 1) is the sign of the x coordinate reversed? 
	# test 2) is the sign of the y coordinate reversed? 
	# test 3) are both x and y coordinate signs reversed? 
	# test 4) are the coordinates flipped (x is y and y is x)?
	# For Fernando dataset, i have intentionally removed test 1 and test 2... I had issues with records points in Europe. 
	if(!(FALSE %in% flags_country_sea$.summary)){coord_second_clean <- coord_first_clean}
	else{
		# Get problematic records
		problematic_records <- flags_country_sea  %>% filter(.summary == FALSE)

		worldmap_problematic_coordinates <- worldmap +  geom_point(data = problematic_records ,aes(x = decimalLongitude, y = decimalLatitude), color = "darkblue", size = 0.5) +
		 labs(title=paste0(nrow(flagged_coordinates), " problematic occurrences - ",species_name[species_id]," - GBIF - May 2019"), x = "Longitude", y = "Latitude") 
	
		ggsave(plot=worldmap_problematic_coordinates, filename = paste0(main_wd,"/Figures/", output_figures, "02.problematic_coordinates_occurrences_", species_name[species_id], "_occurences_", nrow(flagged_coordinates) ,"_GBIF2019.png"), w = 30, h = 15, units = "cm", dpi = 600 )



		# Get natural earth world country polygons 
		country_shapefile <- rnaturalearth::ne_countries()
		# Transform sp object to sf object for optimisation
		country_shapefile_sf <- st_as_sf(country_shapefile, coordinates(country_shapefile), crs = 4326)
		# Perform series of coordinate tests
		flags_corrected_coordinates <- correct_coordinates_fct(problematic_records, country_shapefile_sf)

		# Discard records for which no match could be found between coordinates and country name. Keep new set of corrdinates for others.
		coord_second_clean <- flags_country_sea %>% filter(.summary == TRUE) %>% select(data_fields) 
		if(TRUE %in% flags_corrected_coordinates$.summary){
			# Extract records for which we could modify the set of coordinates
			corrected_coordinates <- flags_corrected_coordinates %>% filter(.summary == TRUE) %>% mutate(decimalLongitude = correctedLongitude, decimalLatitude = correctedLatitude) %>% select(data_fields)
			print(paste0("Corrected ", nrow(corrected_coordinates), " records."))
			# Merge with unflagged records
			coord_second_clean <- rbind(coord_second_clean, corrected_coordinates )
			}
	}

	####################################################################
	# Step 3: Improving data quality using GBIF meta-data    	   #
	####################################################################
	# Remove records with low coordinate precision
	coord_third_clean <- coord_second_clean %>% filter(coordinateUncertaintyInMeters/1000 <= 100 | is.na(coordinateUncertaintyInMeters))
	print(paste0("Removed ", nrow(coord_second_clean)-nrow(coord_third_clean), " records due to spatial uncertainty."))

	# Remove unsuitable data sources (), especially fossils that are often responsible for the majority of problems 
	coord_fourth_clean <- filter(coord_third_clean, basisOfRecord == "HUMAN_OBSERVATION" | basisOfRecord == "OBSERVATION" | basisOfRecord == "PRESERVED_SPECIMEN")
	print(paste0("Removed ", nrow(coord_third_clean)-nrow(coord_fourth_clean), " records due to unsuitable data sources."))
	
	# Remove records with suspicious individual counts GBIF includes few records of absence (individual count = 0) and suspiciously high occurrence counts, which might indicate inappropriate data or data entry problems.
	# Remove records from before second world war
	 if (rgbif_download) { 	cleaned_coordinates <- coord_fourth_clean %>%  filter(individualCount > 0 | is.na(individualCount)) %>%  filter(individualCount < 99 | is.na(individualCount)) %>%
  				filter(year > 1945) # remove records from before second world war # high counts are not a problem
				print(paste0("Removed ", nrow(coord_fourth_clean)-nrow(cleaned_coordinates), " suspicious records based on data or number of counts.")) }
	else { 	cleaned_coordinates <- coord_fourth_clean %>%  filter(!is.na(year)) %>% filter(year > 1945) # remove records from before second world war # high counts are not a problem
	 	print(paste0("Removed ", nrow(coord_fourth_clean)-nrow(cleaned_coordinates), " suspicious records based on data or number of counts.")) }
		
	worldmap_cleaned_coordinates <- worldmap +  geom_point(data = cleaned_coordinates ,aes(x = decimalLongitude, y = decimalLatitude), color = "darkgreen", size = 0.5) + labs(title=paste0(nrow(cleaned_coordinates), " cleaned occurrences - ",species_name[species_id]," - GBIF - May 2019"), x="Longitude", y="Latitude") 
	
	ggsave(plot=worldmap_cleaned_coordinates, filename = paste0(main_wd,"/Figures/", output_figures, "03.cleaned_coordinates_occurrences_", species_name[species_id], "_occurences_", nrow(cleaned_coordinates) ,"_GBIF2019.png"), w = 30, h = 15, units = "cm", dpi = 600 )
	
	nb_cleaned_occurences <- nrow(cleaned_coordinates)
	# Save cleaned occurences into R objects
	saveRDS( cleaned_coordinates, file = paste0(main_wd, "/Data/cleaned_GBIF_occurences/", species_name[species_id], "_", nb_cleaned_occurences, "_GBIF2019.rds") )
	# Return number of cleaned occurences for each species
	return(nb_cleaned_occurences)
}
stopCluster(cl)			

# Save statistics
occurences_summary$nb_cleaned_occurences <- nb_clean_occurences
data_cleaning_summary <- data.frame(occurences_summary)
data_cleaning_summary$lost_percentage <- occurences_summary$nb_cleaned_occurences*100/ occurences_summary$nb_occurences_downloaded
saveRDS( data_cleaning_summary, file = paste0(main_wd, "/Data/cleaned_GBIF_occurences/01.nb_cleanedoccurences_GBIF2019.rds") )




