####################################################################
# Audrey Lustig. May 2019  					   #
####################################################################
# This script loads GBIF occurrence data for the list of species define in the variable species_names
# It doanloads nb_limit occurences at maximum (default is set at 500)
# It saves the raw data as an rds object in folder main_wd/Data/rgbig_download/
# It save a table with the name of the species and the number of occurences downloaded in folder main_wd/Data/rgbig_download/
####################################################################

rm(list=ls())

####################################################################
# Call global libraries     									   #
####################################################################
library(rgbif)
library(foreach)
library(iterators)
library(doParallel)

####################################################################
# Set directories								   				   #
####################################################################
# Set working directory
main_wd <- "/home/audrey/Documents/UC/species-distributions-and-networks/AL/Code"
setwd(main_wd)

# Create output directory
if (!dir.exists(paste0(main_wd,"/Data/rgbif_download/"))){ dir.create(paste0(main_wd,"/Data/rgbig_download/")) }

####################################################################
# Download occurence records from GBIF    	 					   #
####################################################################
# List of species
species_name <- c("Apis mellifera", "Astilbe thunbergii", "Bombus lapidarius", "Bombus pascuorum", "Bombus terrestris", "Cirsium arvense")
# Number of raw occurences to download
nb_limit = 100000 # this is a very high value because some of your specis have a lot of records. Would recommend to go down to 10000 records to test the code in the first place. 

# Number of species
nb_species <- length(species_name)

# Initiate a data frame to keep track of the number of occurences used for the analysis
data_cleaning_summary <- NULL
data_cleaning_summary$species_name  <- species_name

# Select GBIF fields of interest
data_fields <- c('key', 'scientificName', 'decimalLatitude', 'decimalLongitude', 'geodeticDatum', 'countryCode', 'country', 'individualCount', 'coordinateUncertaintyInMeters', 'year', 'basisOfRecord')

# Define cluster type and number of cores
cl = makeCluster(3, outfile='', type='FORK')
# Register cluster
registerDoParallel(cl)

# export vairables to cluster
clusterExport(cl, c('main_wd', 'species_name', 'data_fields', 'nb_limit')) 

nb_occurences_downloaded <- foreach(species_id = icount(nb_species), .combine = c)  %dopar% {	
	# The function taxonKey from rgbif is optimized for speed, and gives back suggested taxon key based on query parameters	
	key <- suppressMessages(name_suggest(q=species_name[species_id], rank='species')$key[1])
	#obtain data from GBIF via rgbif
	# By default occ_search() returns a dplyr like output summary in which the data printed expands based on how much data is returned. You can search by scientific name or taxon key.
	# Like many functions in rgbif, you can choose what to return with the return parameter, here, just returning the data (no metadata)
	# You can choose what fields to return. This isn’t passed on to the API query to GBIF as they don’t allow that, but rgbif filters out the columns before printing the data.
	# by default occ_search return a maximum of 500 data. You need to increase the limit to ensure all data are downloaded.
	# I have set the maximum to 10000 but some of your species have more than about 530,000 records.
	species_occurence_data <- occ_search(taxonKey=key, return='data', fields=data_fields, limit= nb_limit) 
	# number of occuremce downloded 
	nb_occurences_downloaded <- nrow(species_occurence_data)
	
	saveRDS( species_occurence_data, file = paste0(main_wd, "/Data/rgbig_download/.", species_name[species_id], "_rgbif_rawdata_", nb_occurences_downloaded, "_may2019.rds") )
	return(nb_occurences_downloaded)
}
stopCluster(cl)	

data_cleaning_summary$nb_occurences_downloaded  <- nb_occurences_downloaded

data_cleaning_summary <- data.frame(data_cleaning_summary)
saveRDS( data_cleaning_summary, file = paste0(main_wd, "/Data/rgbig_download/00.nbrawoccurences_rgbifdownload_may2019_limits", nb_limit, ".rds") )


