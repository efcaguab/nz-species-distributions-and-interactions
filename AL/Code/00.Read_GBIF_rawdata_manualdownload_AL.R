####################################################################
# Audrey Lustig. May 2019  					   #
####################################################################
# This script loads GBIF occurrence manually downloaded for the list of species define in the variable species_names
# Name of ths csv file should be [species_name]_GBIF_rawdata_may2019.csv where species name is the scientific name of your species, first letter upper case, no space.
# It saves the raw data as an rds object in folder main_wd/Data/GBIFmanualDownload/
# It save a table with the name of the species and the number of occurences downloaded in folder main_wd/Data/GBIFmanualDownload/
####################################################################

rm(list=ls())

####################################################################
# Call global libraries     					   #
####################################################################
library(tidyverse)
library(foreach)
library(iterators)
library(doParallel)

####################################################################
# Set directories						   #
####################################################################
# Set working directory
main_wd <- "/home/audrey/Documents/UC/species-distributions-and-networks/AL/Code"
setwd(main_wd)


####################################################################
# Read occurence records manually downloaded from GBIF 	 	   #
####################################################################
# List of species
species_name <- c("Apismellifera", "Astilbethunbergii", "Bombuslapidarius", "Bombuspascuorum", "Bombusterrestris", "Cirsiumarvense")

# Number of species
nb_species <- length(species_name)

# Initiate a data frame to keep track of the number of occurences used for the analysis
data_cleaning_summary <- NULL
data_cleaning_summary$species_name  <- species_name

# Select GBIF fields of interest
data_fields <- c('taxonKey', 'scientificName', 'decimalLatitude', 'decimalLongitude', 'countryCode', 'coordinateUncertaintyInMeters', 'year', 'basisOfRecord')

# Define cluster type and number of cores
cl = makeCluster(3, outfile='', type='FORK')
# Register cluster
registerDoParallel(cl)

# export vairables to cluster
clusterExport(cl, c('main_wd', 'species_name', 'data_fields')) 

nb_occurences_downloaded <- foreach(species_id = icount(nb_species), .combine = c)  %dopar% {	
	# read csv file downloaded manually from GBIF
	species_occurence_rawdata <- read.csv(file=paste0(main_wd, "/Data/GBIFmanualDownload/",species_name[species_id],"_GBIF_rawdata_may2019.csv" ), header=TRUE, sep="\t")
	# select field of interest
	species_occurence_data <- species_occurence_rawdata %>% select(data_fields)
	# number of occurences downloded 
	nb_occurences_downloaded <- nrow(species_occurence_data)
	# save downloaded data as rds object
	saveRDS( species_occurence_data, file = paste0(main_wd, "/Data/GBIFmanualDownload/", species_name[species_id], "_manualdownload_rawdata_", nb_occurences_downloaded, "_may2019.rds") )
	return(nb_occurences_downloaded)
}
stopCluster(cl)	

data_cleaning_summary$nb_occurences_downloaded  <- nb_occurences_downloaded


####################################################################
# Save metadata						 	   #
####################################################################
data_cleaning_summary <- data.frame(data_cleaning_summary)
saveRDS( data_cleaning_summary, file = paste0(main_wd, "/Data/GBIFmanualDownload/00.nbrawoccurences_manualdownload_may2019.rds") )

