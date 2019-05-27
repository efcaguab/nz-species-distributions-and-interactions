####################################################################
# Audrey Lustig. May 2019  					   #
####################################################################
# This script :
# - Loads cleaned GBIF dataset extracted from 01.GBIF_data_cleaning_May2019_AL.R
# - Overlay bioclimatic data with each occurence records (bioclim dataset - https://www.climond.org/ClimateData.aspx)
# - Reduces the spatial autocorrelation in the occurrence points 
# - Overlay ecoregion profile data with each occurence records (Terrestrial Ecoregions dataset - http://maps.tnc.org/gis_data.html)
# - Extract climatic background (fundamental climatic niche) at different spatial scale (country, continent, ecoregion, major habitat types)
####################################################################

rm(list=ls())
####################################################################
# Call global libraries     					   #
####################################################################
library(sf) # Support for simple features, a standardized way to encode spatial vector data.
library(raster) #  Reading, writing, manipulating, analyzing and modeling of gridded spatial data.
library(tidyverse) # Data wrangling
library(viridis)  # Better colors for everyone
# Pitfall: name clashes
# Just loading the tidyverse reveals a pitfall of using spatial data with the tidyverse that affects the raster package
# The final x shows that dplyr’s select() function has boshed (technically speaking, masked) raster’s select function. 
# This can cause issues. To avoid this pitfall I suggest using dplyr::select() and raster::select() rather than just select() when using this conflicted function name if you use raster and the tidyverse.
library(countrycode) # Standardize country names, convert them into one of eleven coding schemes, convert between coding schemes, and assign region descriptors - https://github.com/vincentarelbundock/countrycode
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

# Create output directory
if (!dir.exists(paste0(main_wd,"/Figures/Climatic_background/"))){ dir.create(paste0(main_wd,"/Figures/Climatic_background/")) }
if (!dir.exists(paste0(main_wd,"/Data/Climatic_background/"))){ dir.create(paste0(main_wd,"/Data/Climatic_background/")) }

# Set function directory
functions_wd <- paste0(main_wd, "/Code/Functions/")
sourceDir(path = paste0(functions_wd))

####################################################################
# Load Biomes - ecoregions shapefile				   #
####################################################################
# Load shapefile of biomes/ecoregions/realms/countries/continents
# The Nature Conservancy, Terrestrial Ecoregions, Major Habitat Types, Biogeographic Realms, TNC, World, Global
# Developed originally by Olson, D. M. and E. Dinerstein (2002), Bailey (1995) and Environment Canada (Wiken, 1986), these data layers were modified by The Nature Conservancy (TNC) to be used in its Biodiversity Planning exercises in the process known as Ecoregional Assessments.
ecoregion_shapefile <- st_read(paste(main_wd, "/Data/Biomes/ecoregion_portofolio.shp", sep = ""))
eco_select <- ecoregion_shapefile %>% select(ISO_CC, CONTINENT, ECO_NAME, WWF_MHTNAM, WWF_REALM2) %>% 
		transmute('country_ecoselect'= ISO_CC, 'continent'= CONTINENT, 'ecoregion' = ECO_NAME, 'major_habitat_types' = WWF_MHTNAM, 'biogeographic_realms'= WWF_REALM2) %>% 
		mutate('country_ecoselect'= countrycode(country_ecoselect, origin =  'iso2c', destination = 'iso3c'))


####################################################################
# Load Bioclim dataset 						   #
####################################################################
# Read climond dataset (bioclim dataset - https://www.climond.org/ClimateData.aspx)
# The Bioclim variables are the core covariates used in correlative species distribution modelling. Collectively, they represent a statistical summary of temperature, precipitation, radiation and soil moisture. Recently, the first five principal components of the initial 35 variables were added as Bioclim variables (Bio36-Bio40). The Bioclim variables are available in both ASCII and ESRI grid format, suitable for use in most popular correlative species distribution modelling packages.
# Details of their derivation can be found on the Bioclim Registry page - https://www.climond.org/BioclimRegistry.aspx#Table1
# Release date: 09th Sep 2014, 10' resolution
# Reading multiple ratser
dir <- Sys.glob(paste(main_wd,"/Data/Climond_CM10_1975H_Bio_ESRI_V1.2/CM10_1975H_Bio_V1.2/bio*/hdr.adf",sep=''))

# Select the climatic variables to use - i.e. removed Bio36-Bio40
nb_climatic_variables = 35
dir <- dir[1: nb_climatic_variables]

# Transform data into raster
bio <- lapply(dir, raster) 
# Concatenates multiple vectors contained in bio into a single vector along with a factor indicating where each observation originated.
climond_layers_stack <- stack(bio) 

# With the raster package you also use R to access the data.
# I like to have them downloaded to make sure I have most updated data set 
# climond_layers_stack <- getData("worldclim",var="bio",res=10)


####################################################################
# Load cleaned occurences metadata				   #
####################################################################
occurences_summary <- readRDS( file = paste0(main_wd, "/Data/cleaned_GBIF_occurences/01.nb_cleanedoccurences_GBIF2019.rds")  )
species_name <- as.character(occurences_summary$species_name) #sapply(occurences_downloaded$species_name, function(x) str_replace_all(as.character(x), fixed(" "), "_"))
nb_cleaned_occurences <- occurences_summary$nb_cleaned_occurences
nb_species <- nrow(occurences_summary) 

# Define cluster type and number of cores
cl = makeCluster(6, outfile='', type='FORK')
# Register cluster
registerDoParallel(cl)

# export vairables to cluster
clusterExport(cl, c('main_wd', 'species_name', 'nb_cleaned_occurences', 'nb_species', 'climond_layers_stack', 'eco_select')) 

 
#for( species_id in seq(1, nb_species) ) {
nb_final_occurence_records <- foreach(species_id = icount(nb_species), .combine = c)  %dopar% {

	####################################################################
	# Extract ecoregions/biomes for each occurence records             #
	####################################################################
	# Load cleaned occurences
	cleaned_coordinates <- readRDS( paste0(main_wd, "/Data/cleaned_GBIF_occurences/", species_name[species_id], "_", nb_cleaned_occurences[species_id], "_GBIF2019.rds") )

	# Transform dataframe to spatial points
	species_climond_occurences_sp <- st_as_sf(cleaned_coordinates, coords = c("decimalLongitude" ,  "decimalLatitude"), crs = 4326) 

	cat("Overlaying occurences records with Biomes - ecoregions shapefile. \n") 
	# Overlay occurence points with realm shapefile						
	intersected_eco_select <- suppressMessages(st_join(species_climond_occurences_sp, eco_select , join = st_intersects))

	# A common issue is that the boundaries of the biomes shapefile do not necessarily match the terrestrial boundaries of GBIF shapefile
	# Remove records for which no ecoregions data where found
	intersected_eco_select <- intersected_eco_select %>% filter( !is.na (ecoregion))
	cat( paste0("Removed ", nrow(cleaned_coordinates)-nrow(intersected_eco_select), " occurences out of ", nrow(cleaned_coordinates)," for which no ecoregion where documented. \n")) 

	# Save selected cleaned records
	records_selected <- data.frame(st_coordinates(intersected_eco_select), intersected_eco_select) %>% select (-geometry, -country_ecoselect) %>% rename("decimalLongitude" = 'X', "decimalLatitude" = 'Y')
	
	####################################################################
	# Extract climatic variables for each presence points     	   #
	####################################################################
	# Extract climatic variables for presence points
	dist_climond <- raster::extract(climond_layers_stack, records_selected[ , c("decimalLongitude" ,  "decimalLatitude")], df = T)

	# Merge climatic variables with each data points
	dist_clim <- data.frame(dist_climond %>% select(-ID), records_selected)

	####################################################################
	# Remove spatial autocorrelation 			     	   #
	####################################################################
	# it is widely acknowledged that spatial autocorrelation arises in ecological data because nearby records tend to be more similar in physical/climatic characteristics, and/or species occurrences or
	# abundances, than are pairs locations that are further apart. Failure to account for such spatial dependence in occurrence data for SDMs can lead to misidentification of important abiotic or biotic
	# variables driving species distributions. To overcome this issue, it is recommended to downscale the species occurrence data such that only one occurrence point per grid cell in the environmental data layer is maintained.
	# Extract coordinates values
	presence_points_ccordinates <- dist_clim %>% select(c("decimalLongitude" ,  "decimalLatitude"))
	# Get cell numbers of a Raster 
	cat("Overlaying occurences records with Bioclim raster. \n") 
	raster_cell_number <- raster::cellFromXY(climond_layers_stack, presence_points_ccordinates)
	# Select only distict raster cell number
	unique_raster_points <- cbind(dist_clim, raster_cell_number) %>% distinct(raster_cell_number, .keep_all = TRUE)
	cat( paste0( "Kept ", nrow(unique_raster_points), " out of ", nrow(dist_clim)," after 'removing' spatial autocorrelation. \n") )

	####################################################################
	# Clean erroneous data						   #
	####################################################################
	# Again a common issue is that the boundaries of the GBIF shapefile do nto necessarily match the raster boundaries of the bioclim dataset (lost of precision when climatic variables are rasterized)
	# It possible that some of the records will have no climatic variables documented
	# Two options here.
	# 1. Remove the data from the analysis
	# 2. Search the closest terrestrial polygons using a buffer search zone in the units of the current projection;

	# 1. Remove the data from the analysis
	dist_clim_na_omit <- unique_raster_points[ complete.cases(unique_raster_points[, 1:nb_climatic_variables]), ]
	cat( paste0( "Found no climatic variables for ", nrow(unique_raster_points)-nrow(dist_clim_na_omit), " occurences out of ", nrow(unique_raster_points)," unique points per raster cell. \n") )

	# 2. Search the closest terrestrial polygons using a buffer search zone 
	# We set this distance to 10000 which correspond to average size of a environemntal layer grid cell. 
	# This function can be slow ! 
	#dist_clim_buffer <- extract_bioclimdata_withbuffer_fct(unique_raster_points, climond_layers_stack, buffer_size = 10000) 
	#print( paste0( "Could find climatic variables for ", nrow(dist_clim)-nrow(dist_clim_buffer), " occurences out of ", nrow(dist_clim)-nrow(dist_clim_na_omit)," using a buffer search.") )

	species_climond_occurences <- dist_clim_na_omit 

	# Final number of occurences selected
	nb_occurences_bioclim_ecoregion_intersected <- nrow(species_climond_occurences)
	# Save cleaned occurences into R objects
	saveRDS( species_climond_occurences, file = paste0(main_wd, "/Data/cleaned_GBIF_occurences/", species_name[species_id], "_records_with_bioclim_ecoregion_intersected.rds") )

	####################################################################
	# Plot data for vizualisation					   #
	####################################################################
	cat("Plotting world mean temperatures and selected species records. \n") 
	meantemperature_spdf <-  as(climond_layers_stack$hdr.1, "SpatialPixelsDataFrame")
	meantemperature_df <- as.data.frame(meantemperature_spdf)
	colnames(meantemperature_df) <- c("value", "x", "y")
	worldmap_temperature <- ggplot() +   geom_tile(data=meantemperature_df, aes(x=x, y=y, fill=value), alpha=0.8) + theme_bw() + coord_fixed()  +
			labs(title = paste0(species_name[species_id], "- curated dataset - GBIF - May 2019"), x = "Longitude", y = "Latitude", fill = "Annual mean temperature (C) " ) +
			scale_fill_viridis_c() +
			geom_point(data = species_climond_occurences ,aes(x = decimalLongitude, y = decimalLatitude), color = "black", size = 0.3) + 
			theme( 
				plot.background = element_rect(fill = NA, color = NA), 
				panel.background = element_rect(fill = NA, color = NA), 
				legend.background = element_rect(fill = NA, color = NA),
				plot.title = element_text(size= 14, color = '#363636', face ="bold", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
				plot.caption = element_text( size=12, color = "#4e4d47"),
				axis.title = element_text(size= 12, color = '#363636', face ="bold"),
				axis.text = element_text(size= 12, color = '#363636', face ="bold"),
				legend.title = element_text(size= 12, color = '#363636', face ="bold"),
				axis.line = element_blank(),
		       		legend.position = "bottom")
	
	ggsave(plot=worldmap_temperature, filename = paste0(main_wd,"/Figures/Climatic_background/03.currated_data_", species_name[species_id], "_GBIF2019.png"), w = 30, h = 15, units = "cm", dpi = 600 )

	 
	####################################################################
	# Extract climatic background for niche analysis		   #
	####################################################################
	# Transform occurences points dataframe to spatial points
	species_climond_occurences_sp <- st_as_sf(species_climond_occurences, coords = c("decimalLongitude" ,  "decimalLatitude"), crs = 4326) 

	# Warning this function might takes a while
	for (analysis_type in c('country', 'continent', 'ecoregion', 'major_habitat_types')) {
		cat( 'Extracting climatic background at', analysis_type, 'level.\n' )
		extract_climatic_background_fct(eco_select, species_climond_occurences_sp, analysis_type, main_wd, species_name[species_id])
	}

	return(nb_occurences_bioclim_ecoregion_intersected )
}
stopCluster(cl)	
		

# Save statistics
occurences_summary$nb_occurences_bioclim_ecoregion_intersected <- nb_final_occurence_records 
data_cleaning_summary <- data.frame(occurences_summary)
saveRDS( data_cleaning_summary, file = paste0(main_wd, "/Data/cleaned_GBIF_occurences/02.nb_occurences_bioclim_ecoregion_intersected_GBIF2019.rds") )
























