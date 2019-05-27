
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
library(ade4)  # PCA computation
library(ecospat)
# Pitfall: name clashes
# Just loading the tidyverse reveals a pitfall of using spatial data with the tidyverse that affects the raster package
# The final x shows that dplyr’s select() function has boshed (technically speaking, masked) raster’s select function. 
# This can cause issues. To avoid this pitfall I suggest using dplyr::select() and raster::select() rather than just select() when using this conflicted function name if you use raster and the tidyverse.
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
if (!dir.exists(paste0(main_wd,"/Figures/niche_analysis/"))){ dir.create(paste0(main_wd,"/Figures/niche_analysis/")) }
if (!dir.exists(paste0(main_wd,"/Data/niche_analysis/"))){ dir.create(paste0(main_wd,"/Data/niche_analysis/")) }

# Set function directory
functions_wd <- paste0(main_wd, "/Code/Functions/")
sourceDir(path = paste0(functions_wd))


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

# Transfrom spatial raster to data frame
climond_df <- cbind(coordinates(climond_layers_stack), as.data.frame(climond_layers_stack)) %>% rename('decimalLongitude' = 'x', 'decimalLatitude' = 'y')


####################################################################
# Load cleaned occurences metadata				   #
####################################################################
occurences_summary <- readRDS( file = paste0(main_wd, "/Data/cleaned_GBIF_occurences/02.nb_occurences_bioclim_ecoregion_intersected_GBIF2019.rds") )
species_name <- as.character(occurences_summary$species_name) #sapply(occurences_downloaded$species_name, function(x) str_replace_all(as.character(x), fixed(" "), "_"))
nb_species <- nrow(occurences_summary) 


# Define cluster type and number of cores
cl = makeCluster(6, outfile='', type='FORK')
# Register cluster
registerDoParallel(cl)
# export vairables to cluster
clusterExport(cl, c('main_wd', 'species_name', 'nb_species', 'climond_df','climond_layers_stack')) 


foreach(species_id = icount(nb_species))  %dopar% {
#for (species_id in seq(1:nb_species)){
	# Extract species occurrences and associated climatic variables
	#climatic_sp <- readRDS( file = paste0(main_wd, "/Data/cleaned_GBIF_occurences/", species_name[species_id], "_records_with_bioclim_ecoregion_intersected.rds"))

	# Transform data frame to spatial point
	#climatic_sp_spatialpoints <- st_as_sf(climatic_sp, coords = c("X" ,  "Y"), crs = 4326) 

	for (type_analysis in c('country', 'continent', 'ecoregion')){
		####################################################################
		# Load cliamatic variables					   #
		####################################################################
		cat('Niche analysis', type_analysis, 'level.\n')
		# Extract climatic data for the whole climatic background 
		climatic_bg <- readRDS( file = paste0(main_wd, "/Data/Climatic_background/", species_name[species_id], "_", type_analysis, "_climatic_bg_may2019.rds") )
		# Transform spatial raster into data frame 
		# A dataframe of the environmental values (in column) for background pixels of the whole study area or climatic background (in row).R

		data_env_bg <- as.data.frame(climatic_bg) %>% drop_na()
		data_env_occ <- data_env_bg %>% filter(records == 1)  
		################################################################# Note to Audrey: why some occurences are stilled not 
		cat('Calibrating PCA. \n')
		# Performs a principal component analysis of a data frame and returns the results as objects of class pca and dudi. 
		# Calibrating the PCA in the whole climatic background
		pca_env <- dudi.pca(data_env_bg  %>% select(starts_with("hdr.")) , center = T, scale = T, scannf = F, nf = 2)

		# Plot variables contribution
		#s.corcircle(pca.env$co)

		# PCA scores for the whole climatic background
		# Fernando: this is where I think in your case you need to project your niche into the world space
		scores_globclim <- pca_env$li

		# PCA scores for the species occurence records
		# Projecting climate variables for species occurence onto the PCA space
		scores_sp <- suprow(pca_env, data_env_occ  %>% select(starts_with("hdr.")))$li

		# Using the scores of an ordination (or SDM prediction), create a grid z of RxR pixels with occurrence densities. 
		# I used the ecospat R package - https://www.rdocumentation.org/packages/ecospat/versions/3.0. Collection of R functions and data sets for the support of spatial ecology analyses with a focus on pre, core and post modelling analyses of species distribution, niche quantification and community assembly. 
		# th.sp is the quantile of the distribution of species density at occurrence sites. For example, if th.sp is set to 0.05, the the species niche is drawn by including 95 percent of the species occurrences, removing the more marginal populations. 
		# Similarly, th.env is the quantile of the distribution of the environmental density at all sites of the study area. If th.env is set to 0.05, the delineation of the study area in the environmental space includes 95 percent of the study area, removing the more marginal sites of the study area. By default, these thresholds are set to 0 but can be modified, depending on the importance of some marginal sites in the delineation of the species niche and/or the study area in the environmnental space. It is recommended to check if the shape of the delineated niche and study area corresponds to the shape of the plot of the PCA scores (or any other ordination techniques used to set the environmental space).
		grid_clim <- ecospat.grid.clim.dyn(glob = scores_globclim, glob1 = scores_globclim, sp = scores_sp, R = 100, th.sp = 0)
		cat('Plotting niche space.\n')
		# Visualisation of the gridded environmental space 
		# quant: The quantile of the environmental density used to remove marginal climates.
		# Note to Audrey: change this function so that we can use ggplot instead of traditionnal plot! 
		png(filename = paste0(main_wd,"/Figures/niche_analysis/04.niche_analysis", species_name[species_id], "_", type_analysis, "_GBIF2019.png"), width = 2400, height = 2400, units = "px", res=250)
			ecospat_plot_niche_dyn_fct(grid_clim, quant = 0.15, title = paste0("Niche analysis - ", species_name[species_id], " - ", type_analysis, " background"), name.axis1 = "PC1", name.axis2 = "PC2", colinter = "#0000FF50", colZ1 = "blue")
			legend(min(pca_env$li[,1])+1, max(pca_env$li[,2])-1 ,  fill = c("#0000FF50",0,0), lty = c(0,1,2), c('Realised niche', 'Fundamental niche extent', 'Fundamental niche without marginal envrionments'),  col = c("#0000FF50","blue","blue"), merge = T, border=rep('white',7))
			# You can check if presence points fall into the estimated environmental density
			# points(scores_sp[, c("Axis1")], scores_sp[, c("Axis2")], col="black", pch=16)
			# points(scores_globclim[, c("Axis1")], scores_globclim[, c("Axis2")], col="black", pch=16) 
		dev.off()

		# Extract distribution of the environmental density at all sites of the study area
		#env_density <- t(as.matrix(grid_clim$z.uncor))[ , nrow(as.matrix(grid_clim$z.uncor)):1]
		
		# Projecting world bioclimn variables onto the PCA space - might take a while
		scores_climond <- suprow(pca_env, climond_df  %>% select(starts_with("hdr.")))$li

		# Extract distribution for all climatic raster cells
		world_env_dens <- unlist( sapply(1:nrow(scores_climond), function(index) ifelse( is.na( scores_climond[ index, 'Axis1' ] ), NA, raster::extract(grid_clim$z.uncor, scores_climond[index, ]))) )

		# Add environmental values to climond raster
		climond_layers_stack$world_env_dens <- world_env_dens

		cat("Plotting world mean temperatures and selected species records. \n") 
		world_env_dens_spdf <-  as(climond_layers_stack$world_env_dens, "SpatialPixelsDataFrame")
		world_env_dens_df <- as.data.frame(world_env_dens_spdf )
		colnames(world_env_dens_df) <- c("value", "x", "y")
		worldmap_env_dens <- ggplot() +   geom_tile(data=world_env_dens_df, aes(x=x, y=y, fill=value), alpha=0.8) + theme_bw() + coord_fixed()  +
			labs(title = paste0(species_name[species_id], "- envrionemntal density - ", type_analysis, "."), x = "Longitude", y = "Latitude", fill = "Environmental density" ) +
			scale_fill_viridis_c()
	
		ggsave(plot = worldmap_env_dens, filename = paste0(main_wd,"/Figures/niche_analysis/Environemntal_density_", species_name[species_id], "_", type_analysis, ".png"), w = 30, h = 15, units = "cm", dpi = 600 )
	}
}
stopCluster(cl)	
