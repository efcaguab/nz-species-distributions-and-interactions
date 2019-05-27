
extract_climatic_background_fct <- function(eco_select, species_climond_occurences_sp, analysis_type, main_wd, species_name){

	if (! analysis_type %in% c('country', 'continent', 'ecoregion', 'major_habitat_types', 'biogeographic_realms') ) {stop( 'Error with analysis_type. Should be a string character. Options are: country, continent, ecoregion, major_habitat_types.') }

	# Extract polygons (boundaries) of countries, continent, ecoregion or major_habitat_type for species location
	if (analysis_type == 'country') {
		ref_bg <- as.character(species_climond_occurences_sp$countryCode)
		eco_select_polygon_bg <- eco_select  %>% filter(as.character(country_ecoselect) %in% ref_bg)
	}
	if (analysis_type == 'continent') {
		ref_bg <- as.character(species_climond_occurences_sp$continent)
		eco_select_polygon_bg <- eco_select  %>% filter(as.character(continent) %in% ref_bg)
	}
	if (analysis_type == 'ecoregion') {
		ref_bg <- as.character(species_climond_occurences_sp$ecoregion)
		eco_select_polygon_bg <- eco_select  %>% filter(as.character(ecoregion) %in% ref_bg)
	}
	if (analysis_type == 'major_habitat_types') {
		ref_bg <- as.character(species_climond_occurences_sp$major_habitat_types)
		eco_select_polygon_bg <- eco_select  %>% filter(as.character(major_habitat_types) %in% ref_bg)
	}
	if (analysis_type == 'biogeographic_realms') {
		ref_bg <- as.character(species_climond_occurences_sp$biogeographic_realms)
		eco_select_polygon_bg <- eco_select  %>% filter(as.character(biogeographic_realms) %in% ref_bg)
	}

	# Extract all climate data available for the specified polygons
	# The mask function create a new Raster* object that has the same values as climond_layers_stack, except for the cells that are NA (or other maskvalue) in a 'mask'. 
	mask_climatic_bg <- raster::mask(climond_layers_stack, eco_select_polygon_bg)

	# Extract raster cell ID with presence points
	climatic_cell_with_records <- raster::extract(mask_climatic_bg, species_climond_occurences_sp, cellnumbers=TRUE)[,"cells"]
	# Create a new column indicating if a species is present or not. 
	mask_climatic_bg$records <- 0
	mask_climatic_bg$records[climatic_cell_with_records] <- 1

	# Save climatic background
	saveRDS( mask_climatic_bg, file = paste0(main_wd, "/Data/Climatic_background/", species_name, "_", analysis_type, "_climatic_bg_may2019.rds") )

}
