####
##  Functions used in Shark Distribution.Rmd
##  
##  author: Max Schwarz
##  date: 12th July 2019
####

import_data <- function(species_name) {
  #
  # Import data from GBIF for a species
  #
  # Argument:
  # ---
  # species_name: Species name provided
  #
  species1 <- name_suggest(q = species_name) # find the 'key' of the species
  
  # using the key, find all data with the occ_search function
  
  species.occ <- occ_search(taxonKey = species1[1,1], limit = 21000, return = 'data', 
                            hasCoordinate = TRUE)
  
  # use the species.occ object and change some column names (mutate) 
  # and then create a tibble based on selected columns
  
  species.occ %>%
    mutate(species_name = species1$canonicalName[1], key = taxonKey) %>%
    dplyr::select(species_name, key, latitude = decimalLatitude, longitude = decimalLongitude) %>%
    as_tibble()
}

create_leaflet_map <- function(species_name, distributions) {
  # 
  # Create a map for each species
  #
  # Argument:
  # ---
  # species_name: Species name provided
  #
  leaflet(data = distributions[[species_name]]) %>%
    addTiles() %>%
    addCircleMarkers(lat = ~latitude, lng = ~longitude, radius = 6, stroke = FALSE, 
                     color = "orange", fillOpacity = 0.5)
  }



create_leaflet_map <- function(species_name, distributions) {
  # 
  # Create a map for each species
  #
  # Argument:
  # ---
  # species_name: Species name provided
  #
  leaflet(data = distributions[[species_name]][["."]]) %>%
    addTiles() %>%
    addCircleMarkers(lat = ~latitude, lng = ~longitude, radius = 6, stroke = FALSE, 
                     color = "orange", fillOpacity = 0.5)
}
