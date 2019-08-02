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
  # Arguments:
  # ---
  # species_name: Species name provided
  # distributions: Dataframe where species_name comes from
  #
  crossIcon <- makeIcon(iconUrl = "Docs/Cross Marker.png", 12, 12, iconAnchorX = 6, iconAnchorY = 6)
  
  leaflet(data = distributions[[species_name]]) %>%
    addTiles() %>%
    addMarkers(lng = ~longitude, lat = ~latitude, icon = crossIcon)
}

# Used CircleMarkers before changing to CrossMarkers
# addCircleMarkers(lat = ~latitude, lng = ~longitude, radius = 3, stroke = FALSE, 
  # color = "gray13", fillOpacity = 0.8)

find_min_max <- function(species_name, distributions) {
  # 
  # Find the Max and Min lats and longs to create bounding boxes
  #
  # Argument:
  # ---
  # species_name: Species name provided
  # distributions: Dataframe where species_name comes from
  #
  list(
  min_lat = min(distributions[[species_name]][,3]),
  max_lat = max(distributions[[species_name]][,3]),
  min_lng = min(distributions[[species_name]][,4]),
  max_lng = max(distributions[[species_name]][,4]))  
}

create_final_map <- function(species_name, distributions, MPA, pos) {
  # 
  # Find the Max and Min lats and longs to create bounding boxes
  #
  # Argument:
  # ---
  # species_name: Species name provided
  # distributions: Dataframe where species_name comes from
  # MPA: MPA data provided
  #
  
  #finding the min and max latitudes -> used in creating map bounding box
  bounds <- find_min_max(species_name, distributions)
  
  #create cross Marker
  crossIcon <- makeIcon(iconUrl = "Docs/Cross Marker.png", 12, 12, iconAnchorX = 6, iconAnchorY = 6)
  
  #create map
  leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 5)) %>% 
  addTiles() %>%
  addMarkers(data = distributions[[species_name]], lng = ~longitude, lat = ~latitude, 
             icon = crossIcon) %>%
  addPolygons(data = MPA, color = "gray", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5, fillColor = "tomato") %>%
  addMiniMap(position = pos, width = 100, height = 100) %>%
  setMaxBounds(lng1 = bounds$min_lng, lat1 = bounds$min_lat, lng2 = bounds$max_lng, lat2 = bounds$max_lat)
}

