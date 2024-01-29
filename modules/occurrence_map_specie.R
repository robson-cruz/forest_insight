#' This function receives a table containing scientific names of trees and
#' returns an occurrence map for each species.
#' 
 
library(rgbif)
library(purrr)
library(dplyr, warn.conflicts = FALSE)
library(leaflet)
library(htmlwidgets)
library(webshot)


occurrence_map <- function(file_path) {
        
        data_input <- unique(file_path[, "nome_cientifico"])
        map_lst <- list()
        
        # Get specie data occurrence from "rgbif" package
        sp_loc <- occ_data(
                scientificName = data_input,
                hasCoordinate = TRUE,
                kingdomKey = 6,
                continent = "south_america"
        ) %>%
                map_df(.f = as.data.frame) %>%
                filter(data.taxonomicStatus == "ACCEPTED") %>%
                filter(!is.na(data.decimalLongitude), !is.na(data.decimalLatitude))
        
        # Iterate over each unique species in the occurrence data
        for (sp in unique(sp_loc$data.species)) {
                # Crete directory path based on the specie names
                map_dir <- paste0("./output/warnings/mapa_ocorrencia_", sp, "_.png")
                # Create a map for each specie
                map_lst[[sp]] <- sp_loc[sp_loc$data.species == sp, ] %>%
                        leaflet() %>%
                        addProviderTiles(provider = "OpenStreetMap", group = "OpenStreetMap") %>%
                        addCircles(
                                group = "Registro", 
                                lng = ~data.decimalLongitude, lat = ~data.decimalLatitude,
                        ) %>%
                        addMiniMap(toggleDisplay = TRUE) %>%
                        fitBounds(
                                lng1 = -74.178383,
                                lat1 = -34.729993,
                                lng2 = -34.729993,
                                lat2 = 5.271393
                        ) %>%
                        leafem::addLogo(
                                img = "https://upload.wikimedia.org/wikipedia/commons/8/81/Logo_IBAMA.svg", 
                                url = "http://www.ibama.gov.br/",
                                position = "bottomleft",
                                width = 100,
                                height = 50
                        )
                
                # save maps
                saveWidget(
                        widget = map_lst[[sp]],
                        file = paste0(map_dir, ".html"),
                        selfcontained = FALSE
                )
                
                webshot::webshot(
                        url = paste0(map_dir, ".html"),
                        file = paste0(map_dir, ".png")
                )
                
                # Remove the HTML files
                file.remove(paste0(map_dir, ".html"))
                
                # Remove the directories created by the saveWidget function
                for (specie in unique(sp_loc$data.species)) {
                        map_dir <- paste0("./output/warnings/mapa_ocorrencia_",
                                          specie, "_.png_files")
                        unlink(map_dir, recursive = TRUE)
                }
        }
}
