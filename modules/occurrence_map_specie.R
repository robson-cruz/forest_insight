#' @name occurrence_species
#' 
#' @title Generate Maps for Species Occurrences
#'
#' @description This function generates maps showing the occurrences of different species in various states of Brazil,
#' excluding occurrences in the Amazon region.
#' The data occurrence of species come from the REFLORA database.
#' The maps are saved as PNG files in a specified output directory.
#'
#' @param dataframe A data frame containing the species occurrence data.
#' It must contain the columns:
#'  `nome_cientifico` (scientific name of the species),
#'  `estado` (state abbreviations),
#'  `ocor_amz` (boolean indicating if the species occurs in the Amazon).
#'
#' @return This function does not return any value. It creates and saves maps in the output directory.
#'
#' @details The function reads a shapefile of Brazilian states, filters and processes the occurrence data,
#' and generates maps for each species showing the states where it occurs. The output maps are saved in the directory `output/Pendencias/`.
#'
#' @import sf
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#'
#'#'#'@references
#' For information about the REFLORA database, see:
#' \url{https://reflora.jbrj.gov.br/reflora/listaBrasil/ConsultaPublicaUC/ResultadoDaConsultaNovaConsulta.do#CondicaoTaxonCP}
#' 
#' @examples
#' \dontrun{
#' # Example usage:
#' # Load your data frame (replace with your actual data loading code)
#' df <- read.csv("path/to/your/data.csv")
#'
#' # Call the function
#' occurrence_species(df)
#' }
#'
#' @export
#' 
library(sf)
library(dplyr)
library(ggplot2)
library(tidyr)


occurrence_species <- function(dataframe) {
    # Check directory
    map_dir <- "output/Pendencias/"
    if (!dir.exists(map_dir)) dir.create(map_dir)
    
    # Read shapefile
    bra_states <- st_read("data/bra_states.shp")
    
    # Filter and summarize the data
    states_sp <- dataframe %>%
        filter(!ocor_amz, estado != "") %>%
        select(nome_cientifico, estado) %>%
        group_by(nome_cientifico, estado) %>%
        summarize(count = n(), .groups = 'drop')
    
    if (nrow(states_sp) > 0) {
        # Split state abbreviations into separate rows
        states_sp <- states_sp %>%
            separate_rows(estado, sep = ", ") %>%
            mutate(estado = toupper(estado))
        
        # Loop through unique species
        unique_species <- unique(states_sp$nome_cientifico)
        
        for (specie in unique_species) {
            # Filter states for the current specie
            states_sp_filter <- states_sp %>%
                filter(nome_cientifico == specie)
            
            # Get the shapefiles for these states
            states_shapes <- bra_states %>%
                filter(SIGLA %in% states_sp_filter$estado)
            
            # Plot
            p <- ggplot() +
                geom_sf(data = bra_states, fill = "white", color = "black", alpha = 0.5) +
                geom_sf_text(data = bra_states, aes(label = SIGLA), size = 2.4) +
                geom_sf(data = states_shapes, aes(fill = SIGLA), alpha = 0.5, show.legend = FALSE) +
                theme(plot.title = element_text(hjust = 0.5, size = 10),
                      axis.text = element_text(size = 7),
                      axis.text.y = element_text(angle = 90, hjust = 0.5),
                      #legend.position = c(0.92, 0.5),
                      #legend.background = element_rect(fill = alpha('white', 0.6)),
                      plot.caption = element_text(size = 7.5, face = "italic", hjust = 0.5, vjust = 2)
                      ) +
                labs(title = bquote("Espécie Sem Ocorrência na Amazônia:" ~italic(.(specie))),
                     x = "", y = "",
                     caption = paste0("Fonte: REFLORA - Herbário Virtual (", format(Sys.Date(), "%Y"), ")"))
            
            ggsave(filename = paste0("ocorrencia_", gsub(" ", "_", specie), ".png"),
                   width = 5, height = 5,
                   plot = p,
                   path = map_dir)
        }
    }
}
