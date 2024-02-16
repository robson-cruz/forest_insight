#' @name classify_species
#' 
#' @title Classify species based on exploitation
#'
#' @description This function classifies species based on their exploitation status.
#'
#' @param dataframe A dataframe containing information about tree species.
#' 
#' @return The input dataframe with an additional column indicating the
#' classification of each species.
#' 
#' @import dplyr
#' @import magrittr
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' # Load required libraries
#' library(magrittr)
#' library(dplyr)
#' 
#' # Create example dataframe
#' dataframe <- data.frame(nome_cientifico = c("Species A", "Species B", "Species A", "Species C"),
#'                         status_conservacao = c("Endangered", "Not endangered", "Endangered", "Endangered"),
#'                         categoria2 = c("Explorar", "Remanescente", "Explorar", "Remanescente"),
#'                         num_arvore = c(1, 2, 3, 4))
#' 
#' # Call the function
#' classify_species(dataframe)
#'}
#'
classify_species <- function(dataframe) {
    
    species_classification <- dataframe %>%
        select(nome_cientifico, status_conservacao, categoria2) %>%
        group_by(nome_cientifico) %>%
        summarize(
            Corte = sum(categoria2 == 'Explorar'),
            Remanescentes = sum(categoria2 == 'Remanescente'),
            Total = Corte + Remanescentes
        ) %>%
        mutate(classificacao = (ifelse(Corte == 0, 'Nao Comercial', 'Comercial')))
    
    # Add species_classification data to the forest inventory dataframe
    dataframe %<>%
        left_join(species_classification[-c(2, 3, 4)], by = 'nome_cientifico') %>%
        # By default, mutate() keeps all columns from the input data
        # Use '.keep_all' to override it
        distinct(num_arvore, .keep_all = TRUE)
    
    # Assign the updated dataframe to the user's global environment
    assign("dataframe", dataframe, envir = .GlobalEnv)
}
