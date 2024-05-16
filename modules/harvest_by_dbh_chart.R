#' @name harvest_by_dbh_chart
#' 
#' @title Harvest by DBH class
#'
#' @description
#' This function takes a dataframe containing information about harvested trees
#' and generates a bar chart depicting the number of trees harvested based on
#' their diameter at breast height (dap), categorized by their class and selection type.
#'
#' @param dataframe A dataframe containing information about harvested trees,
#' including columns: nome_cientifico (scientific name), dap (diameter at breast height),
#' categoria2 (selection type), classe (class).
#' 
#' @return The function generates a PNG file of the plot.
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' # Load required libraries
#' library(dplyr)
#' library(ggplot2)
#' 
#' # Generate sample dataframe
#' dataframe <- data.frame(nome_cientifico = c("Species A", "Species B", "Species C"),
#'                         dap = c(45, 55, 60),
#'                         categoria2 = c("Type 1", "Type 2", "Type 1"),
#'                         classe = c("Class 1", "Class 2", "Class 1"),
#'                         classificacao = c("Comercial", "Comercial", "Comercial"))
#' 
#' # Generate the bar chart
#' harvest_by_dbh_chart(dataframe)
#' }
#'
harvest_by_dbh_chart <- function(dataframe) {
    
    species_to_harvest <- dataframe %>%
        filter(dap >= 50) %>%
        filter(classificacao == "Comercial") %>%
        select(nome_cientifico, dap, categoria2, classe2) %>%
        group_by(classe2, categoria2) %>%
        summarize(N = n())
    
    dbh_to_harvest <- dataframe %>%
        filter(dap >= 50) %>%
        filter(classificacao == "Comercial") %>%
        count(classe2, name = "N") %>%
        mutate(categoria2 = "Total")
    
    harvest_data <- bind_rows(species_to_harvest, dbh_to_harvest)
    
    output_dir <- "./output/Graficos/Selecao_Corte/"
    if (!dir.exists(output_dir)) dir.create(output_dir)
    
    png(paste0(output_dir, "Selecao_Corte_dap", ".png"),
        width = 6,
        height = 4,
        units = "in",
        res = 300)
    
    harvest_plot <- harvest_data %>%
        ggplot(aes(x = classe2, y = N, group = categoria2)) +
        geom_bar(stat = "identity", position = "dodge", aes(fill = categoria2)) +
        theme(
            plot.title = element_text(color = "#000000", size = 10, hjust = 0.5),
            axis.text.x = element_text(angle = 50, size = 7),
            axis.text.y = element_text(angle = 90, size = 7, hjust = 0.5),
            axis.title.x = element_text(size = 9),
            axis.title.y = element_text(size = 9),
            legend.title = element_text(size = 7),
            legend.text = element_text(size = 7),
            legend.key.height = unit(4, "mm"),
            legend.key.width = unit(4, "mm"),
            legend.position = "bottom"
        ) +
        labs(
            title = "Seleção de Árvores",
            x = "DAP (cm)",
            y = "Número de Árvores",
            fill = "Seleção:"
        )
    
    print(harvest_plot)
    
    dev.off()
}
