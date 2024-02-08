#' @name autex_generate
#' 
#' @title AUTEX Generate
#' 
#' @description This function takes a dataframe as input, processes it, and then
#' writes the resulting dataframe to a CSV file named `"Autex.csv"` in a directory
#' named `"output/Planilhas/"`.
#' 
#' @param dataframe with columns `nome_cientifico``, `nome_popular``, `status``,
#' `categoria` and `vol_geo`.
#' 
#' @import dplyr
#' @import tools
#' 
library(dplyr)


autex_generate <- function(df) {
    autex <- df %>%
        select(nome_cientifico, nome_popular, status, categoria, vol_geo) %>%
        filter(categoria == "Explorar") %>%
        group_by(nome_cientifico) %>%
        mutate(numero_árvore = sum(categoria == "Explorar"),
               vol_autorizado = sum(vol_geo)) %>%
        group_by(nome_cientifico, categoria) %>%
        ungroup() %>%
        # By default, mutate() keeps all columns from the input data
        # Use '.keep_all' to override it
        distinct(nome_cientifico, .keep_all = TRUE) %>%
        arrange(nome_cientifico) %>%
        select(-c(4, 5)) %>%
        rename_with(~ tools::toTitleCase(gsub("_", " ", .x, fixed = TRUE)))
    
    output_dir <- "./output/Planilhas/"
    if (!dir.exists(output_dir)) {
        dir.create(output_dir)
    }
    
    write.csv2(
        autex,
        file = paste0(output_dir, "Autex.csv"),
        na = 'NA',
        row.names = FALSE,
        fileEncoding = 'latin1'
    )
}