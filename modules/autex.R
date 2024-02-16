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


autex_generate <- function(dataframe) {
    autex <- dataframe %>%
        select(nome_cientifico, nome_popular, status_conservacao, categoria2, vol_geo) %>%
        filter(categoria2 == "Explorar") %>%
        group_by(nome_cientifico) %>%
        mutate(numero_árvores = sum(categoria2 == "Explorar"),
               volume_autorizado = sum(vol_geo)) %>%
        group_by(nome_cientifico, categoria2) %>%
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
