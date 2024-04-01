#' @name horizontal_structure
#' @title Compute ecological indices and generate a styled table for the top 10 species.
#'
#' @description
#' This function computes various ecological indices, including density, dominance, and importance value index, for different species in a given dataset. It then generates a styled table presenting the top 10 species based on certain criteria.
#'
#' @param dataframe A data frame containing ecological data with specific columns.
#' @return NULL. However, it generates a CSV file and a PNG image representing the top 10 species based on ecological indices.
#' @export
#' @import dplyr
#' @import magrittr
#' @importFrom gt gt gtsave tab_header cols_label cols_align cols_width tab_style fmt_number tab_options
#' @examples
#' # Load required libraries
#' library(dplyr)
#' library(gt)
#'
#' # Example usage
#' # horizontal_structure(my_forest_inventory_data)
#'
horizontal_structure <- function(dataframe) {
    
    n_ut <- length(unique(dataframe$ut))
    
    # Set absolute density
    df <- dataframe |>
        dplyr::select(num_arvore, ut, aem, flona, umf, upa, nome_cientifico, dap, g, volume) |>
        dplyr::mutate(n.ha = 1 / (n_ut * aem)) |>
        dplyr::mutate(G = g * n.ha, v.ha = volume * n.ha)
    
    
    # Set absolute and relative density
    da <- df |>
        dplyr::group_by(nome_cientifico) |>
        dplyr::summarize(DA = sum(n.ha))
    
    dr <- da |>
        dplyr::group_by(nome_cientifico) |>
        dplyr::mutate(DR = DA / sum(da$DA) * 100)
    
    # Sampling unit where the species occurred
    ui <- dataframe |>
        dplyr::select(nome_cientifico, ut) |>
        dplyr::group_by(nome_cientifico, ut) |>
        dplyr::summarise(n = n()) |>
        tidyr::spread(key = ut, value = n)
    
    ui$UI <- rowSums(!is.na(ui[, c(2:ncol(ui))]))
    ui <- ui[, c(1, length(ui))]
    
    horiziontal_str <- merge(dr, ui)
    
    # Set absolute and relative frequency
    horiziontal_str <- horiziontal_str |>
        dplyr::mutate(FA = UI / n_ut * 100) |>
        dplyr::select(-c(tidyr::matches('[0-9]')))
    
    sum_of_FA <- sum(horiziontal_str[, "FA"])
    
    horiziontal_str <- horiziontal_str |>
        dplyr::mutate(FR = FA / sum_of_FA * 100)
    
    # Set Absolute and Relative Dominance
    doa <- df |>
        dplyr::group_by(nome_cientifico) |>
        dplyr::summarize(DOA = sum(G))
    
    sum_of_doa <- sum(doa$DOA)
    
    horiziontal_str <- merge(horiziontal_str, doa)
    
    horiziontal_str <- horiziontal_str %>%
        dplyr::mutate(DOR = DOA / sum_of_doa * 100) %>%
        # Define the Importance Value Index
        dplyr::mutate(VI = rowSums(dplyr::select(., c(DR, FR, DOR))) / 3) %>%
        dplyr::mutate(VC = rowSums(dplyr::select(., c(DR, DOR))) / 2) %>%
        dplyr::arrange(desc(VC))
    
    output_dir <- "output/Planilhas/"
    if (!dir.exists(output_dir)) dir.create(output_dir)
    
    write.csv2(
        horiziontal_str,
        file = paste(output_dir, "Estrutura_Horizontal.csv"),
        row.names = FALSE,
        fileEncoding = "latin1"
    )
    
    # Define a table
    top10_basal_area <- horiziontal_str |>
        dplyr::filter(row_number() <= 10)
    
    title_table <- paste(
        "Tabela 3. Relação das 10 espécies que apresentaram maior volor de cobertura na área da UPA."
    )
    subtitle_table <- paste(
        "Em que: DA = Densidade Absoluta; DR = Densidade Relativa; DOA = Dominância Absoluta; DOR = Dominância Relativa; VI = Valor de Importância; VC = Valor de Cobertura."
    )
    
    top10_basal_area |>
        dplyr::select(-c(UI, FA, FR)) |>
        gt::gt() |>
        gt::tab_header(
            title = title_table,
            subtitle = subtitle_table
        ) |>
        gt::cols_label(
            nome_cientifico = "Espécie"
        ) |>
        gt::cols_align(align = "center") |>
        gt::cols_width(
            nome_cientifico ~ px(120),
            everything() ~ px(50)
        ) |>
        gt::tab_style(
            style = gt::cell_text(style = "italic"),
            locations = gt::cells_body(columns = nome_cientifico)
        ) |>
        gt::fmt_number(
            columns = c(-nome_cientifico),
            decimals = 2,
        ) |>
        gt::tab_options(
            #heading.title.font.size = px(14),
            table.font.size = px(9)
        ) |>
        gt::tab_options(
            table.font.size = px(9),
            footnotes.multiline = FALSE,
            data_row.padding = px(2),
            grand_summary_row.padding = px(2),
            heading.border.bottom.width = 2,
            heading.border.bottom.color = "#000000",
            # hide the top-most border
            table.border.top.color = '#fff',
            # change borders
            summary_row.border.width = 2,
            summary_row.border.color = "#000000",
            grand_summary_row.border.width = 2,
            grand_summary_row.border.color = "#000000",
            column_labels.border.top.width = 2,
            column_labels.border.top.color = "#000000",
            column_labels.border.bottom.width = 2,
            column_labels.border.bottom.color = "#000000",
            # Change the vertical lines of summary body
            stub.border.color = "#fff",
            stub.background.color = "#fff",
            stub_row_group.border.color = "#fff",
            # Change the foot source body
            footnotes.border.bottom.color = "#fff",
            # change the bottom of the body
            table_body.border.bottom.color = "#000000",
            table_body.border.bottom.width = 2,
            # hide the bottom-most line or footnotes
            # will have a border
            table.border.bottom.color = '#fff',
            table.background.color = '#fff'
        ) |>
        gt::gtsave(filename = 'Tabela_3_Estrutura_Horizontal.png', path = './output/Tabelas/')
}
