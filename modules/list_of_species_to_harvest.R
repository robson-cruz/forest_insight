#' @name commercial_species_table
#' 
#' @title Commercial Species Table
#' 
#' @description
#' This function takes a dataframe containing forest inventory data and filters
#' out the commercial species of timber. It then creates a table of these species
#' and saves it as a CSV file and a table as an image.
#'
#' @param dataframe The dataframe containing forest inventory data.
#' @return The function doesn't explicitly return anything, but it saves a CSV file
#' containing the data of commercial timber species and an image of the table.
#'
#' @import dplyr
#' @import gt
#' @import magrittr
#' @importFrom tidyr spread
#' @export
#'
#' @examples
#' # Example usage:
#' # Assuming df is a dataframe containing forest inventory data
#' commercial_species_table(df)
#'
#'@references
#' For information on the Brazilian Ministry of Environment regulations, see:
#' \url{https://www.ibama.gov.br/component/legislacao/?view=legislacao&legislacao=134519}
#' 
library(dplyr, warn.conflicts = FALSE)
library(gt)
library(magrittr)

commercial_species_table <- function(dataframe) {
    # Read The List of Endangered Species, according to the Ordinance of the
    # Brazilian Ministry of Environment ('Portaria MMA 443/2014') and 'Resolution 54/2007'
    # from the Pará state Brazil
    port <- readr::read_csv2(
        "https://raw.githubusercontent.com/rcDeveloping/endangeredBrazilianPlantSpecies/main/output/Especies_Ameacadas_BRA.csv",
        locale = readr::locale(encoding = "latin1")
    ) |>
        filter(fonte %in% c("PA", "Portaria 443")) |>
        select(-c(1, 4, 5, 6, 8))
    
    # Get all species from Portaria MMA 443/2014
    port443 <-
        unique(
            subset(port, dispositivo_legal == "Portaria MMA 443/2014",
                   select = "nome_cientifico")
        )
    
    # Get all species from Resolução COEMA-PA 24/2007
    coema_sp <-
        unique(
            subset(
                port,
                dispositivo_legal == "Resolução COEMA 54/2007",
                select = "nome_cientifico"
            )
        )
    
    #rm(port)
    
    commercial_species_table <- dataframe |>
        group_by(nome_cientifico, categoria2) |>
        summarise(Total = n()) |>
        tidyr::spread(key = categoria2, value = Total) |>
        mutate(Total = Explorar + Remanescente) |>
        filter(Explorar > 0)
    
    
    table_dir <- "output/Tabelas/"
    spreadsheet_dir <- "output/Planilhas/"
    
    if (!dir.exists(table_dir)) dir.create(table_dir)
    if (!dir.exists(spreadsheet_dir)) dir.create(spreadsheet_dir)
    
    write.csv2(commercial_species_table,
               paste0(spreadsheet_dir, "Tabela_2_Especies_a_Serem_Manejadas", ".csv"),
               row.names = FALSE,
               fileEncoding = "latin1")
     
    com_sp_tab <- read.csv2("output/Planilhas/Tabela_2_Especies_a_Serem_Manejadas.csv",
                            fileEncoding = "latin1")
    
    table <- com_sp_tab |>
        mutate(n = 1:n()) |>
        select(n, nome_cientifico, Explorar, Remanescente, Total)
    
    title_table <- "Tabela 2. Relação de Espécies a Serem Manejadas na UPA."
    
    # Create the table
    table |>
        gt(rowname_col = 'n') |>
        tab_stubhead(label = "n") |>
        cols_label(
            nome_cientifico = md("Espécie")
        ) |>
        tab_style(
            style = cell_text(align = "center"),
            locations = list(cells_stubhead(), cells_stub())
        ) |>
        tab_header(title = html(
            paste('<p style="align:center"><i>', title_table, '</i></p>')
        )) |>
        fmt_number(
            columns = c("Explorar", "Remanescente", "Total"),
            decimals = 0,
            sep_mark = "."
        ) |>
        cols_align(align = "center") |>
        summary_rows(
            groups = NULL,
            #vars = c("Explorar", "Remanescente", "Total"),
            fns = list(Total = "sum"),
            fmt = ~ fmt_number(columns = c("Explorar", "Remanescente", "Total"), decimals = 0, use_seps = TRUE),
            sep_mark = ".",
            stub = TRUE
        ) |>
        tab_style(
            style = cell_text(style = "italic"),
            locations = cells_body(columns = nome_cientifico)
        ) |>
        tab_style(
            style = list(
                cell_borders(
                    sides = c("top", "bottom"),
                    color = "#fff",
                    weight = px(1)
                ),
                cell_text(align = "center"),
                cell_fill(color = "#fff", alpha = NULL)
            ),
            locations =
                cells_body(columns = everything(),
                           rows = everything())
        ) |>
        tab_style(style = cell_borders(color = "#fff"),
                  locations = cells_stub()) |>
        tab_source_note(source_note = html(
            '<div align="center"><i>',
            'Fonte: Inventário Florestal UPA.',
            '</i></div>'
        )) |>
        tab_footnote(
            footnote = 'Espécie Ameaçada (Port. MMA 443/2014).',
            locations = cells_body(
                columns = nome_cientifico,
                rows = nome_cientifico %in% port443$nome_cientifico
            )
        ) |>
        tab_footnote(
            footnote = 'Espécie Ameaçada (Resolução COEMA-PA 54/2007).',
            locations = cells_body(
                columns = nome_cientifico,
                rows = nome_cientifico %in% coema_sp$nome_cientifico
            )
        ) |>
        tab_options(
            table.font.size = px(9),
            footnotes.multiline = FALSE,
            data_row.padding = px(2),
            heading.border.bottom.width = 2,
            heading.border.bottom.color = "#000000",
            # hide the top-most border
            table.border.top.color = '#fff',
            # change the column labels section
            column_labels.border.top.width = 2,
            column_labels.border.top.color = "#000000",
            column_labels.border.bottom.width = 2,
            column_labels.border.bottom.color = "#000000",
            # Change the vertical lines of summary body
            stub.border.color = "#fff",
            #stub.background.color = "#fff",
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

    # Save the table as an image
    gtsave(filename = 'Tabela_2.png', path = table_dir)

}
