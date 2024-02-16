#' summary_table function
#'
#' @description
#' This function takes any data frame from a global forest inventory and make a
#' a table only with commercial species of timber.
#'
#' @param dataset
#'
#'

commercial_species_table <- function(dataframe) {

        ## Read The List of Endangered Species, according to the Ordinance of the
        ## Brazilian Ministry of Environment (Portaria MMA 443/2014) and Resolution 54/2007
        ## from the Pará state Brazil
        port <- readr::read_csv2(
                "https://raw.githubusercontent.com/rcDeveloping/endangeredBrazilianPlantSpecies/main/output/Especies_Ameacadas_BRA.csv",
                locale = readr::locale(encoding = "latin1")
        ) %>%
                filter(fonte %in% c('PA', 'Portaria 443')) %>%
                select(-c(1, 4, 5, 6, 8))

        # Get all species from Portaria MMA 300/2023
        port443 <- unique(subset(port, dispositivo_legal == 'Portaria MMA 443/2014',
                                    select = 'nome_cientifico'))

        # Get all species from Resolução COEMA-PA 24/2007
        coema_sp <- unique(subset(port, dispositivo_legal == 'Resolução COEMA 54/2007',
                                  select = 'nome_cientifico'))

        com_sp_table <- dataframe %>%
                filter(uso == 'Comercial') %>%
                group_by(nome_cientifico, categoria) %>%
                summarise(Total = n()) %>%
                spread(key = categoria, value = Total) %>%
                mutate(Total = Explorar + Remanescente)

        file_name_table <- paste0(
                './output/dataFrame/tab2', '_umf_', umf, '_upa_', upa, '_', flona, '.csv'
        )[1]

        write.csv2(com_sp_table, file = file_name_table, row.names = FALSE)

        com_sp_tab <- read.csv2(file_name_table)

        title_table <- paste0('Tabela 2. Relaçãoo de espécies a serem manejadas na UPA ', upa, ', UMF ', umf, ', FLONA ', flona, '.')

        table <- com_sp_tab %>%
                mutate(N = 1:n()) %>%
                select(N, nome_cientifico, Explorar, Remanescente, Total)

        table %<>%
                gt(rowname_col = 'N') %>%
                tab_stubhead(label = 'N') %>%
                tab_header(
                        title = html(paste('<p style="align:center"><i>', title_table, '</i></p>'))
                ) %>%
                cols_width(
                        N ~ px(50),
                        nome_cientifico ~ px(200),
                        Explorar ~ px(100),
                        Remanescente ~ px(100),
                        Total ~ px(100)
                ) %>%
                fmt_number(
                        columns = c('Explorar', 'Remanescente', 'Total'),
                        decimals = 0,
                        sep_mark = '.'
                ) %>%
                cols_label(  ## Change the style names of the columns
                        nome_cientifico = md('Espécie')
                ) %>%
                cols_align(align = 'center') %>%
                summary_rows(
                        groups = NULL,
                        columns = c('Explorar', 'Remanescente', 'Total'),
                        fns = list(Total = 'sum'),
                        formatter = fmt_number,
                        decimals = 0,
                        sep_mark = '.',
                        #use_seps = TRUE,
                        missing_text = ''
                ) %>%
                tab_style(
                        style = cell_text(style = 'italic'),
                        locations = cells_body(
                                columns = nome_cientifico
                        )
                ) %>%
                tab_style(
                        style = list(
                                cell_borders(
                                        sides = c('top', 'bottom'),
                                        color = 'white',
                                        weight = px(1)
                                ),
                                cell_text(
                                        align = 'center'
                                ),
                                cell_fill(color = 'white', alpha = NULL)
                        ),
                        locations =
                                cells_body(
                                        columns = everything(),
                                        rows = everything()
                                )
                ) %>%
                tab_style(
                        style = cell_borders(
                                color = 'white',
                        ),
                        locations = cells_stub()
                ) %>%
                tab_source_note(
                        source_note = html(
                                '<div align="center"><i>',
                                paste0(
                                        'Fonte: Inventário Florestal UPA ',
                                        upa, ', UMF ', umf, ', FLONA ', flona, '.'
                                ),
                                '</i></div>'
                        )
                ) %>%
                tab_footnote(
                        footnote = html('<div align="left"><i>',
                                        'Espécie Ameaçada (Port. MMA 443/2014).',
                                        '</i></div>'),
                        locations = cells_body(
                                columns = nome_cientifico,
                                rows = nome_cientifico %in% port443$nome_cientifico
                        )
                ) %>%
                #opt_footnote_marks(marks = c("*", "**")) %>%
                tab_footnote(
                        footnote = html('<div align="left"><i>',
                                        'Espécie Ameaçada (Resolução COEMA-PA 54/2007).',
                                        '</i></div>'),
                        locations = cells_body(
                                columns = nome_cientifico,
                                rows = nome_cientifico %in% coema_sp$nome_cientifico
                        )
                ) %>%
                tab_style(
                        style = cell_borders(
                                color = 'white',
                        ),
                        locations = cells_stub()
                ) %>%
                tab_options(## make the title size match the body text
                        heading.title.font.size = px(16),
                        table.width = pct(100)
                ) %>%
                tab_options(
                        ## hide the top-most border
                        table.border.top.color = 'white',
                        ## change the column labels section
                        column_labels.border.top.width = 3,
                        column_labels.border.top.color = 'black',
                        column_labels.border.bottom.width = 3,
                        column_labels.border.bottom.color = 'black',
                        ## Change the vertical lines of summary body
                        stub.border.color = 'white',
                        stub.background.color = 'white',
                        ## Change the foot source body
                        footnotes.border.bottom.color = 'white',
                        ## change the bottom of the body
                        table_body.border.bottom.color = 'black',
                        ## hide the bottom-most line or footnotes
                        ## will have a border
                        table.border.bottom.color = 'white',
                        ## make the width 100%
                        table.width = pct(100),
                        table.background.color = 'white'
                )

        ## Save the Table 2 in .png file format
        gtsave(table, filename = 'Tabela_2.png', path = './output/table')
}
