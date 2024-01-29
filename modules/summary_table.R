#' summary_table function
#' 
#' @description 
#' This function takes any data frame from a global forest inventory and make a 
#' summary table with the data.
#' 
#' @param dataset
#' 
#' 

summary_table <- function(df) {
        summaryFI <- df %>%
                select(dap, g, altura, volume) %>%
                summary()
        
        write.csv2(
                summaryFI,
                './output/dataFrame/Tabela_1.csv',
                row.names = FALSE
        )
        
        tab <- read.csv2('./output/dataFrame/Tabela_1.csv')
        
        # set table
        title_table <- paste0('Tabela 1. Estatística descritiva para as variáveis do inventário florestal da UPA ', upa, ', UMF ', umf, ', FLONA ', flona,'.<br> Em que: DAP (cm) = Diâmetro a altura do peito calculado em cm; Altura (m) = Altura comercial em metros estimada durante o inventário florestal; g (m²) = Área basal calculada em metros quadrados; Volume (m³)= Volume calculado a partir de equação ajustada para a UMF ', umf, ' unidade em metros cúbicos.')
        
        summary_table <- tab %>%
                gt() %>%
                cols_label(
                        X.....dap = md('**DAP (cm)**'),
                        X......g = md('**g (m2)**'),
                        X....altura = md('**Altura (m)**'),
                        X....volume = md('**Volume (m3)**')
                ) %>%
                tab_header(
                        title = html(paste('<p style="text-align:center"><i>',title_table,'</i></p>'))
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
                        source_note = html('<div align="center"><i>',
                                           paste0('Fonte: Inventário Florestal UPA ',
                                                  upa, ', UMF ', umf, ', FLONA ', flona, '.'),
                                           '</i></div>')
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
                        table.width = pct(80),
                        table.background.color = 'white'
                )
        
        ## Save the Table 1 in .png file format
        gt::gtsave(summary_table, filename = 'Tabela_1.png', path = './output/table')
}
