#' basa_area_by_plot function
#' 
#' @description 
#' This function takes any data frame from a global forest inventory and make a 
#' a table with the the basal area distributed by each plot.
#' 
#' @param dataset
#' 
#' 

basa_area_by_plot <- function(df) {
        tab4.1 <- df %>%
                # filter(Uso == 'Comercial') %>%
                group_by(ut, categoria) %>%
                summarize(s_g = sum(g)) %>%
                spread(key = categoria, value = s_g) %>%
                rename(g_corte = Explorar, g_rem = Remanescente)
        
        tab4.2 <- df %>%
                group_by(ut) %>%
                summarise(N = n(),
                          g_mean = mean(g),
                          g_med = median(g),
                          g_sd = sd(g),
                          g_UT = sum(g))
        
        tab4 <- tab4.2 %>%
                left_join(tab4.1, by = 'ut')
        
        write.csv2(tab4,
                   './output/dataFrame/Tabela_4.csv',
                   row.names = FALSE,
                   fileEncoding = 'latin1')
        
        
        title_tab4 <- paste0('Tabela 4. Estatística descritiva e intervenção florestal para Variável área basal por UT na UPA ', upa, ', UMF ', umf, ', FLONA ', flona, '. Em que: N = número de árvores por UT; g_mean = área basal média por UT; g_med = mediana da área basal por UT; g_sd = desvio padrão da área basal por UT; g_UT = Área basal total por UT; g_corte = Área basal selecionada para corte por UT; g_rem = Área basal remanescente por UT.')[1]
        
        tabela_4 <-  tab4 %>%
                gt(rowname_col = 'ut') %>%
                tab_stubhead(label = 'UT') %>%
                tab_header(
                        title = html(paste('<p "style=align:center"><i>', title_tab4, '</i></p>'))
                ) %>%
                fmt_number(
                        columns = c('g_mean', 'g_med',
                                    'g_sd', 'g_UT', 'g_corte', 'g_rem'),
                        decimals = 2,
                        sep_mark = '.',
                        dec_mark = ','
                ) %>%
                fmt_number(
                        columns = 'N',
                        decimals = 0,
                        sep_mark = '.'
                ) %>%
                cols_width(
                        ut ~ px(50),
                        starts_with('N') ~ px(100),
                        starts_with('g') ~ px(100)
                ) %>%
                grand_summary_rows(
                        #groups = NULL,
                        columns = c('g_mean', 'g_med',
                                    'g_sd', 'g_UT', 'g_corte', 'g_rem'),
                        fns = list(Total = 'sum'),
                        formatter = fmt_number,
                        decimals = 2,
                        sep_mark = '.',
                        use_seps = TRUE,
                        dec_mark = ',',
                        missing_text = ''
                ) %>%
                grand_summary_rows(
                        columns = 'N',
                        fns = list(Total = 'sum'),
                        #formatter = fmt_number,
                        decimal = 0,
                        sep_mark = '.',
                        use_seps = TRUE
                ) %>%
                summary_rows(
                        #groups = NULL,
                        columns = c('g_UT'),
                        fns = list('%' = ~sum(tab4$g_UT)/sum(tab4$g_UT)*100),
                        formatter = fmt_number,
                        #decimals = 2,
                        #sep_mark = '.',
                        #use_seps = TRUE,
                        #dec_mark = ',',
                        missing_text = '--'
                ) %>%
                grand_summary_rows(
                        #groups = NULL,
                        columns = c('g_corte'),
                        fns = list('%' = ~sum(tab4$g_corte)/sum(tab4$g_UT)*100),
                        formatter = fmt_number,
                        #decimals = 2,
                        #sep_mark = '.',
                        use_seps = TRUE,
                        #dec_mark = ',',
                        missing_text = '--'
                ) %>%
                grand_summary_rows(
                        #groups = NULL,
                        columns = c('g_rem'),
                        fns = list('%' = ~sum(tab4$g_rem)/sum(tab4$g_UT)*100),
                        formatter = fmt_number,
                        #decimals = 2,
                        #sep_mark = '.',
                        use_seps = TRUE,
                        #dec_mark = ',',
                        missing_text = '--'
                ) %>%
                tab_style(
                        style = cell_text(weight = 'bold'),
                        locations = cells_stub_grand_summary(
                                rows = everything()
                        )
                ) %>%
                cols_align(align = 'center') %>%
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
                        locations = cells_body(
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
                                paste0('Fonte: Inventário Florestal UPA ', upa, ', UMF ', umf, ', FLONA ', flona, '.'),
                                '</i></div>'
                        )
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
        
        gtsave(tabela_4, filename = 'Tabela_4.png', path = './output/table')
}
