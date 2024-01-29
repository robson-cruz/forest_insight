#' basal_area_table function
#' 
#' @description 
#' This function takes any data frame from a global forest inventory and make a 
#' a table with the top 10 species that will most receive cutting at basal area.
#' 
#' @param dataset
#' 
#' 


basal_area_table <- function(df) {
        table3 <- df %>%
                group_by(nome_cientifico, categoria) %>%
                summarise(Gsp = sum(g)) %>%
                #mutate(G_perc = Gsp / sum(Gsp) * 100) %>%
                spread(key = categoria, fill = 0, value = 'Gsp') %>%
                rename(gs_exp = Explorar, gs_rem = Remanescente) %>%
                mutate(GS = gs_exp + gs_rem) %>%
                mutate(Perc_Exp = (gs_exp / sum(df$g)) * 100) %>%
                left_join(fi_horiz[c(1, 10)], by = 'nome_cientifico') %>%
                filter(gs_exp > 0)
        
        file_name_table <- paste0(
                './output/dataFrame/tabela_3', '_UMF_', umf, '_UPA_', upa, '.csv'
        )[1]
        write.csv2(table3, file_name_table, row.names = FALSE, fileEncoding = 'latin1')
        
        tabela_3 <- read.csv2(file_name_table)
        
        top10_basal_area <- tabela_3 %>%
                arrange(desc(Perc_Exp)) %>%
                filter(row_number() <= 10)
        
        out_top10_basal_area <- glue::glue_collapse(
                sort(top10_basal_area$nome_cientifico),
                sep = ', ',
                last = ' e '
        )
        
        write.table(out_top10_basal_area,
                    file = paste0(
                            './output/Top10_sp_Basal_Area_',
                            'upa_', upa, '_umf_', umf, '_', flona, '.txt'
                    )[1],
                    quote = FALSE,
                    sep = ',',
                    col.names = '10 sp com maior corte em area basal:\n',
                    row.names = FALSE)
        
        
        title_tab3 <- paste0('Tabela 3. Relação das 10 espécies que receberão maior intervenção de corte em área basal. Em que: gs_exp = Área Basal a ser cortada por Espécie (m²); gs_rem = Área Basal remanescente por Espécie (m²); GS = Área Basal total por Espécie (m²); DOR = Dominância Relativa por Espécie (%); Perc_Exp = Percentual de Corte em Área Basal por Espécie (m²).')[1]
        
        tab3IF <- tabela_3 %>%
                arrange(desc(Perc_Exp)) %>%
                filter(row_number() <= 10) %>%
                gt(rowname_col = 'nome_cientifico') %>%
                tab_stubhead(label = 'Espécie') %>%
                tab_header(
                        title = html(paste('<p "style=alig:center"><i>', title_tab3, '</i></p>'))
                ) %>%
                fmt_number(
                        columns = c('gs_exp', 'gs_rem', 'GS', 'Perc_Exp'),
                        decimals = 4,
                        sep_mark = '.',
                        dec_mark = ','
                ) %>%
                fmt_number(
                        columns = c('DOR'),
                        decimals = 2,
                        sep_mark = '.',
                        dec_mark = ','
                ) %>%
                cols_width(
                        #N ~ px(80),
                        nome_cientifico ~ px(250),
                        starts_with('gs') ~ px(100),
                        starts_with('G') ~ px(100),
                        starts_with('Perc') ~ px(100),
                        DOR ~ px(100)
                ) %>%
                grand_summary_rows(
                        #groups = NULL,
                        columns = c('gs_exp', 'gs_rem', 'GS', 'Perc_Exp', 'DOR'),
                        fns = list(Total = 'sum'),
                        formatter = fmt_number,
                        decimals = 2,
                        sep_mark = '.',
                        use_seps = TRUE,
                        dec_mark = ',',
                        missing_text = ''
                ) %>%
                tab_style(
                        style = cell_text(style = 'italic'),
                        locations = cells_stub()
                ) %>%
                cols_align(align = "center") %>%
                tab_style(
                        style = cell_text(weight = 'bold'),
                        locations = cells_stub_grand_summary(
                                rows = everything()
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
                tab_options(## make the title size match the body text
                        heading.title.font.size = px(16),
                        table.width = pct(100)
                ) %>%
                tab_source_note(
                        source_note = html('<div align = "center"><I>',
                                           paste('Fonte: Inventário Florestal UPA ', upa, ', UMF ', umf, ', FLONA ', flona, '.'),
                                           '</I></div>')
                ) %>%
                # tab_footnote(
                #         footnote = 'Esp?cie Vulner?vel (Port. MMA 443/2014).',
                #         locations = cells_body(
                #                 columns = 2,
                #                 rows = nome_cientifico %in% as.vector(
                #                         df[df$statusSource!='Nao Protegida', 4])
                #         )
                # ) %>%
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
        gtsave(tab3IF, filename = 'Tabela_3_Basal_Area.png', path = './output/table')
}
