#' @name basal_area_table
#'
#' @description
#' This function processes a dataframe containing information about basal area
#' and generates a table visualizing the top 10 species with the highest intervention
#' of cutting in terms of basal area. It also saves the processed data as a CSV file
#' and the table visualization as a PNG image.
#'
#' @param dataframe The input dataframe containing information about basal area.
#' @return None
#' @export
#' @examples
#' \dontrun{
#' basal_area_table(my_dataframe)
#' }
basal_area_table <- function(dataframe) {
    
    df <- dataframe |>
        dplyr::group_by(nome_cientifico, categoria2) |>
        dplyr::summarize(G = sum(g)) |>
        tidyr::spread(key = categoria2, fill = 0, value = G) |>
        dplyr::rename(`g Corte` = Explorar, `g Remanescente` = Remanescente) |>
        dplyr::mutate(`g Total` = `g Corte` + `g Remanescente`) |>
        dplyr::select(`g Total`, `g Corte`, `g Remanescente`)
        
        output_dir <- "output/"
        if (!dir.exists(output_dir)) dir.create(output_dir)
        
        write.csv2(
            df,
            file = paste(output_dir, "Corte_Area_Basal.csv"),
            row.names = FALSE,
            fileEncoding = "latin1"
        )
        
        # Define a table
        sum_of_g <- sum(df$`g Total`)
        
        table <- df |>
            dplyr::mutate(`% Corte` = `g Corte` / sum_of_g * 100) |>
            dplyr::filter(`g Corte` > 0) |>
            dplyr::arrange(desc(`% Corte`)) |>
            tibble::rowid_to_column(var = "n") |>
            dplyr::filter(n <= 10)
            
        title_table <- paste(
            "Tabela 4. Relação das 10 espécies que receberão maior intervenção de corte em área basal."
        )
        subtitle_table <- paste(
            "Em que: g Total = Área basal total por espécie; g Corte = Área basal a ser cortada; g Remanescente = Área basal remanescente e % Corte = Percentual de área basal a ser cortado por espécie."
        )
        
        table |> 
            dplyr::as_tibble() |>
            dplyr::select(c(2:6)) |>
            gt::gt(rownames_to_stub = T) |>
            gt::tab_stubhead(label = "#") |>
            gt::tab_header(
                title = title_table,
                subtitle = subtitle_table
            ) |>
            gt::cols_align(align = "center") |>
            tab_style(
                style = cell_text(align = "center"),
                locations = list(cells_stubhead())
            ) |>
            gt::tab_style(
                style = cell_text(style = "italic"),
                locations = cells_body(columns = "nome_cientifico")
            ) |>
            gt::grand_summary_rows(
                columns = -c(nome_cientifico),
                fns = list(id = "sum_id", label = md("**Total**"), fn = "sum"),
                fmt = ~ fmt_number(., use_seps = TRUE, sep_mark = ".", dec_mark = ","),
                missing_text = ""
            ) |>
            gt::cols_label(
                nome_cientifico = "**Espécie**",
                `g Total` = "**g Total**",
                `g Corte` = "**g Corte**",
                `g Remanescente` = "**g Remanescente**",
                `% Corte` = "**% Corte**",
                .fn = md
            ) |>
            gt::fmt_number(
                columns = -c(nome_cientifico),
                decimals = 2,
            ) |>
            tab_style(
                style = cell_text(weight = "bold"),
                locations = cells_grand_summary()
            ) |>
            gt::tab_style(
                style = list(
                    cell_borders(
                        sides = c("top", "bottom"),
                        color = "#fff",
                        #weight = px(1)
                    ),
                    cell_text(align = "center"),
                    cell_fill(color = "#fff", alpha = NULL)
                ),
                locations = cells_body(columns = everything(), rows = everything())
            ) |>
            gt::tab_style(
                style = cell_borders(color = "#fff"), locations = cells_stub()
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
                stub.font.weight = "bold",
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
            gt::gtsave(
                filename = 'Tabela_4_Top_10_Espedcies_Area_Basal_Corte.png',
                path = './output/Tabelas/'
            )
}
