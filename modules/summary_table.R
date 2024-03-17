#' @name summary_table
#' 
#' @title Generate Summary Table for Forest Inventory Data
#' 
#' @description
#' This function generates a summary table for forest inventory data including
#' descriptive statistics for variables such as diameter at breast height (dap),
#' basal area (g), height (altura), and volume (vol_geo). The summary table is
#' saved as both a CSV file and a PNG image.
#'
#' @param dataframe A dataframe containing forest inventory data with columns
#'                  for DAP, basal area, height, and volume.
#' @return NULL
#' @export
#'
#' @import gt
#' @importFrom htmltools html
#' @importFrom dplyr everything
#' @importFrom dplyr mutate
#' @importFrom stats summary
#' @importFrom utils write.csv2
#' 
#' @examples
#' # Assuming df is your dataframe containing forest inventory data
#' summary_table(df)
#'
summary_table <- function(dataframe) {
    table_dir <- "output/Tabelas/"
    spreadsheet_dir <- "output/Planilhas/"
    if (!dir.exists(table_dir)) dir.create(table_dir)
    if (!dir.exists(spreadsheet_dir)) dir.create(spreadsheet_dir)
    
    summary_data <- data.frame(
        dap = as.vector(summary(dataframe$dap)),
        g = as.vector(summary(dataframe$g)),
        altura = as.vector(summary(dataframe$altura)),
        vol_geo = as.vector(summary(dataframe$vol_geo)),
        row.names = c("Mínimo", "1º Quartil", "Mediana", "Média", "3º Quartil", "Máximo")
    ) |>
        tibble::rownames_to_column(var = "Estatistica")

    write.csv2(summary_data, "./output/Planilhas/Tabela_1.csv", row.names = TRUE, fileEncoding = "latin1")

    # set table
    title_table <- paste0("Tabela 1. Estatística descritiva para as principais variáveis do inventário florestal da UPA.")
    subtitle_table <- paste0("Em que: DAP (cm) = Diâmetro a altura do peito calculado em cm; Altura (m) = Altura comercial em metros estimada durante o inventário florestal; g (m²) = Área basal calculada em metros quadrados; Volume (m³)= Volume calculado a partir de equação ajustada para a UMF, unidade em metros cúbicos.")

    summary_table <- summary_data |>
        gt(rowname_col = "Estatistica") |>
        cols_label(
            dap = md("**DAP (cm)**"),
            g = md("**g (m2)**"),
            altura = md("**Altura (m)**"),
            vol_geo = md("**Volume (m3)**")
        ) |>
        cols_width(
            Estatistica ~ px(60),
            everything() ~ px(80)
        ) |>
        tab_header(
            title = html(
                paste("<p style='text-align:justify'><i>", title_table, "</i></p>")
            ),
            subtitle = html(
                paste("<p style='text-align:justify'><i>", subtitle_table, "</i></p>")
            )
        ) |>
        tab_style(
            style = cell_text(style = "italic"),
            locations = list(cells_stubhead(), cells_stub())
        ) |>
        cols_align(align = "center") |>
        tab_style(
            style = list(cell_borders(sides = c("top", "bottom"),
                                      color = "#fff", weight = px(1)),
                         cell_text(align = "center"),
                         cell_fill(color = "#fff", alpha = NULL)),
            locations = cells_body(columns = everything(), rows = everything())
        ) |>
        tab_style(
            style = cell_borders(color = "#fff"),
            locations = cells_stub()
        ) |>
        tab_source_note(
            source_note = html(
                "<div align='center'><i>",
                paste0("Fonte: Inventário Florestal da UPA."),
                "</i></div>"
            )
        ) |>
        tab_style(
            style = cell_borders(color = "#fff"),
            locations = cells_stub()
        ) |>
        tab_options(
            #heading.title.font.size = px(14),
            table.font.size = px(9)
        ) |>
        tab_options(
            heading.border.bottom.width = 2,
            heading.border.bottom.color = "#000000",
            # hide the top-most border
            table.border.top.color = "#fff",
            # change the column labels section
            column_labels.border.top.width = 2,
            column_labels.border.top.color = "#000000",
            column_labels.border.bottom.width = 2,
            column_labels.border.bottom.color = "#000000",
            # Change the vertical lines of summary body
            stub.background.color = "#fff",
            # Change the foot source body
            footnotes.border.bottom.color = "#fff",
            # change the bottom of the body
            table_body.border.bottom.color = "#000000",
            table_body.border.bottom.width = 2,
            # hide the bottom-most line or footnotes
            # will have a border
            table.border.bottom.color = "#fff"
        )

    # Save the Table 1 in .png file format
    gt::gtsave(summary_table, filename = 'Tabela_1.png', path = './output/Tabelas/')
}
