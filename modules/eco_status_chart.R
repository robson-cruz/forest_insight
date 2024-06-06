#' @name eco_status_chart
#'
#' @title Ecological Status Chart
#'
#' @description This function takes a data frame with 'categoria' and 'status'
#' columns and makes a bar chart showing the number of trees over each ecological
#' status.
#'
#' @param df Data frame with 'categoria' and 'status' columns.
#'
#' @return A ggplot object.
#'
eco_status_chart <- function(dataframe) {

    output_dir <- "./output/Graficos/Selecao_Corte/"

    if (!dir.exists(output_dir)) {
        dir.create(output_dir)
    }

    png(
        paste0(output_dir, 'Selecao_Status_Ecologico', '.png'),
        width = 6,
        height = 4,
        units = "in",
        res = 300
    )

    status_cutting_plot <- ggplot(data = dataframe,
                                  aes(status_conservacao,
                                      fill = as.character(categoria2))) +
        geom_bar(
            position = position_dodge2(width = 0.7),
            stat = "count",
            alpha = 0.75,
            width = 0.70
        ) +
        geom_text(
            aes(label = scales::percent(round(..count.. / sum(..count..),3))),
            stat = "count",
            vjust = -0.5,
            position = position_dodge2(width = 0.7),
            size = 2.8
        ) +
        theme(
            axis.text.x = element_text(size = 7),
            axis.text.y = element_text(size = 7, angle = 90, hjust = 0.5),
            axis.title.y = element_text(size = 7),
            plot.title = element_text(hjust = 0.5, size = 9),
            legend.title = element_text(size = 7),
            legend.position = 'bottom'
        ) +
        labs(
            title = 'Categoria Ecológica e Seleção para Corte',
            x = '',
            y = 'Número de Árvores',
            fill = 'Seleção'
        )

    # Close the graphics device and returns the plot
    print(status_cutting_plot)
    dev.off()

}
