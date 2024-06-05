#' @name dbh_classes_chart
#'
#' @title DBH Class Chart
#'
#' @description This function takes a data frame with a DBH Class column and makes
#' a bar chart with the DBH classes distribution of trees.
#'
#' @param df Data frame with DBH class column.
#' @param threshold DAP threshold for filtering.
#' @param output_dir Directory to save the plot.
#'
#' @return A ggplot object representing the DBH classes distribution of trees.
#'
library(dplyr)
library(ggplot2)


dbh_classes_chart <- function(dataframe, threshold = 40) {
    output_dir <- './output/Graficos/Distribuicao_Diametrica/'
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
    }

    # Filter data
    df_filtered <- dataframe[dataframe$dap >= threshold, ] %>%
            group_by(classe2) %>%
            summarize(N = n())

    # Create ggplot and save as an image
    png(
            paste0(output_dir,
                   'Distribuicao_Dap', '.png'),
            width = 6,
            height = 4,
            units = "in",
            res = 300
    )
    DBH_classes_plot <- ggplot(df_filtered, aes(x = classe2, y = N)) +
            geom_bar(stat = 'identity', fill = 'steelblue') +
            scale_y_continuous(labels = scales::label_number(big.mark = '.')) +
            theme(
                    axis.text.x = element_text(angle = 50, size = 7),
                    axis.text.y = element_text(size = 7, angle = 90, hjust = 0.5),
                    axis.title.x = element_text(size = 9),
                    axis.title.y = element_text(size = 9),
                    plot.title = element_text(hjust = 0.5, size = 10)
            ) +
            labs(
                    title = 'Distribuição Diamétrica',
                    x = 'Classes de Diâmetro (cm)',
                    y = 'Número de Árvores'
            )

    print(DBH_classes_plot)

    dev.off()

}
