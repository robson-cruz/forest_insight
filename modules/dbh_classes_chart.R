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
dbh_classes_chart <- function(df, threshold = 40) {
        
        # Filter data
        df_filtered <- df[df$dap >= threshold, ] %>%
                group_by(classe2) %>%
                summarize(N = n())
        
        # Create ggplot and save as an image
        png(
                paste0('./output/Graficos/Distribuicao_Diametrica/',
                       'Distribuicao_Diametrica', '.png'),
                width = 1500,
                height = 950,
                res = 300
        )
        DBH_classes_plot <- ggplot(df_filtered, aes(x = classe2, y = N)) +
                geom_bar(stat = 'identity', fill = 'steelblue') +
                scale_y_continuous(labels = scales::label_number(big.mark = '.')) +
                theme(
                        axis.text.x = element_text(angle = 50, size = 7),
                        axis.text.y = element_text(size = 8),
                        axis.title.x = element_text(size = 8),
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

        assign('DBH_classes_plot', 'DBH_classes_plot', envir = .GlobalEnv)
}
