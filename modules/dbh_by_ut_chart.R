#' @name dbh_over_ut
#'
#' @description This function takes a data frame with a 'classe2' (DBH class)
#' and 'ut' columns and makes a bar chart with the DBH classes distribution of
#' trees over each 'ut'.
#'
#' @param df Data frame with 'classe2' and 'ut' columns.
#'
#' @import ggplot2
#' @import dplyr
#'
#' @return A ggplot object representing the DBH classes distribution of trees over
#' each 'ut'.
#'
dbh_over_ut <- function(dataframe) {
    output_dir_plt <- "./output/Graficos/Distribuicao_Diametrica/"
    if (!dir.exists(output_dir_plt)) {
        dir.create(output_dir_plt, recursive = TRUE)
    }
    
    # Open the graphic devices to save the box plot
    png(
        paste0(output_dir_plt,
               "Distribuicao_Dap_UT", ".png"),
        width = 6,
        height = 4,
        units = 'in',
        res = 300
    )

    # Set the chart
    BoxPlot_DBH_by_Plt <- dataframe %>%  # DBH Classes Box plot
        ggplot(aes(x = as.factor(ut), y = dap)) +
        #geom_violin(aes(color = as.factor(ut), alpha = 0.8)) +
        geom_boxplot(outlier.size = 0.7, staplewidth = 0.5) +
        #scale_fill_brewer(palette = 'Paired') +
        #geom_boxplot(width = 0.3, outlier.size = -1) +
        labs(
            title = 'Distribuição da Variável DAP por UT',
            x = 'UT', y = 'DAP (cm)'
        ) +
       # scale_y_continuous(breaks =  seq(0, 600, 50))
        theme(
            legend.position = 'none',
            plot.title = element_text(hjust = 0.5, size = 10),
            axis.title.x = element_text(size = 9),
            axis.title.y = element_text(size = 9),
            axis.text.x = element_text(size = 7),
            axis.text.y = element_text(size = 7)
        )

    # Close the graphic devices and returns the plot
    print(BoxPlot_DBH_by_Plt)
    dev.off()

}
