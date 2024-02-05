#' @name qf_chart
#'
#' @title Quality of Stem Chart
#'
#' @params data frame with qf column as factor
#'
#' @import ggplot2
#'
#' @return a ggplot object
#' 
qf_chart <- function(df) {
        png(
                paste0('./output/Graficos/Qualidade_de_Fuste/', 'Qualidade_de_Fuste', '.png'),
                width = 1500,
                height = 950,
                res = 300
        )
        
        qf_plot <- ggplot(df, aes(qf, fill = categoria2)) +
                geom_bar(
                        position = position_dodge2(preserve = 'total'),
                        alpha = 0.75,
                        width = 0.70
                ) +
                theme(
                        #axis.text.x = element_text(angle = 50, size = 7),
                        axis.text.y = element_text(size = 8),
                        axis.title.x = element_text(size = 8),
                        axis.title.y = element_text(size = 9),
                        plot.title = element_text(hjust = 0.5, size = 10),
                        legend.position = 'bottom'
                ) +
                labs(
                        title = 'Qualidade de Fuste e Seleção para Corte',
                        x = 'Qualidade de Fuste',
                        y = 'Número de Árvores',
                        fill = 'Seleção'
                )
        
        # Close the graphics device and assign the object to the environment
        print(qf_plot)
        dev.off()
}

