#' @name qf_chart
#'
#' @title Quality of Stem Chart
#'
#' @params data frame with qf column as factor
#'
#' @import ggplot2
#'
#' @import forcats
#'
#' @return a ggplot object
#'
qf_chart <- function(dataframe) {

    output_dir <- "./output/Graficos/Qualidade_de_Fuste/"

    if (!dir.exists(output_dir)) {
        dir.create(output_dir)
    }

    png(
        paste0(output_dir, 'Qualidade_de_Fuste', '.png'),
        width = 6,
        height = 4,
        units = 'in',
        res = 300
    )

    data2plt <- dataframe %>%
        mutate(categoria2 = as.character(as.factor(categoria2)))

    qf_plot <- ggplot(data2plt, aes(forcats::fct_reorder(qf, categoria2), fill = categoria2)) +
        geom_bar(
            position = position_dodge2(preserve = 'total'),
            alpha = 0.75,
            width = 0.70
        ) +
        geom_text(
            aes(label = scales::percent(..count.. / sum(..count..))),
            stat = "count",
            position = position_dodge(width = 0.7),
            vjust = -0.5,
            size = 2.8
        ) +
        theme(
            #axis.text.x = element_text(angle = 50, size = 7),
            axis.text.y = element_text(size = 7, angle = 90, hjust = 0.5),
            axis.title.x = element_text(size = 7),
            axis.title.y = element_text(size = 7),
            plot.title = element_text(hjust = 0.5, size = 9),
            legend.title = element_text(size = 8),
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
