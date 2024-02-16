#' @name basal_area_by_DBH
#' 
#' @title Basal Area by DBH
#' 
#' @description  Generate a bar plot of basal area by diameter at breast height (DBH)
#'
#' This function filters a dataframe to include only relevant data points based
#' on diameter at breast height (dap) and quality factor (qf) to generates a bar
#' plot showing the total basal area for each diameter class in the provided 
#' data frame. The plot is saved as a PNG file.
#'
#' @param df A dataframe containing the data with columns: 
#' dap (diameter at breast height), qf (quality factor), classe2 (tree class)
#' and g (basal area).
#'
#' @return A PNG file containing the bar plot of basal area by DBH.
#'
#' @import ggplot2
#' @import dplyr
#'
basal_area_by_DBH <- function(df) {
    output_dir_plt <- './output/Graficos/Area_basal/'
    if (!dir.exists(output_dir_plt)) {
        dir.create(output_dir_plt, recursive = TRUE)
    }
    
    png(
        paste0(output_dir_plt, 'Area_Basal_dap', '.png'),
        width = 1500,
        height = 950,
        res = 300
    )
    
    basal_area_DBH_plt <- df %>%
        filter(dap >= 50 & as.numeric(as.factor(qf)) <= 2) %>%
        select(classe2, g) %>%
        filter(!is.na(classe2)) %>%
        group_by(classe2) %>%
        summarize(sum_G = sum(g)) %>%
        ggplot(aes(x = classe2, y = sum_G, fill = sum_G)) +
        geom_bar(stat = 'identity', position = position_dodge(0.8)) +
        theme(
            plot.title = element_text(
                color = 'black',
                size = 10,
                hjust = 0.5),
            axis.text.x = element_text(angle = 50, size = 6),
            axis.title.x = element_text(color = 'black', size = 7),
            axis.title.y = element_text(color = 'black', size = 7),
            legend.text = element_text(size = 4),
            legend.title = element_text(size = 5),
            legend.position = 'bottom'
        ) +
        labs(title = 'Área Basal por Classe de DAP',
             x = 'DAP (cm)',
             y = 'Área Basal (m²)',
             fill = 'Área Basal')
    
    print(basal_area_DBH_plt)
    dev.off()
    
}
