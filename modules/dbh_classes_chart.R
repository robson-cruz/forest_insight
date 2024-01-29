#' @name dbh_class_chart
#' 
#' @title DBH Class Chart
#' 
#' @description This function takes a data frame with DBH Class column and makes 
#' a bar chart with the DBH classes distribution of trees.
#' 
#' @param dataframe with DBH class column
#' 
#' @return a bar chart with the DBH classes distribution of trees
#' 
dbh_class_chart <- function(df) {
        png(
                paste0('./saidas/graficos/',
                       '_umf_', umf, '_upa_', upa , '_', flona, '.png'),
                width = 1500,
                height = 950,
                units = 'px',
                res = 300
        )
        
        DBH_classes_plt <- df %>%
                filter(dap >= 40) %>%
                group_by(classe, dap) %>%
                summarize(N = n()) %>%
                ggplot(aes(x = classe)) +
                geom_bar(stat = 'identity', aes(y = N), fill = 'steelblue') +
                scale_y_continuous(
                        labels = scales::label_number(big.mark = '.')
                ) +
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
        
        print(DBH_classes_plt)
        
        dev.off()
}

