#' @name criterion_1015
#' 
#' @title criterion 10% to 15% of trees by species
#' 
#' @description This function receives a forest inventory data frame,
#' filters the tree species that meet the selection criteria for cutting, and
#' returns a data frame and a ggplot object with the percentage of remaining
#' trees by species and whether or not they meet the maintenance criterion from
#' 10% to 15 % of remaining trees by species in the annual production area.
#' 
#' @param dataframe - Forest inventory data frame with columns nome_cientifico,
#' categoria2, status and aem.
#' 
#' @return A ggplot object and a dataframe with the percentage of remaining trees
#' by species and whether or not they meet the maintenance criterion from 10% to
#' 15 % of remaining trees.
#' 
#' @return A data frame and a lollipop Chart
#' @import ggplot2
#' @import dplyr
#' 
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)


criterion_1015 <- function(data_frame) {
crit_10.15 <- data_frame %>%
            filter(dap >= 50 & as.numeric(qf) <= 2) %>%
            select(nome_cientifico, categoria2, status, aem) %>%
            group_by(nome_cientifico) %>%
            mutate(
                Corte = sum(categoria2 == 'Explorar'),
                Remanescente = sum(categoria2 == 'Remanescente'),
                Total = Corte + Remanescente,
                PercRem = round(Remanescente / Total * 100),
                Criterio = if_else(status == 'Não Ameaçada', (10), (15)),
                Analise = if_else(PercRem >= Criterio | Corte == 0, 'Atende', 'Nao Atende')
            ) %>%
            # By default, mutate() keeps all columns from the input data
            # Use '.keep_all' to override it
            distinct(nome_cientifico, .keep_all = TRUE) %>%
            select(-c(aem, categoria2)) %>%
            filter(Corte != 0) %>%
            arrange(nome_cientifico)
    
    output_dir <- './output/Planilhas/'
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
    }
    
    file_name_crit1015 <- paste0(
        output_dir, 'Criterio_10-15_Porcento', '.csv'
    )

    file_name_crit1015_not <- paste0(
            './output/Pendencias/', 'Criterio_10-15_Porcento', '.csv'
    )

    if (nrow(crit_10.15[crit_10.15$Analise == 'Nao Atende', ])) {
        write.csv2(crit_10.15[crit_10.15$Analise == 'Nao Atende', ],
                   file = file_name_crit1015_not,
                   row.names = FALSE,
                   na = 'NA',
                   fileEncoding = 'latin1')
    }

    write.csv2(crit_10.15,
               file = file_name_crit1015,
               row.names = FALSE,
               na = 'NA',
               fileEncoding = 'latin1'
    )

    
    # Lollipop Chart
    output_dir <- './output/Graficos/Criterio_10_a_15_Porcento/'
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
    }
    
    png(paste0(output_dir, 'Criterio_10_a_15_Porcento', '.png'),
        width = 1500,
        height = 1500,
        res = 300)
    
    crit_10.15_plt <- crit_10.15 %>%
            rename(Análise = Analise) %>%
            mutate(
                nome_cientifico = ifelse(!status == 'Não Ameaçada',
                                         paste(nome_cientifico, '*'),
                                         nome_cientifico)
            ) %>%
            ggplot(aes(x = PercRem, y = nome_cientifico)) +
            geom_point(size = 1, color = 'tomato') +
            geom_segment(
                aes(x = Criterio,
                    xend = PercRem,
                    y = nome_cientifico,
                    yend = nome_cientifico),
                color = 'steelblue'
            ) +
            geom_text(
                color = 'purple',
                size = 2,
                hjust = -0.25,
                aes(label = paste0(PercRem, '%'))
            ) +
            scale_x_continuous(breaks =  seq(0, 100, 5)) +
            labs(
                x = '',
                y = NULL,
                title = 'Percentual de Árvores Remanescentes',
                caption = '* Espécie Vulnerável'
            ) +
            theme(
                plot.title = element_text(size = 10, hjust = 0.5),
                plot.background = element_rect(fill = '#f7f7f7'),
                panel.background = element_rect(fill = '#D8D8D8'), #f7f7f7
                panel.grid.minor = element_blank(),
                #panel.grid.major.y = element_blank(),
                panel.grid.major.x = element_line(),
                axis.ticks.x = element_blank(),
                axis.text.x = element_blank(),
                axis.text.y = element_text(size = 8, face = 'italic'),
                plot.caption = element_text(size = 6, face = 'italic'),
                legend.position = 'top',
                panel.border = element_blank()
            )
    
    print(crit_10.15_plt)
    dev.off()
        
}
