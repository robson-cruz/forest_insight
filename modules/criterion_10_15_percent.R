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
#' @param dataframe - Forest inventory data frame with columns `nome_cientifico`,
#' `categoria2`, `status_conservacao` and `aem`.
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


criterion_1015 <- function(dataframe) {
    crit_10.15 <- dataframe %>%
        filter(dap >= 50 & as.numeric(as.factor(qf)) <= 2) %>%
        select(nome_cientifico, categoria2, status_conservacao, aem) %>%
        group_by(nome_cientifico) %>%
        # mutate(categoria2 = as.numeric(as.factor(categoria2))) %>%
        mutate(
            Corte = sum(categoria2 == 'Explorar'),
            Remanescente = sum(categoria2 == 'Remanescente'),
            Total = Corte + Remanescente,
            PercRem = ceiling(Remanescente / Total * 100)
        ) %>%
        mutate(
            Criterio = if_else(status_conservacao == 'Não Ameaçada', 10, 15),
            Analise = if_else(PercRem >= Criterio |
                                  Corte == 0, 'Atende', 'Nao Atende')
        ) %>%
        # By default, mutate() keeps all columns from the input data
        # Use '.keep_all' to override it
        distinct(nome_cientifico, .keep_all = TRUE) %>%
        select(-c(aem, categoria2)) %>%
        filter(Corte != 0) %>%
        arrange(nome_cientifico)

    output_dir_spreadsheet <- './output/Planilhas/'
    if (!dir.exists(output_dir_spreadsheet)) {
        dir.create(output_dir_spreadsheet, recursive = TRUE)
    }

    output_dir_not <- './output/Pendencias/'
    if (!dir.exists(output_dir_not)) {
        dir.create(output_dir_not, recursive = TRUE)
    }

    file_name_crit1015 <- paste0(output_dir_spreadsheet, 'Criterio_10-15_Porcento', '.csv')

    file_name_crit1015_not <- paste0('./output/Pendencias/',
                                     'Criterio_10-15_Porcento',
                                     '.csv')

    not <- crit_10.15[crit_10.15$Analise == 'Nao Atende', ]

    if (nrow(not)) {
        write.csv2(
            not,
            file = file_name_crit1015_not,
            row.names = FALSE,
            na = 'NA',
            fileEncoding = 'latin1'
        )
    } else {
        write.csv2(
            crit_10.15,
            file = file_name_crit1015,
            row.names = FALSE,
            na = 'NA',
            fileEncoding = 'latin1'
        )
    }


    # Lollipop Chart
    output_dir_plt <- './output/Graficos/Criterio_10_a_15_Porcento/'
    if (!dir.exists(output_dir_plt)) {
        dir.create(output_dir_plt, recursive = TRUE)
    }

    png(
        paste0(output_dir_plt, 'Criterio_10_a_15_Porcento', '.png'),
        width = 7,
        height = 6,
        units = "in",
        res = 300
    )

    data2plt <- crit_10.15 %>%
        rename(Análise = Analise) %>%
        mutate(
            nome_cientifico = ifelse(!status_conservacao == 'Não Ameaçada',
                                     paste(nome_cientifico, '*'),
                                     nome_cientifico)
        )

    crit_10.15_plt <- ggplot(data2plt, aes(x = nome_cientifico, y = Criterio)) +

        geom_segment(
            aes(x = nome_cientifico, xend = nome_cientifico, y = Criterio, yend = PercRem),
            color = '#aeb6bf',
            linewidth = 0.6
        ) +

        geom_point(aes(x = nome_cientifico, y = Criterio,
                       color = "Critério 10% a 15%",
                       fill = "Critério 10% a 15%"),
                   size = 1.5,
                   shape = 21,
        ) +

        geom_point(aes(x = nome_cientifico, y = PercRem,
                       color = "Percentual Remanescente",
                       fill = "Percentual Remanescente"),
                   size = 1.5,
                   shape = 21) +

        scale_color_manual(name = "",
                           values = c("Critério 10% a 15%" = "#3EC70B",
                                      "Percentual Remanescente" = "#5D3587")) +

        scale_fill_manual(name = "",
                          values = c("Critério 10% a 15%" = "#3EC70B",
                                     "Percentual Remanescente" = "#5D3587")) +

        geom_text(
            aes(x = nome_cientifico, y = PercRem, label = paste0(PercRem, '%')),
            color = '#5D3587',
            size = 1.6,
            hjust = ifelse(data2plt$PercRem <= min(data2plt$PercRem), 1.6, -0.2),
            nudge_y = 1
        ) +

        coord_flip() +

        scale_y_continuous(breaks =  seq(10, 100, 10),
                           labels = scales::label_percent(scale = 1)) +

        annotate(
            geom = "rect",
            xmin = 1,
            xmax = nrow(data2plt),
            ymin = min(data2plt$Criterio),
            ymax = max(data2plt$Criterio),
            alpha = 0.25,
            fill = '#393E46'
        ) +

        # annotate(
        #     "text",
        #     x = Inf, y = -Inf,
        #     hjust = 0, vjust = 0,
        #     label = ifelse(nrow(not) > 0, "Não Atende", "Atende"),
        #     color = ifelse(nrow(not) > 0, "red", "black")
        # ) +

        labs(
            x = NULL,
            y = NULL,
            title = 'Percentual de Árvores Remanescentes',
            caption = '* Espécie Vulnerável'
        ) +

        theme(
            plot.title = element_text(size = 10, hjust = 0.5),
            panel.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
            #panel.grid.minor = element_blank(),
            #panel.grid.major.y = element_blank(),
            #panel.grid.major.x = element_blank(),
            #axis.ticks.x = element_blank(),
            #axis.ticks.y = element_line(linewidth = 0.4),
            #axis.ticks.x = element_line(linewidth = 0.4),
            axis.ticks.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.x = element_text(size = 7),
            axis.text.y = element_text(size = 7,
                                       face = 'italic',
                                       color = ifelse(data2plt$Análise == "Nao Atende",
                                                      "red", "black")),
            plot.caption = element_text(size = 7, face = 'italic'),
            legend.position = 'bottom',
            legend.title = element_text(size = 7),
            legend.text = element_text(size = 7),
            #legend.background = element_rect(color = '#0F1035'),
            legend.key = element_blank()
        )

    print(crit_10.15_plt)
    dev.off()

}
