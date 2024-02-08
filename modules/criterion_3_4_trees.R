#' @name crit_34
#' 
#' @title Criterion 3 to 4 trees
#' 
#' @description This assess whether trees in a forest inventory dataset meet a
#' specific criterion (3 or 4 trees by species) related to their abundance,
#' `qf`, ecological status and diameter. 
#' 
#' @param dataframe - Forest inventory data frame
#' 
#' @import dplyr
#' @import tools
#' 
#' @return A data frame with the percentage of remaining trees by species and
#' whether or not they meet the maintenance criterion from 3 to 4 trees by species
#' remaining trees.
#' @return A lollipop Chart
#' 
library(dplyr, warn.conflicts = FALSE)


crit_34 <- function(df) {
    crit_3.4 <- df %>%
        filter(dap >= 50 & as.numeric(as.character(qf)) <= 2) %>%
        select(ut, nome_cientifico, categoria2, status, aem) %>%
        group_by(ut, nome_cientifico) %>%
        mutate(
            Corte = sum(categoria2 == 'Explorar'),
            Remanescente = sum(categoria2 == 'Remanescente'),
            Total = Corte + Remanescente
        ) %>%
        mutate(
            Criterio = ifelse(status == 'Não Ameaçada',
                              ceiling(3 * aem / 100),
                              ceiling(4 * aem / 100))
        ) %>%
        mutate(
            Analise = ifelse(Remanescente >= Criterio | Corte == 0,
                             'Atende',
                             'Nao Atende')
        ) %>%
        distinct(nome_cientifico, .keep_all = TRUE) %>%
        select(-c(aem, categoria2)) %>%
        select(ut, nome_cientifico, status, Total, Corte, Remanescente, Criterio, Analise) %>%
        rename_with(~ tools::toTitleCase((gsub("_", " ", .x, fixed = TRUE)))) %>%
        rename(ut = Ut)
    
    dir_output <- './output/Planilhas/'
    
    if (!dir.exists(dir_output)) {
        dir.create(dir_output, recursive = TRUE)
    }
    
    dir_pendencia <- './output/Pendencias/'
    if (!dir.exists(dir_pendencia)) {
        dir.create(dir_pendencia, recursive = TRUE)
    }
    
    if (nrow(crit_3.4[crit_3.4$Analise == 'Nao Atende', ])) {
        write.csv2(
            crit_3.4[crit_3.4$Analise == 'Nao Atende', ],
            file = paste0(dir_pendencia, 'Criterio_3-4_Arvores_Pendencia.csv'),
            row.names = FALSE,
            na = 'NA',
            fileEncoding = 'latin1'
        )
    }
    
    write.csv2(
        crit_3.4,
        file = paste0(dir_output, 'Criterio_3-4_Arvores.csv'),
        row.names = FALSE,
        na = 'NA',
        fileEncoding = 'latin1'
    )
}

