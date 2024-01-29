#' @description
#' `crit_34` This function receives a forest inventory data frame,
#' filters the tree species that meet the selection criteria for cutting, and
#' returns a data frame with the remaining trees by species and
#' whether or not they meet the maintenance criterion from 3 to 4 trees of
#' remaining trees by species in the plot (`ut`).
#' 
#' @param dataframe - Forest inventory data frame
#' @return A data frame with the percentage of remaining trees by species and
#' whether or not they meet the maintenance criterion from 10% to 15 % of
#' remaining trees.
#' @return A lollipop Chart
#' @author Robson Cruz


library(dplyr, warn.conflicts = FALSE)


crit_34 <- function(df) {
        crit_3.4 <- df %>%
                filter(dap >= 50 & as.numeric(qf) <= 2) %>%
                select(ut, nome_cientifico, categoria, status, aem) %>%
                group_by(ut, nome_cientifico) %>%
                mutate(
                        Corte = sum(categoria == 'Explorar'),
                        Remanescente = sum(categoria == 'Remanescente'),
                        Total = Corte + Remanescente,
                        Criterio = if_else(status == 'Não Ameaçada',
                                           round(3 * aem / 100, digits = 0),
                                           round(4 * aem / 100, digits = 0)),
                        Analise = if_else(Remanescente >= Criterio | Corte == 0,
                                          'Atende',
                                          'Nao Atende')
                ) %>%
                # By default, mutate() keeps all columns from the input data
                # Use '.keep_all' to override it
                distinct(aem, .keep_all = TRUE) %>%
                select(-c(aem, categoria)) %>%
                select(ut, nome_cientifico, status, Total, Corte, Remanescente,
                       Criterio, Analise)
        
        file_name_crit34 <- paste0(
                './output/dataFrame/crit34_UPA', '_umf_', umf,
                '_upa_', upa, '_', flona, '.csv'
        )
        
        file_name_crit34_not <- paste0(
                './output/warnings/crit34_UPA', '_umf_', umf,
                '_upa_', upa, '_', flona, '.csv'
        )
        
        if (nrow(crit_3.4[crit_3.4$Analise == 'Nao Atende', ])) {
                write.csv2(
                        crit_3.4[crit_3.4$Analise == 'Nao Atende', ],
                        file = file_name_crit34_not,
                        row.names = FALSE,
                        na = 'NA',
                        fileEncoding = 'latin1'
                )
        }
        
        write.csv2(
                crit_3.4,
                file = file_name_crit34,
                row.names = FALSE,
                na = 'NA',
                fileEncoding = 'latin1'
        )
}
