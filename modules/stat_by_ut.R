#' @name stat_by_ut
#' 
#' @title Descriptive statistics by `ut`
#' 
#' @description This function receives a data frame with the columns `ut`, `dap`, `g`,
#' `altura`, and `vol_geo` and makes descriptive statistics of these columns
#' grouped by the "ut" column.
#' 
#' @param dataframe - Forest inventory data frame with columns `ut`, `dap`, `g`,
#' `altura`, and `vol_geo`
#' 
#' @return A data frame object containing the summary statistics of columns `dap`, `g`,
#' `altura`, and `vol_geo`, grouped by the `ut` column. 
#' 
#' @import ggplot2
#' @import dplyr
#' 
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)


stat_ut <- function(df) {
    summary_by_ut <- df %>%
    group_by(ut) %>%
    summarize(
        n = n(),
        dap_media = mean(dap),
        dap_mediana = median(dap),
        dap_sd = sd(dap),
        g_media = mean(g),
        g_mediana = median(g),
        g_desvio_padrao = sd(g),
        g_total = sum(g),
        altura_media = mean(altura),
        altura_mediana = median(altura),
        altura_desvio_padrao = sd(altura),
        Volume_medio = mean(vol_geo),
        Vol_mediana = median(vol_geo),
        Volume_desvio_padrao = sd(vol_geo)
    ) %>%
    arrange(ut)
    
    output_dir <- './output/Planilhas/'
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, output_dir, recursive = TRUE)
    }
    
    write.csv2(summary_by_ut,
               paste0(output_dir, 'estatistica_por_ut.csv'),
               row.names = FALSE,
               fileEncoding = 'latin1')
}
