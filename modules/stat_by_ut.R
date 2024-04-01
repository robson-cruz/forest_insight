#' @name stat_by_ut
#'
#' @title Descriptive statistics by `ut`
#'
#' @description This function receives a data frame with the columns `ut`, `dap`, `g`,
#' `altura`, and `volume` and makes descriptive statistics of these columns
#' grouped by the "ut" column.
#'
#' @param dataframe - Forest inventory data frame with columns `ut`, `dap`, `g`,
#' `altura`, and `volume`
#'
#' @return A data frame object containing the summary statistics of columns `dap`, `g`,
#' `altura`, and `volume`, grouped by the `ut` column.
#'
#' @import ggplot2
#' @import dplyr
#' @import tools
#'
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)


stat_ut <- function(dataframe) {
    summary_by_ut <- dataframe %>%
    group_by(ut) %>%
    summarize(
        n = n(),
        dap_min = min(dap),
        dap_max = max(dap),
        dap_media = mean(dap),
        dap_sd = sd(dap),
        g_min = min(g),
        g_max = max(g),
        g_media = mean(g),
        g_desvio_padrao = sd(g),
        g_total = sum(g),
        altura_min = min(altura),
        altura_max = max(altura),
        altura_media = mean(altura),
        altura_desvio_padrao = sd(altura),
        Volume_min = min(volume),
        Volume_max = max(volume),
        Volume_medio = mean(volume),
        Volume_desvio_padrao = sd(volume)
    ) %>%
        rename_with(~ tools::toTitleCase((gsub("_", " ", .x, fixed = TRUE)))) %>%
        rename(ut = Ut) %>%
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
