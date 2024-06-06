#' @name
#' @title Calculate and plot basal area by unit work (ut)
#'
#' @description
#' This function calculates the basal area by unit work (ut) from a given forest
#' inventory dataframe and generates a bar plot to visualize the basal area
#' distribution across UTs.
#'
#' @param dataframe A dataframe containing at least two columns: 'ut' and 'g' (basal area).
#' @return A ggplot object of basal area by ut, saved as a PNG file.
#'
#' @import ggplot2
#' @importFrom dplyr select, summarize, group_by
#' @examples
#' # Load required packages
#' library(dplyr)
#' library(ggplot2)
#'
#' # Generate example dataframe
#' example_data <- data.frame(ut = c("A", "B", "A", "B", "C", "D", "D"),
#'                             g = c(10, 15, 12, 18, 20, "27", "16"))
#'
#' # Calculate and plot basal area by UT
#' basal_area_ut(example_data)
#'
#' @importFrom utils dir.create
basal_area_ut <- function(dataframe) {

    FI_basal_area <- dataframe %>%
        select(ut, g) %>%
        summarize(G_avg = sum(g)/length(unique(dataframe$ut))) # Get total mean basal area

    output_dir_plt <- "./output/Graficos/Area_basal/"
    if (!dir.exists(output_dir_plt)) {
        dir.create(output_dir_plt)
    }

    png(paste0(output_dir_plt, "Area_basal_UT", ".png"),
        width = 6,
        height = 4,
        units = "in",
        res = 300)

    BasalArea_by_ut <- dataframe %>%
        select(ut, g) %>%
        group_by(ut) %>%
        summarize(sum_G = sum(g)) %>%
        ggplot(
            aes(x = as.factor(ut), y = sum_G, fill = sum_G > FI_basal_area$G_avg)
        ) +
        geom_bar(position = position_dodge2(preserve = "total"), stat = "identity") +
        geom_hline(
            aes(
                yintercept = FI_basal_area$G_avg,
                color = paste(round(FI_basal_area$G_avg, 2), "m²")
            ),
            linetype = "dashed",
            linewidth = 0.4
        ) +
        scale_fill_manual(
            name = "",
            values = c("#778899", "#4682B4"),
            labels = c("TRUE" = "Acima da Média", "FALSE" = "Abaixo da Média")
        ) +
        theme(
            plot.title = element_text(
                family = "Arial",
                color = "#000000",
                size = 10,
                hjust = 0.5),
            axis.title.x = element_text(size = 9),
            axis.title.y = element_text(size = 9, angle = 90, hjust = 0.5),
            axis.text.x = element_text(size = 7),
            axis.text.y = element_text(size = 7),
            legend.position = "bottom",
            legend.title = element_text(size = 7),
            legend.text = element_text(size = 7),
            legend.key.height = unit(6, "mm")
        ) +
        labs(
            title = "Área Basal por UT",
            y = "Área Basal (m²)",
            x = "UT",
            color = "Área Basal Média"
        )

    print(BasalArea_by_ut)

    dev.off()
}
