#' @name logging_intensity
#' 
#' @title Calculate the intensity of logging based on input data.
#'
#' @description
#' This function calculates the intensity of logging based on the input data
#' provided in the dataframe. Intensity of logging is defined as the total
#' volume of logging divided by the total area.
#'
#' @param dataframe A dataframe containing the input data.
#'
#' @return logging `intensity`. The function writes the calculated intensity of
#' logging to a text file.
#'
#' @examples
#' # Example usage of logging_intensity function
#' logging_intensity(dataframe)
#'
#' @export
logging_intensity <- function(dataframe) {
    
    logging_intensity_dir <- "./output/Intensidade_Corte/"
    issue_logging_intensity_dir <- "./output/Pendencias/"
    if (!dir.exists(logging_intensity_dir)) dir.create(logging_intensity_dir)
    if (!dir.exists(issue_logging_intensity_dir)) dir.create(issue_logging_intensity_dir)
    
    # Calculate total volume of logging
    volume_upa <- dataframe |>
        filter(categoria2 == "Explorar") |>
        summarize(sum_volume = sum(volume))
    
    # Calculate total area
    area_upa <- dataframe |>
        group_by(ut) |>
        summarize(overall_sum = sum(aem), n = n()) |>
        mutate(sum_ut = overall_sum / n) |>
        summarize(unique_sum_by_ut = sum(sum_ut))
    
    # Extract values from the summary data
    volume <- volume_upa$sum_volume
    area <- area_upa$unique_sum_by_ut
    
    # Calculate intensity of logging
    intensity <- volume / area
    
    # Define texts
    ref <- c(
        "A Norma de Execução IBAMA Nº 1, de 24 abril de 2007 e a Instrução Normativa MMA Nº 5, de 11 de dezembro de 2006, estabelecem os limites máximos de corte:",
        "10 m³/ha para PMFS de baixa intensidade, onde o ciclo de corte deve ser de 10 anos.",
        "30 m³/ha para PMFS Pleno, observando os limites máximo de 35 anos e mínimo 25 anos para ciclo de corte."
    )
    
    if (intensity <= 30) {
        intensity_text <- paste(
            "Os dados do inventário florestal mostram uma intensidade de corte de",
            round(intensity, 2), "m³/ha."
        )
    } else {
        intensity_text <- paste(
            "Os dados do inventário florestal mostram uma intensidade de corte de",
            round(intensity, 2), "m³/ha, o que não está de acordo com as normas para manejo florestal na Amazônia."
        )
        writeLines(
            c(ref, intensity_text),
            con = paste0(issue_logging_intensity_dir, "Intensidade_Corte.txt")
        )
    }
    
    # Write the output to a text file
    writeLines(
        c(ref, intensity_text),
        con = paste0(logging_intensity_dir, "Intensidade_Corte.txt")
    )
    
    return(intensity)
}
