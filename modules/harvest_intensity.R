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
    
    intensidade_corte_dir <- "./output/Intensidade_Corte/"
    if (!dir.exists(intensidade_corte_dir)) dir.create(intensidade_corte_dir)
    
    # Calculate total volume of logging
    volume_upa <- dataframe |>
        filter(categoria2 == "Explorar") |>
        summarize(sum_volume = sum(vol_geo))
    
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
    
    output_text <- paste(
        "Os dados do inventário florestal mostram uma intensidade de corte de",
        intensity, "m³/ha."
    )
    
    # Write the output to a text file
    writeLines(output_text,
               con = paste0(intensidade_corte_dir, "Intensidade_Corte.txt"))
    
    return(intensity)
}
