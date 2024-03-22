#' @name check_threatened_species_for_logging
#' 
#' @title Check for Threatened Species and Log Discrepancies
#'
#' This function checks whether there are threatened species that have been
#' selected for harvesting in the forest inventory and logs any discrepancies or errors found.
#'
#' @param dataframe A dataframe containing information about trees,
#' their scientific names, categories, and conservation statuses.
#' 
#' @return This function doesn't return any value explicitly, but it logs information
#' about threatened species and potential typos in scientific names into text files.
#' 
#' @export
#' 
#' @examples
#' dataframe <- data.frame(
#'   num_arvore = c(1:30),
#'   nome_cientifico = rep(c("Bertholletia excelsa", "Dinizia excelsa", "Vouacapoua americana", "Hymenaea courbaril"), times = 3),
#'   nome_aceito = rep(c("Bertholletia excelsa", "Dinizia excelsa", "Vouacapoua americana", "Hymenaea courbaril"), times = 3),
#'   categoria2 = rep(c("Explorar", "Remanescente"), times = 30),
#'   status_conservacao = ""
#' )
#' 
#' dataframe$status_conservacao <- ""
#' dataframe[dataframe$nome_cientifico == "Bertholletia excelsa", ]$status_conservacao <- "Vulnerável"
#' dataframe[dataframe$nome_cientifico == "Hymenaea courbaril", ]$status_conservacao <- "Não ameaçada"
#' dataframe[dataframe$nome_cientifico == "Dinizia excelsa", ]$status_conservacao <- "Não ameaçada"
#' dataframe[dataframe$nome_cientifico == "Vouacapoua americana", ]$status_conservacao <-  "Em Perigo"
#' 
#' check_threatened_species_for_logging(dataframe)
#' 
#' @importFrom glue glue_collapse
#' @importFrom base writeLines
#' @importFrom utils write.csv2 dir.create
#' @keywords threatened species conservation logging
check_threatened_species_for_logging <- function(dataframe) {
    
    check_1 <- dataframe$categoria2 == "Explorar"
    check_2 <- dataframe$nome_cientifico == "Bertholletia excelsa"
    check_3 <- dataframe$nome_aceito == "Bertholletia excelsa"
    check_4 <- dataframe$status_conservacao == "Em Perigo"
    
    df <- dataframe[check_1 & check_2 | check_3 | check_4, ]
    
    df_output <- df[df$categoria2 == "Explorar", ]
    
    if (nrow(df_output) > 0) {
        issue_dir <- './output/Pendencias/'
        if (!dir.exists(issue_dir)) {
            dir.create(issue_dir, recursive = TRUE)
        }
        write.csv2(
            df_output,
            paste0(issue_dir, "Especies_Proibidas_Corte_a_Explorar.csv"),
            row.names = FALSE, fileEncoding = "latin1"
        )
    }
    
    check_typo <- function() {
        
        typo <- df_output[df_output$nome_cientifico != df_output$nome_aceito, ]
        
        if (nrow(typo) > 0) {
            species_typo <<- unique(typo$nome_cientifico)
            accepted_name <<- unique(typo$nome_aceito)
            
            return(TRUE)
        }
        else {
            return(FALSE)
        }
    }
    
    save_text <- function(text) {
        writeLines(
            text = text,
            con = paste0(issue_dir, "Especies_Proibidas_Corte_a_Explorar.txt")
        )
    }
    
    if (nrow(df_output) > 0 & length(unique(df_output$nome_aceito)) == 1) {
        if (check_typo()) {
            output_text <<- paste0(
                "No inventário florestal constam árvores selecionadas para corte da espécie ",
                glue::glue_collapse(unique(df_output$nome_aceito), last = " e "),
                ", no entanto, esta espécie é proíbida de corte. Registra-se nesta análise que há erro de digitação, a saber: ",
                unique(accepted_name),
                " foi grafada de forma incorreta como ",
                "\"", unique(species_typo), "\"."
            )
            save_text(output_text)
        } 
        else {
            output_text <<- paste0(
                "No inventário florestal constam árvores selecionadas para corte da espécie ",
                unique(df_output$nome_cientifico)
            )
            save_text(output_text)
        }
    
    }
    else if (nrow(df_output) > 0 & length(unique(df_output$nome_aceito)) > 1) {
        if (check_typo()) {
            if (length(species_typo) == 1) {
                output_text <<- paste0(
                    "No inventário florestal constam árvores selecionadas para corte das espécies ",
                    glue::glue_collapse(df_output$nome_aceito, sep = ", ", last = " e "),
                    ", no entanto, estas espécies são proíbidas de corte. Registra-se nesta análise que há erro de digitação no inventário, a saber: ",
                    accepted_name,
                    " foi grafada de forma incorreta como ",
                    "\"", species_typo, "\"."
                )
                save_text(output_text)
            }
            else {
                output_text <<- paste0(
                    "No inventário florestal constam árvores selecionadas para corte das espécies ",
                    glue::glue_collapse(unique(df_output$nome_aceito), sep = ", ", last = " e "),
                    ", no entanto, estas espécies são proíbidas de corte. Registra-se nesta análise que há erro de digitação no inventário, a saber: ",
                    glue::glue_collapse(unique(accepted_name), last = " e "),
                    " foram grafadas de forma incorreta como ",
                    "\"", glue::glue_collapse(unique(species_typo), last = " e "), "\"."
                )
                save_text(output_text)
            }
        }
        else {
            output_text <<- paste0(
                "No inventário florestal constam árvores selecionadas para corte das espécies ",
                glue::glue_collapse(unique(df_output$nome_cientifico), sep = ", ", last = " e "),
                ", no entanto, estas espécies são proíbidas de corte."
            )
            save_text(output_text)
        }
    }
}
