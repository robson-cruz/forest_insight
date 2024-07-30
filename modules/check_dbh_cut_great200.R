#' @name dbh_gt200
#' 
#' @title Filter trees with diameter at breast height greater than 200 cm
#'
#' @description
#' This function filters trees from a dataframe that are selected for cutting
#' and have a diameter at breast height (dbh - dap in Portuguese language) greater than 200 cm. It creates
#' a CSV file containing the filtered trees and a text file with the count of trees meeting the criteria.
#'
#' @param dataframe The dataframe containing tree data.
#'
#' @return None
#' @export
#'
#' @examples
#' # Read data
#' df <- read.csv2("Inventario_florestal.csv", fileEncoding = "latin1")
#'
#' # Filter trees with dbh greater than 200 cm selected for cutting
#' dbh_gt200(df)
#'
dbh_gt200 <- function(dataframe) {
    
    # Filters trees selected for cutting with a diameter greater than 200 cm
    dbh_gt200 <- dataframe[dataframe$classe2 == ">200" & dataframe$categoria2 == "Explorar", ]
    
    issue_dir <- "output/Pendencias/"
    if (!dir.exists(issue_dir)) dir.create(issue_dir)
    
    if (nrow(dbh_gt200) > 0) {
        write.csv2(
            dbh_gt200,
            paste0(issue_dir, "Arvores_Corte_Dap_Maior_200cm.csv"),
            fileEncoding = "latin1",
            row.names = FALSE
        )
        
        writeLines(
            text = paste("No inventário florestal consta(m)", nrow(dbh_gt200),
                         "árvore(s) selecionada(s) para corte com dap maior do que 200cm."),
            con = paste0(issue_dir, "Arvores_Corte_Dap_Maior_200cm.txt")
        )
    }
}
