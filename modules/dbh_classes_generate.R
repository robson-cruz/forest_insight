#' dbh_class function
#' 
#' @description 
#' This function takes any data frame from a global forest inventory with DBH of 
#' trees and returns a new data frame containing the variable class (class of DBH)
#' for each tree.
#' 
#' @param dataset
#' 
#' 

library(dplyr, warn.conflicts = FALSE)
library(magrittr)


# Set the function
dbh_class <- function(df) {
        
        limits <- seq(10, 1000, 10)
        labels <- paste0(limits[-length(limits)], "-", limits[-1])
        labels2 <- append(labels, ">200")
        
        df$classe <- cut(df$dap, breaks = limits, labels = labels, right = FALSE)
        
        # Set class column as factor
        df$classe <- factor(df$classe, levels = labels, ordered = TRUE)
        
        # Set class2
        df %<>%
                mutate(classe2 = if_else(dap > 200, ">200", as.character(classe))) %>%
                mutate(classe2 = factor(classe2, levels = labels2, ordered = TRUE)) %>%
                select(num_arvore, flona, umf, upa, ut, nome_cientifico, familia,
                       status, dispositivo_legal, nome_popular, cap, dap, classe,
                       classe2, altura, qf, g, volume, categoria, lat, lon, norte,
                       leste, zona_utm, mc, typo, isSynonym, nome_aceito, familia,
                       status_conservacao, dispositivo_legal, status, aem)
        
        # Assign the local copy of the data frame to the R global environment
        #df <<- df
        assign('df', df, envir = .GlobalEnv)
}
