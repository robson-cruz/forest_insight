#' @name drop_duplicated_rows
#'
#' @title Drop Duplicated Rows
#'  
#' @description 
#' This function takes any data frame from a global forest inventory with duplica- 
#' ted rows returns a new data frame containing only unique rows as also save the 
#' duplicated rows as .csv file in a folder called 'alert'.
#' 
#' @param data frame
#' 
#' 
drop_duplicated_rows <- function(df) {
        
        # Check out for duplicated rows
        if (sum(duplicated(df$num_arvore)) > 0) {
                
                # Save the duplicated row
                d <- df[duplicated(df$num_arvore), ]
                write.csv2(d, './output/Pendencias/Arvores_Placa_Duplicada.csv',
                           fileEncoding = "latin1",
                           row.names = FALSE)
                
                # Drop Partial Duplicates
                df <- df %>%
                        distinct(num_arvore, .keep_all = TRUE)
                
                # Assign the local copy of df to the global R environment
                assign('df', df, envir = .GlobalEnv)
                
        }
}
