#' drop_duplicated function
#' 
#' @description 
#' This function takes any data frame from a global forest inventory with duplica- 
#' ted rows returns a new data frame containing only unique rows as also save the 
#' duplicated rows as .csv file in a folder called 'alert'.
#' 
#' @param dataset
#' 
#' 

# Set function to remove duplicates rows
drop_duplicated <- function(df) {
        
        # Check out for duplicated rows
        if (sum(duplicated(df$num_arvore)) > 0) {
                
                # Save the duplicated row
                d <- df[duplicated(df$num_arvore), ]
                if (!dir.exists('./output/warnings')) {
                        dir.create('./output/warnings')
                        write.csv2(d, './output/warnings/duplicated_rows.csv',
                                   row.names = FALSE)
                } else {
                        write.csv2(d, './output/warnings/duplicated_rows.csv',
                                   row.names = FALSE)
                }
                
                
                # Drop Partial Duplicates
                df <- df %>%
                        distinct(num_arvore, .keep_all = TRUE)
                
                # Assign the local copy of df to the global R environment
                assign('df', df, envir = .GlobalEnv)
                
        } else {
                df
        }
}
