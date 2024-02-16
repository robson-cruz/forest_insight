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
drop_duplicated_rows <- function(dataframe) {

    output_dir_not <- './output/Pendencias/'
    if (!dir.exists(output_dir_not)) {
        dir.create(output_dir_not, recursive = TRUE)
    }

        # Check out for duplicated rows
        if (sum(duplicated(dataframe$num_arvore)) > 0) {

                # Save the duplicated row
                d <- dataframe[duplicated(dataframe$num_arvore), ]
                write.csv2(d, paste0(output_dir_not, "Arvores_Placa_Duplicada.csv"),
                           fileEncoding = "latin1",
                           row.names = FALSE)

                # Drop Partial Duplicates
                dataframe %<>%
                        distinct(num_arvore, .keep_all = TRUE)

                # Assign the local copy of df to the global R environment
                assign('dataframe', dataframe, envir = .GlobalEnv)

        }
}
