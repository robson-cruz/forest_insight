#' @name dbh_class function
#'
#' @description
#' This function takes any data frame from a global forest inventory with DBH of
#' trees and returns a new data frame containing the variable class (class of DBH)
#' for each tree.
#'
#' @param df Data frame with DBH column (dap).
#'
#'
#'

library(dplyr, warn.conflicts = FALSE)
library(magrittr)


# Set the function
dbh_classes_generate <- function(dataframe) {
        # Set breaks and labels for DBH classes
        limits <- seq(10, 1000, 10)
        labels <- paste0(limits[-length(limits)], "-", limits[-1])
        labels2 <- append(labels, ">200")

        # Create a new column 'classe' based on DBH breaks
        dataframe$classe <- cut(dataframe$dap, breaks = limits, labels = labels, right = FALSE)
        #dataframe$classe <- paste0(floor(dap / 10) * 10, "-", floor(dap / 10 ) * 10 + 10)
        
        # Set 'classe' column as factor
        dataframe$classe <- factor(dataframe$classe, levels = labels, ordered = TRUE)

        # Create a new column 'classe2' based on DBH breaks (with >200 category)
        dataframe$classe2 <- ifelse(dataframe$dap > 200, ">200", as.character(dataframe$classe))
        dataframe$classe2 <- factor(dataframe$classe2, levels = labels2, ordered = TRUE)
        
        # Create 'centro_classe' column
        dataframe$centro_classe <- floor(dataframe$dap / 10) * 10 + 5
        
        # Assign the local copy of the data frame to the R global environment
        assign('dataframe', dataframe, envir = .GlobalEnv)
}
