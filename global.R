# Load Packages
suppressPackageStartupMessages(library(tidyverse, warn.conflicts = FALSE))
library(stringr, warn.conflicts = FALSE)
library(ggalt, warn.conflicts = FALSE)
library(gt, warn.conflicts = FALSE)
library(sjmisc, warn.conflicts = FALSE)
library(magrittr, warn.conflicts = FALSE)


# Load data from the app
my_data <- NULL

setUploadedData <- function(data) {
        my_data <<- data
        
        my_data %<>%
                mutate(categoria = R.utils::capitalize(categoria)) %>%
                mutate()
        
}
