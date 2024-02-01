# Load Packages
suppressPackageStartupMessages(library(tidyverse, warn.conflicts = FALSE))
library(stringr, warn.conflicts = FALSE)
library(ggalt, warn.conflicts = FALSE)
library(gt, warn.conflicts = FALSE)
library(sjmisc, warn.conflicts = FALSE)
library(magrittr, warn.conflicts = FALSE)


# Load data from the app
user_data <- NULL

setUploadedData <- function(data) {
        user_data <<- data
        
        user_data %<>%
                mutate(categoria = R.utils::capitalize(categoria)) %>%
                # Diameter at Breast Height - DBH ("dap" in Portuguese)
                mutate(dap = cap / pi) %>%
                # Basal area in square meters
                mutate(g = pi * dap**2 / 40000) %>%
                # Geometric volume
                mutate(vol_geo = g * altura * 0.7) %>%
                # Define 'qf' column as a factor
                mutate(qf = factor(qf, levels = c(1L, 2L, 3L)))
        
        assign("user_data", user_data, inherits = TRUE, envir = .GlobalEnv)
}
