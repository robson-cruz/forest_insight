# Load Packages
suppressPackageStartupMessages(library(tidyverse, warn.conflicts = FALSE))
library(stringr, warn.conflicts = FALSE)
library(ggalt, warn.conflicts = FALSE)
library(gt, warn.conflicts = FALSE)
library(sjmisc, warn.conflicts = FALSE)
library(magrittr, warn.conflicts = FALSE)


# Load data from the app
aem <- NULL

setUploadAem <- function(data) {
        aem <<- data

        assign("aem", aem, inherits = TRUE, envir = .GlobalEnv)
}

usr_data <- NULL

setUploadedData <- function(data) {
    usr_data <<- data

    usr_data %<>%
        mutate(categoria = R.utils::capitalize(categoria)) %>%
        # Diameter at Breast Height - DBH ("dap" in Portuguese)
        mutate(dap = cap / pi) %>%
        # Basal area in square meters
        mutate(g = pi * dap**2 / 40000) %>%
        # Geometric volume
        mutate(vol_geo = g * altura * 0.7) %>%
        # Define 'qf' column as a factor
        mutate(qf = factor(qf, levels = unique(qf))) %>%
        # Define 'Categoria2' column
        mutate(categoria2 = if_else(categoria == "Substituta", "Remanescente", categoria)) %>%
        mutate(categoria2 = factor(categoria2, levels = unique(categoria2)))

    assign("usr_data", usr_data, inherits = TRUE, envir = .GlobalEnv)
}
