#' @name scientific_name_clean
#'
#' @title Scientific Name Clean
#'
#' @description
#' This function takes a data frame from a global forest inventory and points
#' out typos by using the globalnames API from
#' http://verifier.globalnames.org/data_sources. Identifies taxonomic synonyms and
#' geographical occurrence of species using the REFLORA database.
#'
#' @param dataframe
#'
#'#'@references
#' For information about the REFLORA database, see:
#' \url{https://reflora.jbrj.gov.br/reflora/listaBrasil/ConsultaPublicaUC/ResultadoDaConsultaNovaConsulta.do#CondicaoTaxonCP}
#' For information about the globalnames API, see:
#' \url{https://verifier.globalnames.org/api}
#' 
#'@import httr
#'@import jsonlite
#'@import magrittr
#'@import stringr
#'@import dplyr
#'@importFrom tibble rowid_to_column
#'@importFrom readr read_csv2
#'@importFrom tidyr unnest_wider
#'
library(httr)
library(jsonlite)
library(magrittr)
library(stringr)
library(dplyr, warn.conflicts = FALSE)


scientific_name_clean <- function(dataframe) {
    # Basic data cleaning
    dataframe %<>%
        # remove extra white space between genus and epithet
        mutate(
            nome_cientifico = str_squish(nome_cientifico),
            
            # remove white space from the beginning and end of scientific names
            nome_cientifico = str_trim(nome_cientifico),
            
            # Capitalization inconsistency
            nome_cientifico = tolower(nome_cientifico),
            nome_cientifico = R.utils::capitalize(nome_cientifico),
            
            # get only the scientific name, without taxonomist's names
            nome_cientifico = str_extract(
                nome_cientifico,
            "(\\w+\\s\\w+)(-\\w+)?(\\s\\w+\\ssubsp.\\s\\w+)?(\\ssubsp.\\s\\w+)?(\\svar.\\s\\w+)?(\\.\\s\\w+)?"
            ),
            # Remove sp, sp., spp and spp.
            nome_cientifico = gsub("(\\w+)\\sspp$|\\ssp$||\\ssp.$|\\sspp.$", "\\1", nome_cientifico),
            # Remove typos due SISTAXON list
            nome_cientifico = recode(
                nome_cientifico,
                "Handroanthus impetiginosum" = "Handroanthus impetiginosus",
                
            )
        )
    
    # Get unique scientific names form the input data set
    input_names <- unique(dataframe$nome_cientifico)
    # Get data set column names
    col_names <- names(dataframe)

    # Read The List of Endangered Species, according to the Ordinance of the
    # Brazilian Ministry of Environment (Portaria MMA 443/2014) and Resolution 54/2007
    # from the Pará state Brazil
    port <- readr::read_csv2(
        "https://raw.githubusercontent.com/robson-cruz/endangeredBrazilianPlantSpecies/main/output/Especies_Ameacadas_BRA.csv",
            locale = readr::locale(encoding = "latin1")
    ) %>%
        filter(lista %in% c("Lista Flora Ameaçada Pará", "Lista Portaria 443")) |>
        group_by(nome_cientifico, status_conservacao) |>
        summarise(dispositivo_legal = paste(unique(dispositivo_legal), collapse = ", "))

    # Read "reflora" data set to get specie geographic distribution
    reflora <- readr::read_csv2(
        "https://raw.githubusercontent.com/robson-cruz/endangeredBrazilianPlantSpecies/main/data/reflora_v393_400.csv"
    )
    
    reflora_location <- readr::read_csv2(
        "https://raw.githubusercontent.com/robson-cruz/endangeredBrazilianPlantSpecies/main/data/reflora_distribuicao_v393_400.csv"
    )
    
    reflora_location_by_state <- reflora_location |>
        inner_join(reflora, by = "id") |>
        group_by(specie) |>
        summarize(state = paste(locationID, collapse = ", "))
    
    reflora_not_amz <- reflora_location |>
        mutate(
            occurrenceRemarks = gsub(
                ",", ", ", gsub("\\[|\\]", "",
                                str_extract(occurrenceRemarks, "\\[([A-Za-zà-ÿ, ]*)\\]"))
            )
        ) |>
        mutate(amz_biom = str_extract(occurrenceRemarks, "Amazônia")) |>
        inner_join(reflora, by = "id") |>
        filter(is.na(amz_biom), !is.na(occurrenceRemarks))
    
    # Using POST method in globalnames API
    post_global_names <- httr::POST(
        "https://verifier.globalnames.org/api/v1/verifications",
        config = httr::add_headers(accept = "application/json",
                                   `Content-Type` = "application/json"),
        body = jsonlite::toJSON(list(nameStrings = input_names)
        ))
    
    
    data <- jsonlite::fromJSON(rawToChar(post_global_names$content))$names |>
        rename(submitedName = name) |>
        mutate(
            typo = if_else(matchType == "Fuzzy", TRUE, FALSE)
        ) |>
        select(-c(matchType, curation)) |>
        tidyr::unnest_wider(bestResult) |>
        mutate(
            familia = gsub(".*(\\b\\w+ceae\\b).*", "\\1", classificationPath)
        )

    # Join data
    dataframe %<>%
        tibble::rowid_to_column(var = 'id_tmp') %>%
        left_join(data,
                  by = c("nome_cientifico" = "submitedName"),
                  relationship = "many-to-many") %>%
        distinct(id_tmp, .keep_all = TRUE) %>%
        select(id_tmp, all_of(col_names), typo, familia) %>%
        rename(erro_digitação = typo)
    
    dataframe <- dataframe |>
        left_join(reflora, by = c("nome_cientifico" = "specie"), relationship = "many-to-many") |>
        mutate(specie = if_else(is.na(id), "", paste(genus, specificEpithet, sep = " "))) |>
        distinct(num_arvore, .keep_all = TRUE) |>
        mutate(specie_clean = if_else(!is.na(acceptedNameUsage), acceptedNameUsage, specie)) |>
        mutate(ocor_amz = if_else(specie == "" | specie %in% reflora_not_amz$specie, FALSE, TRUE)) |>
        left_join(reflora_location_by_state, by = c("specie_clean" = "specie"), relationship = "many-to-many") |>
        mutate(ocor_uf_pa_aux = stringr::str_extract(state, "PA")) |>
        mutate(ocor_uf_pa = if_else(is.na(ocor_uf_pa_aux), FALSE, TRUE)) |>
        tidyr::replace_na(
            list(state = "", dispositivo_legal = "", acceptedNameUsage = "",
                 specie = "", nomenclaturalStatus = "", taxonomicStatus = "",
                 genus = "", scientificName = "", family = "", specificEpithet = "")
        ) |>
        rename(estado = state, nome_aceito = acceptedNameUsage) |>
        mutate(familia = if_else(is.na(familia), family, familia)) |>
        select(-c(ocor_uf_pa_aux, infraspecificEpithet, family))
    
    dataframe %<>%
        left_join(port,
                  by = c("specie_clean" = "nome_cientifico"),
                  relationship = "many-to-many") %>%
        distinct(id_tmp, .keep_all = TRUE) %>%
        # NA values replaced by "Nao Ameaçada"
        mutate(
            status_conservacao = if_else(is.na(status_conservacao),
                                         'Não Ameaçada',
                                         status_conservacao)
        ) %>%
        # Set status_conservacao column as a factor
        mutate(status_conservacao = factor(
            status_conservacao,
            levels = c(
                "Não Ameaçada",
                "Vulnerável",
                "Em Perigo",
                "Criticamente em Perigo"
            )
        )) %>%
        select(-c(id_tmp, specie_clean))

    # Filter typo and save it
    typo <- dataframe |>
        filter(erro_digitação == TRUE) |>
        select(num_arvore, ut, nome_cientifico, nome_aceito)

    output_dir_not <- './output/Pendencias/'
    if (!dir.exists(output_dir_not)) {
        dir.create(output_dir_not, recursive = TRUE)
    }

    if (nrow(typo > 0)) {
        write.csv2(
            typo,
            paste0(output_dir_not, "Erros_Digitacao.csv"),
            row.names = FALSE,
            fileEncoding = "latin1"
        )
    }

    # Filter taxonomic synonyms and save it
    synonyms <- dataframe |>
        filter(taxonomicStatus == "SINONIMO") |>
        select(nome_cientifico, nome_aceito) |>
        unique()

    if (nrow(synonyms > 0)) {
        write.csv2(
            synonyms,
            paste0(output_dir_not, "sinonimos_taxonomicos.csv"),
            row.names = FALSE,
            fileEncoding = "latin1"
        )
    }
    
    # Save the forest inventory
    output_dir <- "./output/"
    if (!dir.exists(output_dir)) dir.create(output_dir)
    
    write.csv2(
        dataframe,
        paste0(output_dir, "Inventario_Processado.csv"),
        fileEncoding = "latin1",
        row.names = FALSE
    )
    
    # Assign the new data frame to the user's global environment
    assign("dataframe", dataframe, envir = .GlobalEnv)
}
