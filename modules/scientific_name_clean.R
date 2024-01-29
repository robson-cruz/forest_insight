#' scientific_name_clean function
#' 
#' @description 
#' This function takes a data frame from a global forest inventory and points 
#' out typos and taxonomic synonym by using the globalnames API from
#' http://verifier.globalnames.org/data_sources
#' 
#' @param dataset
#' 
 

library(httr)
library(jsonlite)
library(magrittr)
library(stringr)
library(dplyr, warn.conflicts = FALSE)


scientific_name_clean <- function(df) {
        
        # Get unique scientific names form the input data set
        input_names <- unique(df$nome_cientifico)
        # Get data set column names
        col_names <- names(df)
        
        # Read The List of Endangered Species, according to the Ordinance of the
        # Brazilian Ministry of Environment (Portaria MMA 443/2014) and Resolution 54/2007
        # from the Pará state Brazil
        port <- readr::read_csv2(
                'https://raw.githubusercontent.com/rcDeveloping/endangeredBrazilianPlantSpecies/main/output/Especies_Ameacadas_BRA.csv', 
                locale = readr::locale(encoding = "latin1")
        ) %>%
                filter(lista %in% c('Lista Flora Ameaçada Pará', 'Lista Portaria 443')) %>%
                select(-c(1, 4, 5, 6, 8)) %>%
                group_by(nome_cientifico, status_conservacao) %>%
                summarise(dispositivo_legal = paste(dispositivo_legal, collapse = ", "))
        
        # Read "reflora" data set to get specie geographic distribution
        reflora <- readr::read_csv2(
                'https://raw.githubusercontent.com/rcDeveloping/endangeredBrazilianPlantSpecies/main/data/reflora20230606.csv'
                ) %>%
                select(specie, locationID) %>%
                rename(specieReflora = specie)
                
        # Basic data cleaning
        df %<>%
                # remove extra white space between genus and epithet
                mutate(nome_cientifico = str_squish(nome_cientifico),
                       
                       # remove white space from the beginning and end of scientific names
                       nome_cientifico = str_trim(nome_cientifico),
                       
                       # Capitalization inconsistency
                       nome_cientifico = tolower(nome_cientifico),
                       nome_cientifico = R.utils::capitalize(nome_cientifico),
                       
                       # get only the scientific name, without taxonomist's names
                       nome_cientifico = str_extract(
                               nome_cientifico,
                               '(\\w+\\s\\w+)(-\\w+)?(\\s\\w+\\ssubsp.\\s\\w+)?(\\ssubsp.\\s\\w+)?(\\svar.\\s\\w+)?(\\.\\s\\w+)?'
                       )
                )
        
        # Using POST method in globalnames API
        post_global_names <- httr::POST(
                "https://verifier.globalnames.org/api/v1/verifications",
                config = httr::add_headers(accept = "application/json",
                                           `Content-Type` = "application/json"),
                body = jsonlite::toJSON(list(nameStrings = input_names)
                ))
        
        data <- jsonlite::fromJSON(rawToChar(post_global_names$content))$names %>%
                rename(submitedName = name) %>%
                mutate(
                        typo = if_else(matchType == "Fuzzy", TRUE, FALSE)
                ) %>%
                select(-c(matchType, curation)) %>%
                tidyr::unnest_wider(bestResult) %>%
                mutate(
                        familia = gsub(".*(\\b\\w+ceae\\b).*", "\\1", classificationPath)
                ) %>%
                select(submitedName, typo, isSynonym,
                       currentCanonicalSimple, familia)
        
        # Join data
        df %<>%
                tibble::rowid_to_column(var = 'id') %>%
                left_join(data,
                          by = c("nome_cientifico" = "submitedName"),
                          relationship = "many-to-many") %>%
                distinct(id, .keep_all = TRUE) %>%
                select(id, all_of(col_names), typo, isSynonym,
                       currentCanonicalSimple, familia) %>%
                rename(nome_aceito = currentCanonicalSimple)
        
        df %<>%
                left_join(port,
                          by = c("nome_aceito" = "nome_cientifico"),
                          relationship = "many-to-many") %>%
                distinct(id, .keep_all = TRUE) %>%
                # NA values replaced by "Nao Ameaçada"
                mutate(
                        status_conservacao = if_else(is.na(status_conservacao),
                                         'Não Ameaçada',
                                         status_conservacao)
                ) %>%
                # Set status column as a factor
                mutate(
                        status = factor(status_conservacao,
                                        levels = c('Não Ameaçada', 'Vulnerável',
                                                   'Em Perigo'))
                ) %>%
                select(-c(id))
        
        # Filter species that not have occurrence in "Pará" state
        occurrence_issue <- df %>%
                mutate(
                        aux_name = if_else(typo == TRUE, nome_aceito, nome_cientifico)
                ) %>%
                left_join(reflora,
                          by = c("aux_name" = "specieReflora"),
                          relationship = "many-to-many") %>%
                distinct(num_arvore, .keep_all = TRUE) %>%
                filter(locationID != "PA") %>%
                select(num_arvore, ut, nome_cientifico, nome_aceito, locationID)
        
        if (nrow(occurrence_issue > 0)) {
                write.csv2(occurrence_issue,
                           "./output/warnings/especies_sem_ocorrencia_conhecida_PA.csv",
                           row.names = FALSE,
                           fileEncoding = "latin1")
        }
        
        # Filter typo and save it
        typo <- df %>%
                filter(typo == TRUE) %>%
                select(num_arvore, ut, nome_cientifico, nome_aceito)
        
        if (nrow(typo > 0)) {
                write.csv2(typo,
                           "./output/warnings/erro_digitacao.csv",
                           row.names = FALSE,
                           fileEncoding = "latin1")
        }
        
        # Filter taxonomic synonyms and save it
        synonyms <- df %>%
                filter(isSynonym == TRUE) %>%
                select(nome_cientifico, nome_aceito) %>%
                unique()
        
        if (nrow(synonyms > 0)) {
                write.csv2(synonyms,
                           "./output/warnings/sinonimos.csv",
                           row.names = FALSE,
                           fileEncoding = "latin1")
        }
        
        # Assign the new data frame to the user's global environment
        assign("df", df, inherits = TRUE, envir = .GlobalEnv)
}
