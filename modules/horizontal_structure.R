library(dplyr, warn.conflicts = FALSE)
library(magrittr)


horizontal_structure <- function(dataframe) {
        ## Get some information from the data
        umf <- paste0(unique(dataframe$umf))
        upa <- paste0(unique(dataframe$upa))
        flona <- paste0(unique(dataframe$flona))

        fi_horiz <- dataframe %>%
                group_by(nome_cientifico) %>%
                count(nome_cientifico, ut, name = 'N') %>%
                spread(key = ut, value = N) %>%

                ## Count the occurrence of each specie per UT (plot)
                row_count(count = NA) %>%
                mutate(n_UT = sum(abs(rowcount - length(unique(dataframe$ut))))) %>%

                ## Set absolute density
                mutate(DA = n_UT / sum(aem$aem)) %>%

                ## Set absolute frequency
                mutate(FA = n_UT / length(unique(dataframe$ut))) %>%
                select(-c(tidyr::matches('[0-9]')))


        n_sp <- dataframe %>% group_by(nome_cientifico) %>% summarise(n = n())

        fi_horiz %<>%
                left_join(n_sp, by = 'nome_cientifico')

        ## Set relative density
        fi_horiz %<>%
                select(n, n_UT, DA, FA) %>%
                mutate(DR = DA / sum(fi_horiz$DA) * 100) %>%

                ## Set relative frequency
                mutate(FR = FA / sum(fi_horiz$FA) * 100)


        ## Set basal area per specie
        g_upa <- dataframe %>%
                group_by(nome_cientifico) %>%
                summarise(Gsp = sum(g))

        fi_horiz %<>%
                left_join(g_upa, by = 'nome_cientifico')

        ## Set total basal area
        FI_G <- dataframe %>%
                select(nome_cientifico, g) %>%
                summarize(SumG = sum(g))

        ## Set Dominance (DO)
        fi_horiz %<>%
                mutate(DO = Gsp / sum(aem$aem)) %>%
                mutate(DOR = (DO / (FI_G$SumG / sum(aem$aem)) * 100)) %>%

                ## Set the Coverage Value (VC)
                mutate(VC = DR + DOR) %>%
                mutate(VCR = VC / 2) %>%

                ## Set the Importance Value (VI)
                mutate(VI = FR + DR + DOR)


        fi_horiz %<>%
                mutate(VIR = (VI / sum(fi_horiz$VI)) * 100)

        # Get top 10 DOR
        top_10_dor <- fi_horiz %>%
                arrange(desc(DOR)) %>%
                tibble::rowid_to_column(var = 'row_id') %>%
                filter(row_id < 11)

        sp_top_10 <- top_10_dor$nome_cientifico
        dor_top_10 <- as.character(round(top_10_dor$DOR, 2))

        for (i in seq_along(sp_top_10)) {
                sp_top_10[i] <- paste0(sp_top_10[i], ' (', dor_top_10[i], '%)')
        }

        sp_top_txt <- glue::glue_collapse(sp_top_10, sep = ', ', last = ' e ')

        write.table(
                sp_top_txt,
                file = paste0('./output/Top_10_sp_DOR', '_umf_', umf, '_upa_',
                             upa, '_', flona, '.txt')[1],
                quote = FALSE,
                sep = ', ',
                col.names = '10 espécies que apresentaram as maiores dominâncias relativas\n',
                row.names = FALSE
        )

        file_name_horiz <- paste0(
                './output/dataFrame/Horiz_Structure_', 'umf_', umf, '_upa_',
                upa, '_', flona, '.csv'
        )[1]

        write.csv2(
                fi_horiz,
                file = file_name_horiz,
                row.names = FALSE,
                fileEncoding = 'latin1'
        )


        # Assign the local copy of df to the Global R Environment
        assign('fi_horiz', fi_horiz, envir = .GlobalEnv)

}
