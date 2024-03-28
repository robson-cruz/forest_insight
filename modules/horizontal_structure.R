library(dplyr, warn.conflicts = FALSE)
library(magrittr)

dataframe <- readxl::read_excel("output/Inventario_Amostral.xlsx")

horizontal_structure <- function(dataframe) {
    
    n_ut <- length(unique(dataframe$Parcela))
    
    # Set absolute density
    df <- dataframe |>
        #select(num_arvore, ut, aem, flona, umf, upa, nome_cientifico, dap, g, vol_geo) |>
        #mutate(n.ha = 10000 / (n_ut * aem)) |>
        #mutate(G = g * n.ha) |>
        mutate(G = g * n.ha, v.ha = vol_geo * n.ha) |>
        group_by(nome_cientifico) |>
        summarize(DA = sum(n.ha))
    
    # Set relative density
    df %<>%
        group_by(nome_cientifico) %>%
        mutate(DR = DA / sum(da$DA) * 100)
            
    # Sampling unit where the species occurred
    ui <- dataframe |>
        select(nome_cientifico, Parcela) |>
        group_by(nome_cientifico, Parcela) |>
        summarise(n = n()) |>
        tidyr::spread(key = Parcela, value = n)
    
    ui$UI <- rowSums(!is.na(ui[, c(2:ncol(ui))]))
    ui <- ui[, c(1, length(ui))]
    ui
    
    df <- merge(df, ui)
    
    # Set absolute and relative frequency
    df %<>%
        mutate(FA = UI / n_ut * 100) %>% # ToDo:->>>>> CONFIRMAR
        select(-c(tidyr::matches('[0-9]')))
    
    sum_of_FA <- sum(df[, "FA"])
    
    df %<>%
        mutate(FR = FA / sum_of_FA * 100)
    
    # Set Absolute and Relative Dominance
    doa <- dataframe |>
        mutate(G = g * n.ha, v.ha = vol_geo * n.ha) |>
        group_by(nome_cientifico) |>
        summarize(DOA = sum(G))
    
    sum_of_doa <- sum(doa$DOA)
    
    df <- merge(df, doa)
    
    df %<>%
        mutate(DOR = DOA / sum_of_doa * 100) %>%
        mutate(VI = rowSums(select(., c(DR, FR, DOR))) / 3) %>%
        mutate(VC = rowSums(select(., c(DR, DOR))) / 2) %>%
        arrange(desc(VC))
    
    output_dir <- "output/"
    if (!dir.exists(output_dir)) dir.create(output_dir)
    
    write.csv2(
            df,
            file = file_name_horiz,
            row.names = FALSE,
            fileEncoding = 'latin1'
    )

    # Define a table
    top10_basal_area <- df %>%
        filter(row_number() <= 10)
    
    title_table <- paste(
        c(
            "Tabela 3. Relação das 10 espécies que apresentaram maior volor de cobertura na área da UPA.",
            "Em que: DA = Densidade Absoluta; DR = Densidade Relativa; UI = Unidade de Trabalho onde a espécie ocorre; FA = Frequência Absoluta; FR = Frequência Relativa; DOA = Dominância Absoluta; DOR = Dominância Relativa; VI = Valor de Importância; VC = Valor de Cobertura."
        ),
        sep = "\n"
    )
}
