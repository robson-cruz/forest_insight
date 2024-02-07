library(shiny)
library(ggplot2)


# Source script
source("global.R")
# Source Functions
source("./modules/dbh_classes_generate.R")
source("./modules/drop_duplicated_rows.R")
source("./modules/scientific_name_clean.R")
source("./modules/dbh_classes_chart.R")
source("./modules/qf_chart.R")
source("./modules/eco_status_chart.R")
source("./modules/dbh_by_ut_chart.R")
source("./modules/criterion_10_15_percent.R")
source("./modules/stat_by_ut.R")
source("./modules/criterion_3_4_trees.R")
source("./modules/autex.R")


inventario_modelo <- read.csv2("./data/input_data.csv")
aem_modelo <- read.csv2("./data/aem.csv")

# Define function for data processing with progress indicator
process_data <- function() {
    withProgress(
        message = 'Processando dados...',
        detail = 'Isso pode demorar um pouco...',
        value = 0, {
            # Update the value parameter to indicate progress (0 to 10)
            incProgress(0.2, detail = 'Etapa 1 de 10')
            # Perform data processing steps here
            inventario %<>% left_join(aem, by = 'ut')
            dbh_classes_generate(inventario)
            drop_duplicated_rows(df)
            
            incProgress(0.1, detail = 'Etapa 2 de 10')
            scientific_name_clean(df)
            
            incProgress(0.2, detail = 'Etapa 3 de 10')
            crit_34(df)
            stat_ut(df)
            
            incProgress(0.2, detail = 'Etapa 4 de 10')
            dbh_classes_chart(df)
            dbh_over_ut(df)
            qf_chart(df)
            eco_status_chart(df)
            
            incProgress(0.2, detail = 'Etapa 5 de 10')
            criterion_1015(df)
            autex_generate(df)
            
            
            
            # Final step
            incProgress(1, detail = 'Análise Finalizada!')
        }
    )
    # Return a message indicating the process is done
    return("Dados Processados com Sucesso. Veja Análise")
}

# Define server function
function(input, output, session) {
    
    observe({
        if (!is.null(input$uploadAem)) {
            tryCatch({
                uploadAem <- read.csv2(input$uploadAem$datapath)
                setUploadAem(uploadAem)
                "Click em Fazer upload."
            }, error = function(e) {
                print(paste(
                    "Erro ao ler o arquivo de Áreas de Efetivo Manejo:",
                    e
                ))
            })
        }
    })
    
    observe({
        if (!is.null(input$uploadInventario)) {
            tryCatch({
                uploadInventario <- read.csv2(input$uploadInventario$datapath)
                setUploadedData(uploadInventario)
                "Clik em Fazer upload e Rodar Análise."
            }, error = function(e) {
                print(paste("Erro ao ler o arquivo do Inventário:", e))
            })
        }
    })
    
    observeEvent(input$uploadFileAem, {
        output$fileStatusAem <- renderText({
            if (!is.null(aem)) {
                "Dados Enviados com Sucesso."
            } else {
                "Por favor, carregue um arquivo válido antes de prosseguir."
            }
        })
    })
    
    observeEvent(input$uploadFileInventario, {
        output$fileStatusInventario <- renderText({
            if (!is.null(inventario)) {
                # Call the function for processing data with progress indicator
                process_message <- process_data()
                return(process_message)
            } else {
                "Por favor, carregue um arquivo válido antes de prosseguir."
            }
        })
    })
    
    # Render output of process_data function in the progressOutput uiOutput
    output$progressOutput <- renderUI({
        if (!is.null(input$uploadFileInventario) && input$uploadFileInventario > 0) {
            return(textOutput("fileStatusInventario"))
        }
    })
    
    # Data frame
    output$verDados <- DT::renderDataTable({
        DT::datatable(df,
                      rownames = FALSE,
                      options = list(
                          pageLength = 6,
                          language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese.json')
                      ))
    })
    
    output$summary <- renderPrint({
        summary(df[, c("dap", "altura", "qf", "g", "vol_geo", "categoria2")])
    })
    
    output$DBH_classes_plot <- renderImage({
        dbh_classes_chart(df)
        list(src = './output/Graficos/Distribuicao_Diametrica/Distribuicao_Dap.png',
             contentType = 'image/png')
        
    }, deleteFile = FALSE)
    
    output$BoxPlot_DBH_by_Plt <- renderImage({
        dbh_over_ut(df)
        list(src = './output/Graficos/Distribuicao_Diametrica/Distribuicao_Dap_UT.png',
             contentType = 'image/png')
    })
    
    output$qf_plot <- renderImage({
        qf_chart(df)
        list(src = './output/Graficos/Qualidade_de_Fuste/Qualidade_de_Fuste.png',
             contentType = 'image/png')
    })
    
    output$status_cutting_plot <- renderImage({
        eco_status_chart(df)
        list(src = './output/Graficos/Selecao_Corte/Selecao_Status_Ecologico.png',
             contentType = 'image/png')
    })
    
    output$crit_10.15_plt <- renderImage({
        criterion_1015(df)
        list(src = './output/Graficos/Criterio_10_a_15_Porcento/Criterio_10_a_15_Porcento.png',
             contentType = 'image/png')
    })
    
    output$DownloadDataAnalysis <- downloadHandler(
        filename = function() {
            paste('Dados_Processados_', Sys.Date(), '.zip', sep = '')
        },
        content = function(file) {
            df_file <- file.path("./output", 'Inventario_Processado.csv')
            
            write.csv2(df, df_file,
                       row.names = FALSE,
                       fileEncoding = 'latin1')
            
            files_to_zip = list.files("./output/",
                                      recursive = TRUE,
                                      full.names = TRUE,
                                      include.dirs = TRUE)
            
            zip(file, files_to_zip)
        }
    )
    
    # Download - Spreadsheet Models
    output$Download_inventario_Modelo <- downloadHandler(
        filename = function() {
            paste('Planilha_Modelo_Inventario', '.csv', sep = '')
        },
        content = function(file) {
            write.csv2(inventario_modelo,
                       file,
                       row.names = FALSE,
                       fileEncoding = 'latin1')
            
        }
    )
    
    output$Download_Aem_modelo <- downloadHandler(
        filename = function() {
            paste('Planilha_Modelo_Areas_Efetivo_Manejo', '.csv', sep = '')
        },
        content = function(file) {
            write.csv2(aem_modelo,
                       file,
                       row.names = FALSE,
                       fileEncoding = 'latin1')
            
        }
    )
    
    session$onSessionEnded(function() {
        stopApp(returnValue = invisible())
    })
}
