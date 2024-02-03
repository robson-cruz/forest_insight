# server.R
library(shiny)
library(ggplot2)


# Source script
source("global.R")
# Source Functions
source("./modules/dbh_classes_generate.R")
source("./modules/dbh_classes_chart.R")
source("./modules/drop_duplicated_rows.R")
source("./modules/scientific_name_clean.R")
source("./modules/qf_chart.R")


input_data_model <- read.csv2("./data/input_data.csv")

function(input, output, session) {
        observe({
                if (!is.null(input$uploadAem)) {
                        tryCatch({
                                uploadAem <- read.csv2(input$uploadAem$datapath)
                                setUploadAem(uploadAem)
                                "Click em Fazer upload."
                        }, error = function(e) {
                                print(paste("Erro ao ler o arquivo de Áreas de Efetivo Manejo:", e))
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
                                inventario %<>% left_join(aem, by = 'ut')
                                dbh_classes_generate(inventario)
                                drop_duplicated_rows(df)
                                scientific_name_clean(df)
                                "Dados Processados com Sucesso.Veja Análise"
                                
                        } else {
                                "Por favor, carregue um arquivo válido antes de prosseguir."
                        }
                })
        })
        
        # Data frame
        output$verDados <- DT::renderDataTable({
                DT::datatable(
                        df,
                        options = list(
                                pageLength = 6,
                                rownames = FALSE,
                                language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese.json')
                        )
                )
        })
        output$summary <- renderPrint({
                summary(df[, c("dap", "altura", "qf", "g", "vol_geo", "categoria2")])
        })
        
        output$DBH_classes_plot <- renderPlot({
                dbh_classes_chart(df)
        }, width = 700, res = 150)
        
        output$qf_plot <- renderPlot({
                qf_chart(df)
        }, width = 700, res = 150)
        
        output$downloadAnalise <- downloadHandler(
                filename = function() {
                        paste('Dados_Processados_', Sys.Date(), '.zip', sep = '')
                },
                content = function(file) {
                        df_file <- file.path("./output", 'Inventario_Processado.csv')
                        write.csv2(df, df_file, row.names = FALSE, fileEncoding = 'latin1')
                        zip(file, c("./output", df_file))
                }
        )
        
        # Download - Spreadsheet Model
        output$DownloadDataModel <- downloadHandler(
                filename = function() {
                        paste('Planilha_Modelo_ForestInsight', '.csv', sep = '')
                },
                content = function(file) {
                        write.csv2(input_data_model, file, row.names = FALSE, fileEncoding = 'latin1')
                }
        )
        
        session$onSessionEnded(function() {
                stopApp(returnValue = invisible())
        })
}
