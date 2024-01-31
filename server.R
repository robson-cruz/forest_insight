# server.R
library(shiny)
source("global.R")


# Source Functions
source("./modules/dbh_classes_generate.R")


input_data_model <- read.csv2("./data/input_data.csv")

function(input, output, session) {
        observe({
                if (!is.null(input$uploadFile)) {
                        tryCatch({
                                uploadedFile <- read.csv2(input$uploadFile$datapath)
                                setUploadedData(uploadedFile)
                        }, error = function(e) {
                                print(paste("Erro ao ler o arquivo:", e))
                        })
                }
        })
        
        observeEvent(input$loadFile, {
                output$fileStatus <- renderText({
                        if (!is.null(user_data)) {
                                "Arquivo carregado com sucesso!"
                                # Generate DBH Classes
                                dbh_classes_generate(user_data)
                        } else {
                                "Por favor, carregue um arquivo antes de prosseguir."
                        }
                })
        })
        
        # Data frame
        output$tbl <- DT::renderDataTable({
                DT::datatable(
                        df,
                        options = list(
                                pageLength = 6, 
                                rownames = FALSE,
                                language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese.json')
                        )
                )
        })
        
        # Download - Data Analysis
        output$DownloadDataAnalysis <- downloadHandler(
                filename = function() {
                        paste('Dados_Processados', Sys.Date(), '.csv', sep = '_')
                },
                content = function(file) {
                        write.csv2(df, file, row.names = FALSE, fileEncoding = 'latin1')
                }
        )
        
        # Download - Spreadsheet Model
        output$DownloadData <- downloadHandler(
                filename = function() {
                        paste('Planilha_Modelo_ForestInsight', '.csv', sep = '')
                },
                content = function(file) {
                        write.csv2(input_data_model, file, row.names = FALSE, fileEncoding = 'latin1')
                }
        )
}

