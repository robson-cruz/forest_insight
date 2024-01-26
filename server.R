# server.R
library(shiny)
source("global.R")


function(input, output, session) {
        observe({
                if (!is.null(input$loadFile)) {
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
                        if (!is.null(my_data)) {
                                "Arquivo carregado com sucesso!"
                        } else {
                                "Por favor, carregue um arquivo antes de prosseguir."
                        }
                })
        })
        
        # Data frame
        output$tbl <- DT::renderDataTable({
                DT::datatable(
                        my_data, 
                        options = list(
                                pageLength = 6, 
                                rownames = FALSE,
                                language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese.json')
                        )
                )
        })
}

