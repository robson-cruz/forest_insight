# server.R
library(shiny)
source("global.R")


# Source Functions
source("./modules/dbh_classes_generate.R")
source("./modules/dbh_classes_chart.R")
source("./modules/drop_duplicated_rows.R")
source("./modules/scientific_name_clean.R")

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
                                # Generate DBH Classes and DBH plot
                                dbh_classes_generate(user_data)
                                dbh_classes_chart(df)
                                drop_duplicated_rows(df)
                                scientific_name_clean(df)
                                "Análise Finalizada!"
                                
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

        # output$DBH_classes_plot <- renderImage({
        #         outfile <- tempfile(fileext = '.png')
        #         png(outfile, width = 650, height = 450, res = 150)
        #         dbh_classes_chart(df)
        #         dev.off()
        #         list(src = outfile, alt = "Distribuiçao Diamétrica")
        # }, deleteFile = TRUE)
        
        output$DownloadDataAnalysis <- downloadHandler(
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
        output$DownloadData <- downloadHandler(
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
