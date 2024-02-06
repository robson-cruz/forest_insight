# server.R
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


inventario_modelo <- read.csv2("./data/input_data.csv")
aem_modelo <- read.csv2("./data/aem.csv")

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
                inventario %<>% left_join(aem, by = 'ut')
                dbh_classes_generate(inventario)
                drop_duplicated_rows(df)
                scientific_name_clean(df)
                crit_34(df)
                stat_ut(df)
                dbh_classes_chart(df)
                dbh_over_ut(df)
                qf_chart(df)
                eco_status_chart(df)
                criterion_1015(df)
                "Dados Processados com Sucesso. Veja Análise"
                
            } else {
                "Por favor, carregue um arquivo válido antes de prosseguir."
            }
        })
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
