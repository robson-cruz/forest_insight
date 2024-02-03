library(shiny)


shinyUI(
    navbarPage(
        "Forest Insight v1.0.0 - Apuleia",
        lang = "pt-BR",
        #tags$head(includeScript("google-analytics.js"))
        tags$head(
            tags$style(type = "text/css", includeCSS("./www/css/styles.css"))
               
        ),
       tabPanel(
           "Planilha Áreas de Efetivo Manejo",
           fileInput(inputId = "uploadAem", label = "Planilha Áreas de Efetivo Manejo"),
           actionButton(inputId = "uploadFileAem", label = "Fazer upload"),
           textOutput("fileStatusAem")
       ),
       tabPanel(
           "Planilha do Inventário",
           fileInput(inputId = "uploadInventario", label = "Planilha Inventário"),
           actionButton(inputId = "uploadFileInventario", "Fazer upload e Rodar Análise"),
           textOutput("fileStatusInventario")
       ),
       navbarMenu(
           "Planilha Modelo",
           tabPanel(
               "Planilha Modelo", 
               downloadButton(outputId = "DownloadDataModel", label = "Baixar Análise")
           )
       ),
       navbarMenu(
           "Normas Florestais",
           HTML("<a href = 'https://www.ibama.gov.br/sophia/cnia/livros/normasflorestaisfederaisparaaamazonia.pdf' target = '_blank'>Acessar Normas Florestais</a>")
       ),
       navbarMenu(
           "Ver Análise",
           tabPanel(
               "Gráficos",
               sidebarPanel(
                   radioButtons(inputId = "plotType", label = "Gráfico",
                                choices = c("Distribuição Diamétrica" = "dap",
                                            "Distribuição Diamétrica por UT" = "dapUt",
                                            "Área Basal por Classe de DAP" = "areaBasalDap",
                                            "Área Basal por UT" = "areaBasalUt",
                                            "Seleção por Classe de DAP" = "dapCorte",
                                            "Qualidade de Fuste" = "qf",
                                            "Seleção por Categoria e Status Ecológico" = "categorias",
                                            "Critério 10% a 15%" = "crit1015")
                   )
               ),
               conditionalPanel(
                   condition = "input.plotType == 'dapUt'",
                   plotOutput("BoxPlot_DBH_by_Plt")
               ),
               conditionalPanel(
                   condition = "input.plotType == 'dap'",
                   plotOutput("DBH_classes_plot")
               ),
               conditionalPanel(
                   condition = "input.plotType == 'areaBasalDap'",
                   plotOutput("")
               ),
               conditionalPanel(
                   condition = "input.plotType == 'areaBasalUt'",
                   plotOutput("")
               ),
               conditionalPanel(
                   condition = "input.plotType == 'dapCorte'",
                   plotOutput("")
               ),
               conditionalPanel(
                   condition = "input.plotType == 'qf'",
                   plotOutput("qf_plot")
               ),
               conditionalPanel(
                   condition = "input.plotType == 'categorias'",
                   plotOutput("status_cutting_plot")
               ),
               conditionalPanel(
                   condition = "input.plotType == 'crit1015'",
                   plotOutput("")
               )
           ),
           tabPanel(
               "Inventário Processado",
               DT::dataTableOutput(outputId = "verDados")
           ),
           tabPanel("Resumo Inventário",
                    verbatimTextOutput(outputId = "summary")
           ),
           tabPanel(
               "Baixar Análise Completa",
               downloadLink('DownloadDataAnalysis', 'Download')
           )
       )
    )
)