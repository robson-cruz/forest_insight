# ui.R
library(shiny)


shinyUI(
        fluidPage(
                tags$head(
                        tags$style(type = "text/css", includeCSS("./www/css/styles.css"))
                        
                ),
                navbarPage(
                        title = 'Forest Insight v1.0.0 - Apuleia',
                        lang = "pt-BR",
                        tabPanel(
                                "Carregar Arquivo",
                                fileInput("uploadFile", "Selecione a planilha"),
                                actionButton("loadFile", "Carregar arquivo"),
                                textOutput("fileStatus")
                        ),
                        tabPanel("Ver Dados", DT::dataTableOutput('tbl')),
                        tabPanel('Gráficos', plotOutput('DBH_classes_plot')),
                        tabPanel("Baixar Análise", downloadLink('DownloadDataAnalysis', 'Download')),
                        tabPanel(
                                "Normas Florestais",
                                HTML("<a href = 'https://www.ibama.gov.br/sophia/cnia/livros/normasflorestaisfederaisparaaamazonia.pdf' target = '_blank'>Acessar Normas Florestais</a>")
                        ),
                        tabPanel('Planilha Modelo', downloadLink('DownloadData', 'Download'))
                )
        )
)
