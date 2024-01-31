# ui.R
library(shiny)

shinyUI(
        fluidPage(
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
                        tabPanel("Baixar Análise", downloadLink('DownloadDataAnalysis', 'Download')),
                        tabPanel("Normas Florestais", a(href = 'https://github.com/rcflorestal/endangeredBrazilianPlantSpecies/blob/main/README.md')),
                        tabPanel('Planilha Modelo', downloadLink('DownloadData', 'Download')),
                        
                        
                )
        )
)
