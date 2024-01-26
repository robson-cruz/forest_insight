# ui.R
library(shiny)

shinyUI(
        fluidPage(
                navbarPage(
                        title = 'Forest Insight v1.0.0 - Apuleia',
                        fluidRow(
                                tabPanel(
                                        "Carregar Arquivo",
                                        fileInput("uploadFile", "Selecione a planilha"),
                                        actionButton("loadFile", "Carregar arquivo"),
                                        textOutput("fileStatus")
                                )
                        ),
                        fluidRow(
                                tabPanel(
                                        "Ver Dados!",
                                        DT::dataTableOutput('tbl')
                                )
                        )
                )
        )
)
