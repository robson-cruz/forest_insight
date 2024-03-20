library(rmarkdown)


# Defines a function to render the analysis report to PDF
render_report <- function() {
    # Renders R Markdown file to PDF
    render(
        input = "./report.Rmd",
        runtime = "auto",
        output_file = "./output/Relatorio_Análise.pdf"
    )
}
