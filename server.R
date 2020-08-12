

server <- function(input, output, session) {


# length composition ------------------------------------------------------


  lcomps <- read_csv(here("data", "BiometricosTable.csv")) %>%
    janitor::clean_names()

  output$lcomps <-
    renderDataTable(lcomps,
                    options = list(pageLength = 5))

  output$inspectplot_x <- renderUI({
    vars <- colnames(lcomps[map_lgl(lcomps, is.numeric)])
    selectizeInput(
      "inspectplot_x",
      "Select X Variable from raw data to plot",
      vars
    )
  })

  output$inspectplot_y <- renderUI({
    vars <- c(NA,colnames(lcomps[map_lgl(lcomps, is.numeric)]))
    selectizeInput(
      "inspectplot_y",
      "Select Y Variable from raw data to plot",
      vars
    )
  })



  output$inspectplot <-
    renderPlot(inspect_plot(lcomps, x = input$inspectplot_x, y = input$inspectplot_y))


}
