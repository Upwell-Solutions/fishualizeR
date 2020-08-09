library(shiny)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(dplyr)
library(purrr)
library(broom)
library(rstanarm)
library(here)
library(shinythemes)
library(shinycssloaders)
library(shinydashboard)
library(FishLife)
library(taxize)
library(here)
library(shinyalert)

server <- function(input, output, session) {

  dataset <- reactive({
    get(input$dataset, "package:datasets")
  })

  output$summary <- renderPrint({
    summary(dataset())
  })

  output$table <- renderTable({
    dataset()
  })
}
