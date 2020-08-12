library(shiny)
library(shinythemes)
library(shinycssloaders)
library(shinydashboard)
library(here)
library(shinyalert)
library(janitor)
library(readr)
library(ggplot2)
library(purrr)

functions <- list.files(here::here("R"))

purrr::walk(functions, ~ source(here::here("R", .x)))


function(input,output){


  header  <-  dashboardHeader(title = "fishualizeR")

  # sidebar -----------------------------------------------------------------
  sidebar <- dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Description", tabName = "description", icon = icon("book-reader")),
      menuItem("Length Composition",tabName = "lcomps", icon = icon("chart-line")),
      menuItem("CPUE",tabName = "cpue", icon = icon("chart-line"))
    )) # close dashboardSidebar


  # body -----------------------------------------------------------------
  body <-  dashboardBody(
    useShinyalert(),
    tabItems(
    tabItem(tabName = "description",
    h1("fishualizeR"),
    "fishualizeR helps users visualize common fishery related data"),
    tabItem(tabName = "lcomps",
      fluidRow(box(title = "Load Data", "This will be a button for loading your data and specifying details about your data", width = 12)),
    h2("This is some text"),
    "Followed by more text",
    box(title = "Length Composition Data", dataTableOutput("lcomps"), width = 12),
    fluidRow(
    box(uiOutput("inspectplot_x")),
    box(uiOutput("inspectplot_y")),
    plotOutput("inspectplot")
    ))
  ) # close dashboard body
)
  dashboardPage(
    header,
    sidebar,
    body
  ) # close dashboard Padte

} # close server function
