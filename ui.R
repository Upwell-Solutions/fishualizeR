options(shiny.maxRequestSize = 50 * 1024^2)
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
library(dplyr)
library(tidyr)
library(vroom)
library(hrbrthemes)
library(DT)
library(lares, include.only = "plot_timeline")
library(readxl)
library(pivottabler)

shiny_theme <- hrbrthemes::theme_ipsum(base_size = 14,
                                       axis_title_size = 16)

ggplot2::theme_set(shiny_theme)
functions <- list.files(here::here("R"))

purrr::walk(functions, ~ source(here::here("R", .x)))


function(input,output){


  header  <-  dashboardHeader(title = "fishualizeR")

  # sidebar -----------------------------------------------------------------
  #icons: https://fontawesome.com/v5.15/icons?d=gallery&p=2&m=free or icon("cog", lib = "glyphicon") https://getbootstrap.com/docs/3.4/components/#glyphicons 
  sidebar <- dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Introduction", tabName = "description", icon = icon("book-reader")),
      menuItem("1. Data Inventory", tabName = "inventory", icon = icon("calendar")),
      #menuItem("2. Time Series Data Coverage", tabName = "coverage", icon = icon("chart-line")),
      menuItem("2. Life History Parameters", tabName = "parameters", icon = icon("fish")),
      menuItem("Upload Data File", tabName = "dataUpload", icon = icon("upload")),
      menuItem("Univariate Data Exploration",
               menuSubItem("Univariate: Categorical Data", tabName = "UniCategorical", icon = icon("chart-line")),
               menuSubItem("Univariate: Numerical Data", tabName = "UniNumerical", icon = icon("chart-line")),
               menuSubItem("Univariate: Date Data", tabName = "UniDate", icon = icon("calendar"))
        ),
      # menuItem("Multivariate Data Exploration",
      #          menuSubItem("1 Numerical + Categorical Data", tabName = "Multi1Numeric", icon = icon("chart-line")),
      #          menuSubItem("2 Numerical Data", tabName = "Multi2Numeric", icon = icon("chart-line"))
      # ),
      menuItem("Multivariate Data Exploration", tabName = "Multivariate", icon = icon("chart-line")),
      menuItem("Data Coverage", tabName = "dataCoverage", icon = icon("percent")),
      menuItem("General Data Exploration",tabName = "lcomps", icon = icon("chart-line"))
      #menuItem("CPUE",tabName = "cpue", icon = icon("chart-line"))
    )) # close dashboardSidebar


  # body -----------------------------------------------------------------

  body <-  dashboardBody(
    useShinyalert(),
    tabItems(
    tabItem(tabName = "description",
    h1("fishualizeR"),
    "fishualizeR helps users visualize common fishery related data. There are two ways to explore your data: 1) Add your data in the provided templates and follow along with each step (as applicable); 2) Upload your data as a CSV and use the various tools in the General Data Exploration tab."),
    
    tabItem(tabName = "inventory",
            fluidRow(
              box(title = "Load Data Inventory file", fileInput("dataInvFile", NULL), width = 12)),
            fluidRow(column(4, checkboxInput("inventoryFilterAvailable", "Show only available data sources?"),
                               checkboxInput("inventoryFilterEvents", "Hide Events Timeline?")),
                    column(4, uiOutput("invGearFilter"), uiOutput("invAreaFilter")),
                    column(4, uiOutput("invUnitsFilter"), uiOutput("invSectorFilter"))),
                     # column(4, selectInput("inventoryRowLabel", "What to use as the row label?", 
                     #                       c("Source Name", "Institution", "Fleet"), selected = "Source Name")),
                     # column(4,selectInput("inventoryInlineLabel", "What to use as the inline label",
                     #                      c("None", "Source Name", "Institution", "Fleet"), selected = "Fleet"))),
            fluidRow(plotOutput("dataInvTimeline"))
    ), #close inventory tab
    
    tabItem(tabName = "parameters",
            fluidRow(
              box(title = "Load life history parameter file", fileInput("LHParamFile", NULL), width = 12)),
            fluidRow(column(6, selectInput("LHParamFacet", "Group by category?", c("None", "Area", "Sex", "Methodology"), selected = "None"))),
            fluidRow(column(6,plotOutput("vonBertGrowthPlot")), column(6,tableOutput("vonBertTable"))),
            fluidRow(column(6,plotOutput("maturityPlot")), column(6,tableOutput("maturityTable"))),
            fluidRow(column(6,plotOutput("lengthWeightPlot")), column(6,tableOutput("lengthWeightTable"))),
            fluidRow(column(6,plotOutput("natMortPlot")), column(6,tableOutput("natMortTable")))
            
            
    ), #close parameter tab
    tabItem(tabName = "dataUpload",
            fluidRow(h4("Upload the data file (CSV only) that you would like to explore:")),
            fluidRow(box(title = "Load data file", fileInput("dataFile", NULL), width = 12)),
            conditionalPanel("output.dataFileUploaded == true", 
                             fluidRow(column(6, tableOutput("uploadSummary")),column(6, tableOutput("uploadColTypes"))),
                             box(
                               title="Data Preview", width = NULL,status = "primary",
                               div(style = 'overflow-x: scroll',tableOutput("uploadDataHead"))
                              )
            )
            #tableOutput("uploadSummary")
    ), #close dataUpload tab
    #TODO: add how many missing values or NAs
    tabItem(tabName = "UniCategorical",
              conditionalPanel("output.dataFileUploaded == false", h4("First, use the Upload Data File Tab to upload data to explore.")),
              conditionalPanel("output.dataFileUploaded == true",
                uiOutput("selectUniqueCol"),
                fluidRow(textOutput("missingValueCountText")), br(),
                fluidRow(dataTableOutput("uniqueValues")),
                fluidRow(plotOutput("uniqueValuesBarPlot"))
              )
    ), #close UniCategorical tab
    tabItem(tabName = "UniNumerical",
            conditionalPanel("output.dataFileUploaded == false", h4("First, use the Upload Data File Tab to upload data to explore.")),
            conditionalPanel("output.dataFileUploaded == true",
              fluidRow(uiOutput("selectUnivariateNumberCol"), selectInput("uniPlotType", "Choose plot type:", c("Histogram", "Density", "Box", "Violin"))),
              fluidRow(column(3,tableOutput("summaryStatTable")), column(9, plotOutput("univariatePlot")))
            )
    ), #close UniNumerical tab
    tabItem(tabName = "UniDate",
            conditionalPanel("output.dataFileUploaded == false", h4("First, use the Upload Data File Tab to upload data to explore.")),
            conditionalPanel("output.dataFileUploaded == true",
                             h4("Under construction")
                             #TODO: choose single date or range
                             # select columns for day, month, year
                             # add date table/graphs
            )
    ), #close UniDate tab
    # tabItem("Multi1Numeric",
    #         conditionalPanel("output.dataFileUploaded == false", h4("First, use the Upload Data File Tab to upload data to explore.")),
    #         conditionalPanel("output.dataFileUploaded == true",
    #                          
    #         )
    # ), #close Multi1Numeric tab
    # tabItem("Multi2Numeric",
    #         conditionalPanel("output.dataFileUploaded == false", h4("First, use the Upload Data File Tab to upload data to explore.")),
    #         conditionalPanel("output.dataFileUploaded == true",
    #                          
    #         )
    # ), #close Multi2Numeric tab
    tabItem("Multivariate",
            conditionalPanel("output.dataFileUploaded == false", h4("First, use the Upload Data File Tab to upload data to explore.")),
            conditionalPanel("output.dataFileUploaded == true",
                             fluidRow( column(4, uiOutput("multi_plot_x"),
                                                uiOutput("multi_plot_y")),
                                       column(4, uiOutput("multi_plot_fill"),
                                              uiOutput("multi_plot_facet"),
                                              checkboxInput("multi_factorfill","Convert Color Variable to Categorical?", value = FALSE),
                                              selectInput("multi_scales", "Use same scales for faceted plots?", c("Use same for rows and columns" = "fixed", "Vary for rows only" = "free_x", "Vary for columns only" = "free_y", "Vary for rows and columns" = "free"))),
                               
                               column(4, selectInput("pivotTableStatInput", "Select stat for pivot table:", c("Count Only", "Mean" = "mean", "Min/Med/Max" = "quantiles", "Standard Deviation" = "sd")))
                              ),
                             actionButton("multi_plot_button","Plot Data"),
                             plotOutput("multi_plot"), 
                             br(),
                             br(),
                             pivottablerOutput("multi_table")
            )
    ), #close Multivariate tab
    tabItem(tabName = "dataCoverage", "Under Construction"),
    tabItem(tabName = "lcomps",
      checkboxInput("example","Check this box to use example data instead of uploading data"),
      fluidRow(h4("Go through and fill out the various options in different sections. Leave as NA anything that you don't want to plot.")),
      fluidRow(
        box(title = "Load Data as .csv file (max size 50 MB)",fileInput("file", NULL), width = 12)),
      # numericInput("n", "Rows", value = 5, min = 1, step = 1),
      # tableOutput("head"),
      fluidRow(box(title = "Length Composition Data",
                   uiOutput("colnames"),
                   dataTableOutput("lcomps"), width = 12, solidHeader = TRUE, collapsible = TRUE)),
      fluidRow(
        box(
          uiOutput("select_ldata"),
          "If rows represent tallies per length bin, select column with number of observations per length bin. Otherwise leave as 'NA'",
          uiOutput("select_tally")
        )
      ),
      fluidRow(
      tabBox(
      title = "Examine Raw Data",
      id = "inspect",
      tabPanel(
        "Plot Raw Data",
        br(),
        "Use this area to explore your raw data",
        uiOutput("raw_plot_x"),
        uiOutput("raw_plot_y"),
        uiOutput("raw_plot_fill"),
        uiOutput("raw_plot_facet"),
        actionButton("plot_raw","Plot Data"),
        checkboxInput("raw_factorfill","Convert Color Variable to Categorical?", value = FALSE),
        plotOutput("raw_plot"),
        br(),
        br()
      ),
      br(),
      br(),
      br(),
      br(),
      tabPanel(title = "Modify Raw Data",
        "Use this tab to correct units in the length data. For example, divide all fish with length measurements over 100 by 10.",
        uiOutput("max_length"),
        uiOutput("max_length_factor"),
        uiOutput("min_length"),
        uiOutput("min_length_factor"),
        actionButton("correct_units","Correct Length Units"),
        actionButton("reset","Reset to Original Data")
  
      ),
      width = 12)
      ), # close inspect fluidrow
      fluidRow(
        box(title = "Assess Data Coverage", width = 12, solidHeader = TRUE, collapsible = TRUE,
            "Use this area to assess your data coverage",
            uiOutput("cov_var_1"),
            uiOutput("cov_var_2"),
            uiOutput("cov_var_3"))),
      fluidRow(downloadButton("download_coverage", "Download Data Coverage Summary")),
      fluidRow(plotOutput("data_tally_plot")),
      fluidRow(dataTableOutput("data_tally")),
      fluidRow(
        box(title = "Aggregate Length Data", width = 12,solidHeader = TRUE, collapsible = TRUE,
        "Use this area to bin your length data by different groups. For example, you can count the total number of individuals in each length bin by year and month and region",
        uiOutput("select_groupers"),
        "Use this area to specify a bin width for your length data (unless your data are already binned)",
        sliderInput("bin_width","Length Bin Width", min = 0, value = 1, max = 20, step = 0.25),
        actionButton("group","Aggregate Data"),
        dataTableOutput("grouped_lcomps"),
        downloadButton("download_grouper", "Download Aggregated Data"),
        )
      ),
      fluidRow(
        box(title = "Plot Aggregated Length Data", width = 12,solidHeader = TRUE, collapsible = TRUE,
        "Use this area to examine your aggregated length data",
        uiOutput("grouped_plot_fill"),
        uiOutput("grouped_plot_facet"),
        checkboxInput("factorfill","Convert Color Variable to Categorical?", value = FALSE),
        actionButton("plot_groupers","Plot Length Composition Data"),
        plotOutput("lcomp_plot")
       )
      )
    ) # close lcomps tab
  )
)
  dashboardPage(
    header,
    sidebar,
    body
  ) # close dashboard Padte

} # close server function
