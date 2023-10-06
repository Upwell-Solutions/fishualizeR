options(shiny.maxRequestSize = 50 * 1024^2)
library(shiny)
library(shinythemes)
library(shinycssloaders)
library(shinydashboard)
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
#library(ggpubr)
library(openxlsx)
#library(ragg)

#TODO: review libraries for use
#TODO: fonts dont work on shinyappsio
# shiny_theme <- hrbrthemes::theme_ipsum(base_size = 14,
#                                        axis_title_size = 16)
# 
# ggplot2::theme_set(shiny_theme)
functions <- list.files(here::here("R"))

#purrr::walk(functions, ~ source(here::here("R", .x)))


function(input,output){


  header  <-  dashboardHeader(title = "fishualizeR")

  # sidebar -----------------------------------------------------------------
  #icons: https://fontawesome.com/v5.15/icons?d=gallery&p=2&m=free or icon("cog", lib = "glyphicon") https://getbootstrap.com/docs/3.4/components/#glyphicons 
  sidebar <- dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Introduction", tabName = "description", icon = icon("book-reader")),
      menuItem("1. Data Inventory", tabName = "inventory", icon = icon("calendar")),
      menuItem("2. Life History Parameters", tabName = "parameters", icon = icon("fish")),
      menuItem("Upload Data File", tabName = "dataUpload", icon = icon("upload")),
      menuItem("Univariate Data Exploration",
               menuSubItem("Text/Categorical Data", tabName = "UniCategorical", icon = icon("chart-line")),
               menuSubItem("Numerical Data", tabName = "UniNumerical", icon = icon("chart-line")),
               menuSubItem("Date Data", tabName = "UniDate", icon = icon("calendar"))
        ),
      menuItem("Multivariate Data Exploration", tabName = "Multivariate", icon = icon("chart-line")),
      menuItem("Data Coverage", tabName = "dataCoverage", icon = icon("percent"))
    )) # close dashboardSidebar


  # body -----------------------------------------------------------------

  body <-  dashboardBody(
    useShinyalert(),
    tabItems(
    tabItem(tabName = "description",
      h1("fishualizeR"),
      p("FishualizeR is an online, publicly available, web-based R Shiny app that helps users visualize common fishery-related data. There are two ways to explore your data: 1) Add your data in the 2 provided templates and follow along with each step contained in the Manual; 2) Upload your data as a CSV and use the various tools in the General Data Exploration tab."),
      a("Click here to download zip file of the templates and example datasets listed below.", href = "Fishualize_Templates_and_Examples.zip", target="_blank", rel="noopener noreferrer"),
      p("Excel Templates:"),
      tags$ol(
        tags$li("Excel_Template1_DataInventory_Events"),
        tags$li("Excel_Template2_LifeHistoryInventory")),
      p("Example or Mock Datasets:"),
      tags$ol(
        tags$li("Figure_3.3.1_mock_data_inventory_events.xlsx"),
        tags$li("Figure_3.3.2_mock_length_freq.csv"),
        tags$li("Box_B6.2.1.1_mock_length_freq_area.csv"),
        tags$li("Box_B6.3.1_B6.3.2_mock_biometric_area_trip_duration.csv"),
        tags$li("Box_B7.1.1_mock_length_freq_area_gear.csv"))
    ),
    
    tabItem(tabName = "inventory",
            fluidRow(
              box(title = "Load Data Inventory File", width = 12, "The data inventory file must be an xlsx following the format of the template provided.", br(), a("Download the data inventory template.", href = "Annex1_Template_DataInventory_Events.xlsx", target="_blank", rel="noopener noreferrer"),br(), br(), fileInput("dataInvFile", NULL))),
            fluidRow(column(4, selectInput("invDataCat", "Which data category to use?", c("Summary", "Catch" = "Catch", "Effort" = "Effort", "Abundance" = "Abundance", "Length" = "Length composition"))),
                     column(4,checkboxInput("inventoryFilterAvailable", "Show only available data sources?"),
                              checkboxInput("inventoryFilterEvents", "Hide Events Timeline?")),
                     column(4, uiOutput("timelineValues"))),
            fluidRow(column(12, plotOutput("dataInvTimeline")))
    ), #close inventory tab
    
    tabItem(tabName = "parameters",
            fluidRow(
              box(title = "Load life history parameter file", width = 12, "The life history parameter inventory file must be an xlsx following the format of the template provided.", br(), a("Download the life history parameter inventory template.", href = "Annex2_Template_LifeHistoryInventory.xlsx", target="_blank", rel="noopener noreferrer"),br(), br(), fileInput("LHParamFile", NULL))),
            fluidRow(column(6, selectInput("LHParamFacet", "Group by category?", c("None", "Area", "Sex", "Methodology"), selected = "None"))),
            fluidRow(hr(style = "border-top: 1px solid #000000;"), 
                     column(6, plotOutput("vonBertGrowthPlot")), column(6,tableOutput("vonBertTable"))),
            br(),
            fluidRow(hr(style = "border-top: 1px solid #000000;"), 
                     column(6,plotOutput("maturityPlot")), column(6,tableOutput("maturityTable"))),
            br(),
            fluidRow(hr(style = "border-top: 1px solid #000000;"), 
                     column(6,plotOutput("lengthWeightPlot")), column(6,tableOutput("lengthWeightTable"))),
            br(),
            fluidRow(hr(style = "border-top: 1px solid #000000;"), 
                     column(6,plotOutput("natMortPlot")), column(6,tableOutput("natMortTable")))
            
            
    ), #close parameter tab
    tabItem(tabName = "dataUpload",
            fluidRow(column(12,h4("Upload the data file (CSV only) that you would like to explore:"))),
            fluidRow(box(title = "Load data file", fileInput("dataFile", NULL), width = 12)),
            conditionalPanel("output.dataFileUploaded == true", 
                             fluidRow(column(6, tableOutput("uploadSummary"))),
                             box(
                               title="Data Preview", width = NULL,status = "primary",
                               div(style = 'overflow-x: scroll',tableOutput("uploadDataHead"))
                              ),
                             fluidRow( column(8,
                                              h3("Column Data Types"), 
                                               "Use this to understand how the data has been loaded by the app. The app, using R programming language, requires each column to contain only one type of data. The app will automatically determine what type of data the column contains. For example, if every data in a column is a number, then the app will determine that the column is numeric (or interger) data. However, if even one data point contains letters (or other non-numeric values), then the app will consider the entire column to be text (character) values. This has implications for how the data can be plotted or analyzed by the app. If a plot or analysis is not behaving how expected, use this table to check if the data is being interpreted as the intended data type. If it is not, then you might need to review the raw data to determine why. For example, if numeric data has text to intdicate a missing value, then it will be interpreted by the app as text (character) data. Possible ways to address this include, creating a subset of the data with the affected rows removed, or creating a version of the dataset where missing values are indicated by blanks (e.g., no data), instead of text. See manual for more details.", br(), 
                                               strong("R Data types"), br(),
                                               h5("\"character\": text data"),
                                               h5("\"numeric\", \"integer\": numbers"),
                                               h5("\"logical\": true\\false")),
                                       tableOutput("uploadColTypes"))
            )
            #tableOutput("uploadSummary")
    ), #close dataUpload tab
    tabItem(tabName = "UniCategorical",
              conditionalPanel("output.dataFileUploaded == false", h4("First, use the Upload Data File Tab to upload data to explore.")),
              conditionalPanel("output.dataFileUploaded == true",
                uiOutput("selectUniqueCol"),
                fluidRow(column(4,textOutput("uniCatMissingValueCountText")),
                         column(8, downloadButton("download_unique_values_table", "Download Summary Table"), align='right')),
                        #column(4, downloadButton("download_unique_values_plot", "Download Plot"))),
                br(),
                fluidRow(column(12, dataTableOutput("uniqueValues"))),
                fluidRow(column(12, plotOutput("uniqueValuesBarPlot")))
              )
    ), #close UniCategorical tab
    tabItem(tabName = "UniNumerical",
            conditionalPanel("output.dataFileUploaded == false", h4("First, use the Upload Data File Tab to upload data to explore.")),
            conditionalPanel("output.dataFileUploaded == true",
              fluidRow(column(4, uiOutput("selectUnivariateNumberCol")), column(4, selectInput("uniPlotType", "Choose plot type:", c("Histogram", "Density", "Box", "Violin")))),
              fluidRow(column(4, tableOutput("uniNumMissingValueCountText")), column(4,tableOutput("summaryStatTable"))), br(), #check if converts "NA" to actual NA values in R, not as strings
              fluidRow(column(12, plotOutput("univariatePlot"))),
              #fluidRow(column(4, downloadButton("download_uni_numerical_table", "Download Summary Table")))
                       #column(4, downloadButton("download_uni_numerical_plot", "Download Plot"))),
            )
    ), #close UniNumerical tab
    tabItem(tabName = "UniDate",
            conditionalPanel("output.dataFileUploaded == false", h4("First, use the Upload Data File Tab to upload data to explore.")),
            conditionalPanel("output.dataFileUploaded == true",
              fluidRow(column(12, h5("The section requires dates to be formatted into 3 columns: Day, Month, Year. All need to be in numerical format. Years must be 4 digits."), checkboxInput("dateRangeCheckbox", "Is this a date range?"))),
              fluidRow(column(3, h5("Select the values for the date:"), uiOutput("selectDateYear"), uiOutput("selectDateMonth"), uiOutput("selectDateDay")),
                       conditionalPanel("input.dateRangeCheckbox == true",
                          column(3, h5("Select the values for the ending date:"), uiOutput("selectEndDateYear"), uiOutput("selectEndDateMonth"), uiOutput("selectEndDateDay")))),
              fluidRow(column(4, actionButton("date_plot_button","Plot Dates"))), br(),
              fluidRow(column(6, tableOutput("dateTable"))),
              fluidRow(column(6, plotOutput("startDatePlot")), conditionalPanel("input.dateRangeCheckbox == true",column(6, plotOutput("endDatePlot")))),
              conditionalPanel("input.dateRangeCheckbox == true",
                               br(), fluidRow(column(6, plotOutput("dateRangePlot")))),
              #fluidRow(column(4, downloadButton("download_uni_date_table", "Download Summary Table")))
                       #column(4, downloadButton("download_uni_date_plot", "Download Plot"))),
              #TODO: handle non numeric and blank values; causes error compiling the date column
            )
    ), #close UniDate tab
    tabItem("Multivariate",
            conditionalPanel("output.dataFileUploaded == false", h4("First, use the Upload Data File Tab to upload data to explore.")),
            conditionalPanel("output.dataFileUploaded == true",
                             fluidRow( column(4, uiOutput("multi_plot_x"),
                                              #checkboxInput("multi_factor_x","Convert X Variable to Categorical?", value = FALSE),
                                                uiOutput("multi_plot_y"),
                                                selectInput("multi_plot_type", "Choose plot type", c("Histogram", "Density", "Box", "Violin"))),
                                       column(4, uiOutput("multi_plot_fill"),
                                              checkboxInput("multi_factorfill","Convert Color Variable to Categorical?", value = FALSE),
                                              uiOutput("multi_plot_factor_x"),
                                              ),
                               
                               column(4, uiOutput("multi_plot_facet"),
                                      selectInput("multi_scales", "Use same scales for faceted plots?", c("Use same for rows and columns" = "fixed", "Vary for rows only" = "free_x", "Vary for columns only" = "free_y", "Vary for rows and columns" = "free")))
                              ),
                             actionButton("multi_plot_button","Plot Data"),
                             br(),
                             br(),
                             plotOutput("multi_plot", height = "100%"), 
                             br(),
                             br(),
                             fluidRow(
                               column(4, h5("Pivot Table")),
                               # column(4, downloadButton("download_multi_plot", "Download Plot")),
                               column(8, downloadButton("dl_multi_table", "Download Table"), align='right')),
                             div(style="overflow-x: scroll", pivottablerOutput("multi_table")),
                             br(),
                             br(),
                             fluidRow(column(12, plotOutput("eventsTimeline")))
            )
    ), #close Multivariate tab
    tabItem(tabName = "dataCoverage", 
              conditionalPanel("output.dataFileUploaded == false", h4("First, use the Upload Data File Tab to upload data to explore.")),  
              conditionalPanel("output.dataFileUploaded == true",
                fluidRow(
                  box(title = "Assess Data Coverage", width = 12, solidHeader = TRUE, collapsible = TRUE,
                      "Use this area to assess your data coverage",
                      uiOutput("dataCoverageVar"),
                      uiOutput("dataCoverageGroup1"),
                      uiOutput("dataCoverageGroup2"),
                      uiOutput("dataCoverageMetric"))),
                fluidRow(column(12, plotOutput("data_tally_plot"))), br(),
                fluidRow(column(12, dataTableOutput("data_tally"))),
                fluidRow(column(4, downloadButton("download_coverage_table", "Download Data Coverage Summary Table")))
                         # column(4, downloadButton("download_coverage_plot", "Download Data Coverage Summary Plot"))),
                ),
            ),
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
     #  fluidRow(
     #    box(title = "Assess Data Coverage", width = 12, solidHeader = TRUE, collapsible = TRUE,
     #        "Use this area to assess your data coverage",
     #        uiOutput("cov_var_1"),
     #        uiOutput("cov_var_2"),
     #        uiOutput("cov_var_3"))),
     # # fluidRow(downloadButton("download_coverage", "Download Data Coverage Summary")),
     #  fluidRow(plotOutput("data_tally_plot_old")),
     #  fluidRow(dataTableOutput("data_tally_old")),
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
