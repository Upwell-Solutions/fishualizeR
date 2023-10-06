#source("R/grouped-plotter.R")

options(shiny.maxRequestSize = 500*1024^2) #increase max upload to 500MB

# getInvLabelCol <- function(column){
#   colName <- switch(column,
#          "None" = "None",
#          "Source Name" = "source_name",
#          "Institution" = "institution",
#          "Fleet" = "fleet")
#   return(colName)
# }

removeNAsByCol <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

gg_facet_nrow <- function(p){
  p %>% ggplot2::ggplot_build()  %>%
    magrittr::extract2('layout')       %>%
    magrittr::extract2('layout') %>%
    magrittr::extract2('ROW')          %>%
    unique()                           %>%
    length()
}

server <- function(input, output, session) {

  # length composition ------------------------------------------------------

  ldata <- reactiveValues(lcomps = NA)
  dataInv <- reactiveValues(inv = NA)
  dataStore <- reactiveValues()

  tmp <- reactive({


    if (input$example == FALSE){
      req(input$file)
    }
    ext <- tools::file_ext(input$file$name)
    switch(ext,
           csv = vroom::vroom(input$file$datapath, delim = ","),
           tsv = vroom::vroom(input$file$datapath, delim = "\t"),
           validate("Invalid file; Please upload a .csv file")
    )

    tmp <- read.csv(input$file$datapath) %>%
      janitor::clean_names()

    # ldata$lcomps <- tmp

    # ldata$oglcomps <- tmp

  })

  observeEvent(input$file, {

    ldata$lcomps <- tmp()

    ldata$oglcomps <- tmp()


  })
  
  readDataInv <- reactive({ #TODO: validate that is xlsx
    tryCatch({
      readDataInv <- read_excel(input$dataInvFile$datapath, sheet = "DataInventory")
    }, 
    error=function(e){
      showNotification(paste("ERROR. No data inventory tab. Need to use template format."), duration = 0)
    })
  })
  
  observeEvent(input$dataInvFile, {

    tryCatch({
      dataInv$inv <- readDataInv() %>%
        janitor::clean_names()
      dataInv$timeline <- readTimeline() %>%
        janitor::clean_names()
    },
    error=function(e){
      showNotification(paste("ERROR. Need to use template format."), duration = 0)
    })

  })
  
  readTimeline <- reactive({
    tryCatch({
      readTimeline <- read_excel(input$dataInvFile$datapath, sheet = "Timeline")
    }, 
    error=function(e){
      showNotification(paste("ERROR. No timeline tab. Need to use template format."), duration = 0)
    })
  })
  
  observeEvent(input$LHParamFile, {
    # tryCatch({
      dataInv$vonBParams <- readvonBParams()
      dataInv$natMortParams <- readNatMortParams()
      dataInv$maturityParams <- readMaturityParams()
      dataInv$lenWeightParams <- readLenWeightParams()
    # }, 
    # error=function(e){
    #  # showNotification(paste("ERROR. Could not load file. Need to use template format."), duration = 0)
    # })
    
    
  })
  
  readvonBParams <- reactive({
    tryCatch({
      readvonBParams <- read_excel(input$LHParamFile$datapath, sheet = "vonBert")
    }, 
    error=function(e){
      showNotification(paste("ERROR. No von Bertalanffy tab. Need to use template format."), duration = 0)
    })
  })
  
  readNatMortParams <- reactive({
    tryCatch({
      readNatMortParams <- read_excel(input$LHParamFile$datapath, sheet = "Mortality")
    }, 
    error=function(e){
      showNotification(paste("ERROR. No natural mortality tab. Need to use template format."), duration = 0)
    })
    
    
  })
  
  readMaturityParams <- reactive({
    tryCatch({
      readMaturityParams <- read_excel(input$LHParamFile$datapath, sheet = "Maturity")
    }, 
    error=function(e){
      showNotification(paste("ERROR. No maturity tab. Need to use template format."), duration = 0)
    })
  })
  readLenWeightParams <- reactive({
    tryCatch({
      readLenWeightParams <- read_excel(input$LHParamFile$datapath, sheet = "LW")
    }, 
    error=function(e){
      showNotification(paste("ERROR. No length/weight tab. Need to use template format."), duration = 0)
    })
  })
  
  observeEvent(input$dataFile, { #TODO: handle both csv and xlsx? no xlsx is too slow
    dataStore$d <- readDataFile()
  })
  
  readDataFile <- reactive({
    req(input$dataFile)
    readDataFile <- read.csv(input$dataFile$datapath)
    
  })
  
  output$uploadColTypes <- renderTable({
    fileSummary <- data.frame("Column_Name" = colnames(dataStore$d), "Data_Type"=unlist(map(dataStore$d,class),use.names = FALSE))
    colnames(fileSummary)[1] <- "Column Name"
    colnames(fileSummary)[2] <- "Data Type"
    return(fileSummary)
  })
  
  output$uploadSummary <- renderTable({
    sumTable <- data.frame(c("Number of Columns", "Number of Rows"), c(length(dataStore$d), nrow(dataStore$d)))
    return(sumTable)
  }, colnames = FALSE)
  
  output$uploadDataHead <- renderTable({
    head(dataStore$d)
  })
  
  output$dataFileUploaded <- reactive({
    return(!is.null(dataStore$d))
  })
  
  output$inventoryUploaded <- reactive({
    return(!is.null(dataInv$timeline))
  })
  
  outputOptions(output, 'dataFileUploaded', suspendWhenHidden=FALSE)
  
  output$selectUniqueCol <- renderUI({
    
    vars <- colnames(dataStore$d) #[map_lgl(dataStore$d, is.character)]
    
    #vars <- c(NA,vars)
    
    selectizeInput("selectUniqueCol",
                   "Select the column to see unique values",
                   vars)
  })

  output$uniqueValues <- renderDataTable({
    uniqueValuesTable()
  })
  
  uniqueValuesTable <- function(){
    req(input$selectUniqueCol)
    uniqueVals <- count(dataStore$d, !!sym(input$selectUniqueCol)) %>%
      rename(Count = n)
    uniqueVals[input$selectUniqueCol] <- paste0("\'", uniqueVals[[input$selectUniqueCol]], "\'")

    return(uniqueVals)
  }
  
  output$download_unique_values_table <- downloadHandler(
    filename = function() {
      paste(input$selectUniqueCol, "categories-table.csv")
    },
    content = function(file) {
      vroom::vroom_write(uniqueValuesTable(), file, ",")
    }
  )
  
  output$uniCatMissingValueCountText <- renderText({
    return(paste0("Missing values: ", sum(is.na(dataStore$d[input$selectUniqueCol]))))
  })
  
  output$uniNumMissingValueCountText <- renderTable({
    numMissingVals <- sum(is.na(dataStore$d[input$selectUnivariateNumberCol]))
    totalRows <- nrow(dataStore$d)
    numDataRows <- totalRows - numMissingVals
    
    missingTable <- data.frame(c("Rows with Data", "Rows Missing Values", "Total Rows"), c(numDataRows, numMissingVals, totalRows))
    return(missingTable)
  }, colnames = FALSE)
  
  output$uniqueValuesBarPlot <- renderPlot({
    uniqueValsPlot()
  })
  
  uniqueValsPlot <- function(){
    uniqueValsBarPlot <- ggplot(dataStore$d, aes(x=factor(!!sym(input$selectUniqueCol)))) +
      geom_bar()
    
    return(uniqueValsBarPlot)
  }
  
  # output$download_unique_values_plot <- downloadHandler(
  #   filename = function() {
  #     paste(input$selectUniqueCol, "categories-plot.png")
  #   },
  #   content = function(file) {
  #     ggsave(file, plot = uniqueValsPlot(), device = "png")
  #   }
  # )
  
  output$selectUnivariateNumberCol <- renderUI({
    vars <- colnames(dataStore$d[map_lgl(dataStore$d, is.numeric)]) #TODO: some numeric columns have 'ND' or similar. discuss with Ricky, maybe if you don't see if then it has text and need to go clean, replace with NA/blank values
   # vars <- c(NA,vars)
    selectizeInput("selectUnivariateNumberCol",
                    "Select the column to see summary statistics (numeric columns only)",
                    vars)
  })
  
  univariateDF_NoNAs <- reactive({
    colForSummary <- input$selectUnivariateNumberCol
    univariateDF_NoNAs <- removeNAsByCol(dataStore$d, colForSummary)
  })
  
  output$summaryStatTable <- renderTable({
    uniSummaryTable()
  }, colnames = FALSE)
  
  uniSummaryTable <- function(){
    req(input$selectUnivariateNumberCol)
    # if(input$selectUnivariateNumberCol != "NA"){
    colForSummary <- input$selectUnivariateNumberCol
    # df_NoNAs <- removeNAsByCol(dataStore$d, colForSummary)
    df_NoNAs <- univariateDF_NoNAs()
    min <- min(df_NoNAs[[colForSummary]])
    max <- max(df_NoNAs[[colForSummary]])
    median <- median(df_NoNAs[[colForSummary]])
    mean <- mean(df_NoNAs[[colForSummary]])
    sd <- sd(df_NoNAs[[colForSummary]])
    #summary(df_NoNAs[colForSummary])
    summaryTable <- data.frame(c("Min", "Max", "Median", "Mean", "Std. Dev."), c(min, max, median, mean, sd))
    return(summaryTable)
    # }
  }
  
  # output$download_uni_numerical_table <- downloadHandler(
  #   filename = function() {
  #     paste(input$selectUnivariateNumberCol, "summary-table.csv")
  #   },
  #   content = function(file) {
  #     vroom::vroom_write(uniSummaryTable(), file, ",")
  #   }
  # )
  
  output$univariatePlot <- renderPlot({
    uniNumericPlot()
  })
  
  uniNumericPlot <- function(){
    req(input$selectUnivariateNumberCol)
    df_NoNAs <- univariateDF_NoNAs()
    plotType <- input$uniPlotType
    
    if(plotType == "Histogram"){
      plot <- ggplot(df_NoNAs, aes(x=!!sym(input$selectUnivariateNumberCol))) + geom_histogram()
    } else if(plotType == "Box"){
      plot <- ggplot(df_NoNAs, aes(x=factor(0), y=!!sym(input$selectUnivariateNumberCol))) + geom_boxplot() +
        scale_x_discrete(breaks = NULL) +
        xlab(NULL)
    } else if(plotType == "Violin"){
      plot <- ggplot(df_NoNAs, aes(x=factor(0), y=!!sym(input$selectUnivariateNumberCol))) + geom_violin() +
        scale_x_discrete(breaks = NULL) +
        xlab(NULL)
    } else if(plotType == "Density"){
      plot <- ggplot(df_NoNAs, aes(x=!!sym(input$selectUnivariateNumberCol))) + geom_density() 
    }
    
    return(plot)
  }
  
  # output$download_uni_numerical_plot <- downloadHandler(
  #   filename = function() {
  #     paste(input$selectUnivariateNumberCol, input$uniPlotType, "-plot.png")
  #   },
  #   content = function(file) {
  #     ggsave(file, plot = uniNumericPlot(), device = "png")
  #   }
  # )
  
  output$selectDateDay <- renderUI({
    vars <- c("<No day data>", colnames(dataStore$d))

    selectizeInput("selectDateDay",
                   "Day (optional):",
                   vars)
  })
  
  output$selectDateMonth <- renderUI({
    vars <- colnames(dataStore$d) 
    
    selectizeInput("selectDateMonth",
                   "Month:",
                   vars)
  })
  
  output$selectDateYear <- renderUI({
    vars <- colnames(dataStore$d) 
    
    selectizeInput("selectDateYear",
                   "Year:",
                   vars,
                   multiple = FALSE)
  })
  
  output$selectEndDateDay <- renderUI({
    vars <- c("<No day data>", colnames(dataStore$d)) 
    
    selectizeInput("selectEndDateDay",
                   "End Day (Optional):",
                   vars)
  })
  
  output$selectEndDateMonth <- renderUI({
    vars <- colnames(dataStore$d) 
    
    selectizeInput("selectEndDateMonth",
                   "End Month:",
                   vars)
  })
  
  output$selectEndDateYear <- renderUI({
    vars <- colnames(dataStore$d) 
    
    selectizeInput("selectEndDateYear",
                   "End Year:",
                   vars)
  })

  
  startDatePlot <- eventReactive(input$date_plot_button, {
    startDtPlot()
  })
  
  startDtPlot <- function(){
    if(input$selectDateDay == "<No day data>"){
      #set day to 1 for all
      dataStore$dateTmpTable <- dataStore$d %>% mutate(fishRStartDate = as.Date(paste(!!sym(input$selectDateYear), !!sym(input$selectDateMonth), 1, sep = "-")))
    } else {
      dataStore$dateTmpTable <- dataStore$d %>% mutate(fishRStartDate = as.Date(paste(!!sym(input$selectDateYear), !!sym(input$selectDateMonth), !!sym(input$selectDateDay), sep = "-")))
    }
    plot <- ggplot(dataStore$dateTmpTable, aes(x=fishRStartDate)) + geom_histogram()
    return(plot)
  }
  

  
  output$dateTable <- renderTable({
    dateTable()
  }, colnames = FALSE)
  
  endDatePlot <- eventReactive(input$date_plot_button, {
    endDtPlot()
  })
  
  endDtPlot <- function(){
    if(input$selectEndDateDay == "<No day data>"){
      #set day to 1 for all
      dataStore$dateTmpTable <- dataStore$dateTmpTable %>% mutate(fishREndDate = as.Date(paste(!!sym(input$selectEndDateYear), !!sym(input$selectEndDateMonth), 1, sep = "-")))
    } else {
      dataStore$dateTmpTable <- dataStore$dateTmpTable %>% mutate(fishREndDate = as.Date(paste(!!sym(input$selectEndDateYear), !!sym(input$selectEndDateMonth), !!sym(input$selectEndDateDay), sep = "-")))
    }
    plot <- ggplot(dataStore$dateTmpTable, aes(x=fishREndDate)) + geom_histogram()
    return(plot)
  }
  
  output$endDatePlot <- renderPlot(
    endDatePlot()
  )
  
  dateRangePlot <- eventReactive(input$date_plot_button, {
    dtRangePlot()
  })
  
  dtRangePlot <- function(){
    dataStore$dateTmpTable <- dataStore$dateTmpTable %>% mutate(fishRDateRange = fishREndDate - fishRStartDate)
    plot <- ggplot(dataStore$dateTmpTable, aes(x=fishRDateRange)) + geom_histogram()
    return(plot)
  }
  
  output$dateRangePlot <- renderPlot(
    dateRangePlot()
  )
  
  output$startDatePlot <- renderPlot(
    startDatePlot()
  )
  
  # output$download_uni_date_plot <- downloadHandler(
  # 
  #   filename = function() {
  #     paste("date-plot.png")
  #   },
  #   
  #   content = function(file) {
  #     if(input$dateRangeCheckbox){
  #       start <- startDtPlot()
  #       end <- endDtPlot()
  #       range <- dtRangePlot()
  #       combinedPlots <- ggarrange(start, end, range)
  #       ggsave(file, plot = combinedPlots, device = "png")
  #     } else {
  #       ggsave(file, plot = startDtPlot(), device = "png")
  #     }
  #   }
    
  #)
  
  dateTable <- eventReactive(input$date_plot_button, {
    dateSummaryTable()
  })
  
  dateSummaryTable <- function(){
    #Note: this doesn't remove rows so will calculate all rows with a start date (or all w end date) regardless if there is a matching end date
    #if wanting to only look at rows with both a start AND end date,  then need to remove rows before calculating both
    
    #TODO: pull into function so only calculating once between table and plots and downloading
    
    if(input$selectDateDay == "<No day data>"){
      #set day to 1 for all
      dataStore$dateTmpTable <- dataStore$d %>% mutate(fishRStartDate = as.Date(paste(!!sym(input$selectDateYear), !!sym(input$selectDateMonth), 1, sep = "-")))
    } else {
      dataStore$dateTmpTable <- dataStore$d %>% mutate(fishRStartDate = as.Date(paste(!!sym(input$selectDateYear), !!sym(input$selectDateMonth), !!sym(input$selectDateDay), sep = "-")))
    }
    
    startMin <- format(min(dataStore$dateTmpTable$fishRStartDate, na.rm = TRUE),'%Y-%m-%d')
    startMax <- format(max(dataStore$dateTmpTable$fishRStartDate, na.rm = TRUE),'%Y-%m-%d')
    startMedian <- format(median(dataStore$dateTmpTable$fishRStartDate, na.rm = TRUE),'%Y-%m-%d')
    startMean <- format(mean(dataStore$dateTmpTable$fishRStartDate, na.rm = TRUE),'%Y-%m-%d')
    
    if(!input$dateRangeCheckbox){
      summaryTable <- data.frame(c("","Min", "Max", "Median", "Mean"), c("Date", startMin, startMax, startMedian, startMean))
    } else {
      if(input$selectEndDateDay == "<No day data>"){
        #set day to 1 for all
        dataStore$dateTmpTable <- dataStore$dateTmpTable %>% mutate(fishREndDate = as.Date(paste(!!sym(input$selectEndDateYear), !!sym(input$selectEndDateMonth), 1, sep = "-")))
      } else {
        dataStore$dateTmpTable <- dataStore$dateTmpTable %>% mutate(fishREndDate = as.Date(paste(!!sym(input$selectEndDateYear), !!sym(input$selectEndDateMonth), !!sym(input$selectEndDateDay), sep = "-")))
      }
      
      dataStore$dateTmpTable <- dataStore$dateTmpTable %>% mutate(fishRDateRange = fishREndDate - fishRStartDate)
      
      endMin <- format(min(dataStore$dateTmpTable$fishREndDate, na.rm = TRUE),'%Y-%m-%d')
      endMax <- format(max(dataStore$dateTmpTable$fishREndDate, na.rm = TRUE),'%Y-%m-%d')
      endMedian <- format(median(dataStore$dateTmpTable$fishREndDate, na.rm = TRUE),'%Y-%m-%d')
      endMean <- format(mean(dataStore$dateTmpTable$fishREndDate, na.rm = TRUE),'%Y-%m-%d')
      
      rangeMin <- min(dataStore$dateTmpTable$fishRDateRange, na.rm = TRUE)
      rangeMax <- max(dataStore$dateTmpTable$fishRDateRange, na.rm = TRUE)
      rangeMedian <- median(dataStore$dateTmpTable$fishRDateRange, na.rm = TRUE)
      rangeMean <- round(mean(dataStore$dateTmpTable$fishRDateRange, na.rm = TRUE), 1)
      
      summaryTable <- data.frame(c("","Min", "Max", "Median", "Mean"), 
                                 c("Start Date", startMin, startMax, startMedian, startMean),
                                 c("End Date", endMin, endMax, endMedian, endMean),
                                 c("Range", rangeMin, rangeMax, rangeMedian, rangeMean))
    }
    
    return(summaryTable)
  }
  
  # output$download_uni_date_table <- downloadHandler(
  #   filename = function() {
  #     "date-summary-table.csv"
  #   },
  #   content = function(file) {
  #     vroom::vroom_write(dateSummaryTable(), file, ",")
  #   }
  # )
  
  output$dataInvTimeline <- 
    renderPlot({
      req(input$dataInvFile)
      timelinePlot();
    })
  
  output$eventsTimeline <- 
    renderPlot({
      req(input$dataInvFile)
      timelinePlot(TRUE);
    })
  
  timelinePlot <- function(eventsOnly = FALSE){
    if(input$invDataCat != "Summary"){
      inventoryData <- dataInv$inv %>% mutate(rowID = paste(data_source, sector, gear, area, sep="_"))
      inventoryData <- filter(inventoryData, data_type_category==input$invDataCat)
    } else {
      inventoryData <- dataInv$inv %>% mutate(rowID = paste(data_source, sector, sep="_"))
    }
    
    if(input$inventoryFilterAvailable){
      inventoryData <- filter(inventoryData, available=="Yes")
    }
    
    eventData <- dataInv$timeline
    eventData$data_type_category <- "Major Events"
    eventData$data_type_sub_category <- "Major Events"
    colnames(eventData)[1] <- "data_source"
    eventData$rowID <- eventData$data_source


    combinedTimelineData <- merge(inventoryData, eventData, all = TRUE) %>%
      mutate(end_year = if_else(!is.na(end_year), as.Date(paste0(end_year, "-12-31")), as.Date(paste0(start_year, "-12-31")))) %>% #set any na value to the end of the start year so it shows on graph
      mutate(start_year = as.Date(paste0(start_year, "-01-01"))) 
    #TODO: handle not having these or lock the template so cant remove
    if("Catch Peaks" %in% combinedTimelineData$data_source){
      catchPeakDate <- combinedTimelineData[combinedTimelineData$data_source == "Catch Peaks", "start_year"]
      catchPeakYear <- as.numeric(format(catchPeakDate, '%Y'))
    } else {
      showNotification(paste("ERROR. No \"Catch Peaks\" event. This event is required."), duration = 0)
    }
      
    if("Fishery start (approximate if not known)" %in% combinedTimelineData$data_source){
      fisheryStartDate <- combinedTimelineData[combinedTimelineData$data_source == "Fishery start (approximate if not known)", "start_year"]
      fisheryStartYear <- as.numeric(format(fisheryStartDate, '%Y'))
    } else {
      showNotification(paste("ERROR. No \"Fishery start (approximate if not known)\" event. This event is required"), duration = 0)
    }
    
    combinedTimelineData <- combinedTimelineData[!(combinedTimelineData$data_source=="Catch Peaks" | combinedTimelineData$data_source == "Fishery start (approximate if not known)"),]
    
    if(input$inventoryFilterEvents & !eventsOnly){
      combinedTimelineData <- filter(combinedTimelineData, data_type_category != "Major Events")
    }
    

    
    # mutate(end_date = as.Date(if_else(as.Date(as.character(end_date)) - as.Date(as.character(start_date)) < 30,
    #                                   as.character(as.Date(as.character(end_date))+30),
    #                                   as.character(end_date))))
    # rowLabelCol <- getInvLabelCol(input$inventoryRowLabel) #event = combinedTimelineData[[rowLabelCol]]
    # inlineLabelCol <- getInvLabelCol(input$inventoryInlineLabel)
    #View(combinedTimelineData)
    #p <- ggplot()
    if(eventsOnly){
      combinedTimelineData <- filter(combinedTimelineData, data_type_category == "Major Events")
      timelinePlot <- plot_timeline(event = combinedTimelineData$rowID,
                                    start = combinedTimelineData$start_year,
                                    end = combinedTimelineData$end_year,
                                    #label = combinedTimelineData$fleet,
                                    group = combinedTimelineData$data_type_category,#TODO: pass as factor to set order
                                    title = "Timeline of Major Events",
                                    subtitle = "",
                                    save = FALSE)
    }
    else if(input$invDataCat == "Summary"){
      timelinePlot <- plot_timeline(event = combinedTimelineData$rowID,
                                    start = combinedTimelineData$start_year,
                                    end = combinedTimelineData$end_year,
                                    #label = combinedTimelineData$fleet,
                                    group = combinedTimelineData$data_type_category,#TODO: pass as factor to set order
                                    title = "Visualization of Data Inventory and Timeline of Influential Events",
                                    subtitle = "",
                                    save = FALSE)
    } else {
      timelinePlot <- plot_timeline(event = combinedTimelineData$rowID,
                                    start = combinedTimelineData$start_year,
                                    end = combinedTimelineData$end_year,
                                    #label = combinedTimelineData$fleet,
                                    group = combinedTimelineData$data_type_sub_category,#TODO: pass as factor to set order
                                    title = "Visualization of Data Inventory and Timeline of Influential Events",
                                    subtitle = "",
                                    save = FALSE)
    }
    
    earliestDate <- min(combinedTimelineData$start_year)
    earliestYear <- as.numeric(format(earliestDate, '%Y'))
    
    graphOrigin <- ggplot_build(timelinePlot)$layout$panel_scales_x[[1]]$range$range[1]
    catchPeakDaysFromGraphOrigin <- graphOrigin + (catchPeakYear - earliestYear) * 365
    fisheryStartDaysFromGraphOrigin <- graphOrigin + (fisheryStartYear - earliestYear) * 365
    
    timelinePlot + geom_vline(aes(xintercept = catchPeakDaysFromGraphOrigin, linetype="dotdash"), size=1) +
      geom_vline(aes(xintercept = fisheryStartDaysFromGraphOrigin, linetype="solid"), size=1) +
      scale_linetype_identity(name = "", guide = guide_legend(reverse=TRUE),
                              labels = c("Catch Peak", "Fishery Start")) + theme(legend.position = "bottom")
  }
  
  output$timelineValues <- renderTable({
    req(input$dataInvFile)
    eventData <- data.frame(dataInv$timeline)
    catchPeakYear <- eventData[eventData$influential_event_or_change == "Catch Peaks", "start_year"]
    fisheryStartYear <- eventData[eventData$influential_event_or_change == "Fishery start (approximate if not known)", "start_year"]

    timelineTable <- data.frame(c("Start of Fishery", "Catch Peak"), c(as.integer(fisheryStartYear), as.integer(catchPeakYear)))
    return(timelineTable)
  }, colnames = FALSE)
  
  # output$download_inventory <- downloadHandler(
  #   filename = function() {
  #     "data-inventory.png"
  #   },
  #   content = function(file) {
  #     ggsave(file, plot = timelinePlot(), device = "png")
  #   }
  # )
  # 
  # output$download_LH <- downloadHandler(
  #   filename = function() {
  #     "life-history-plots.png"
  #   },
  #   content = function(file) {
  #     vb <- vonBertPlot()
  #     mat <- matPlot()
  #     lw <- lwPlot()
  #     natM <- natMPlot()
  #     combinedPlots <- ggarrange(vb, mat, lw, natM)
  #     ggsave(file, plot = combinedPlots, device = "png")
  #   }
  # )
  
  output$vonBertGrowthPlot <- renderPlot({
    vonBertPlot()
  })
  
  vonBertPlot <- function(){
    req(input$LHParamFile)
    vonBParams <- dataInv$vonBParams
    paramDF <- data.frame(vonBParams, group = as.character(1:length(vonBParams$Linf)))
    vonBerteq = function(x, Linf, k, t0){Linf*(1-exp(-k*(x-t0)))}
    x <- seq(0,20,1)
    vonBFunc_data <- lapply(1:nrow(paramDF),function(i) {
      args <- paramDF[i,c("Linf","k","t0")]
      data.frame(row=paramDF$group[i],
                 x=x,
                 y=do.call(vonBerteq,c(list(x=x),args)),
                 Area=paramDF$Area[i],
                 Sex=paramDF$Sex[i],
                 Methodology=paramDF$Methodology[i])
    }) %>% bind_rows
    vonBPlot <- ggplot(vonBFunc_data,aes(x=x, y=y, colour=row, linetype=Sex)) + geom_line(lwd=1.1) + labs(title="von Bertalanffy Growth Curves", x = "Years", y="Length")
    if(input$LHParamFacet != "None"){
      vonBPlot <- vonBPlot + facet_wrap(input$LHParamFacet)
    }
    return(vonBPlot)
  }
  
  output$vonBertTable <- renderTable({
    req(input$LHParamFile)
    vonBParams <- dataInv$vonBParams
    paramDF <- data.frame(row = as.character(1:length(vonBParams$Linf)),vonBParams)
    return(paramDF)
  })
  
  output$maturityPlot <- renderPlot({
   matPlot()
  })
  
  matPlot <- function(){
    req(input$LHParamFile)
    mateq <- function(x, L50, L95){1 / (1+exp(-log(19)*((x-L50)/(L95-L50))))}
    matData <- dataInv$maturityParams
    #TODO: determine label for lines
    matParamDF <- data.frame(matData, row = as.character(1:length(matData$L50)))
    x <- seq(0,150,1) #TODO: get max size to plot
    matFunc_data <- lapply(1:nrow(matParamDF),function(i) {
      args <- matParamDF[i,c("L50","L95")]
      data.frame(row=matParamDF$row[i],
                 x=x,
                 y=do.call(mateq,c(list(x=x),args)),
                 Area=matParamDF$Area[i],
                 Sex=matParamDF$Sex[i],
                 Methodology=matParamDF$Methodology[i])
    }) %>% bind_rows
    matPlot <- ggplot(matFunc_data,aes(x=x, y=y, colour=row, linetype=Sex)) + geom_line(lwd=1) +
      labs(title="Maturity Curves", x = "Length", y="% Mature")
    
    if(input$LHParamFacet != "None"){
      matPlot <- matPlot + facet_wrap(input$LHParamFacet)
    }
    
    return(matPlot)
  }
  
  output$maturityTable <- renderTable({
    req(input$LHParamFile)
    #mateq <- function(x, L50, L95){1 / (1+exp(-log(19)*((x-L50)/(L95-L50))))}
    matData <- dataInv$maturityParams
    #TODO: determine label for lines
    matParamDF <- data.frame(row = as.character(1:length(matData$L50)), matData)
    return(matParamDF)
  })
  
  output$lengthWeightPlot <- renderPlot({
    lwPlot()
  })
  
  lwPlot <- function(){
    req(input$LHParamFile)
    LWeq <- function(x, a, b){a*x^b}
    lwData <- dataInv$lenWeightParams
    
    lwParamDF <- data.frame(lwData, row = as.character(1:nrow(lwData)))
    # TODO: determine line label
    #LWline1Label <- paste(test$Reference[1], test$Sex[1], test$Area[1], sep="_")
    x <- seq(0,100,1) #TODO: what should be max length
    lwFunc_data <- lapply(1:nrow(lwParamDF),function(i) {
      args <- lwParamDF[i,c("a","b")]
      data.frame(row=lwParamDF$row[i],
                 x=x,
                 y=do.call(LWeq,c(list(x=x),args)),
                 Area=lwData$Area[i],
                 Sex=lwData$Sex[i],
                 Methodology=lwData$Methodology[i])
    }) %>% bind_rows
    
    lenWeightPlot <- ggplot(lwFunc_data,aes(x=x, y=y, colour=row, linetype=Sex)) + geom_line(lwd=1) +
      labs(title="Length-Weight Curves", x = "Length", y="Weight")
    
    if(input$LHParamFacet != "None"){
      lenWeightPlot <- lenWeightPlot + facet_wrap(input$LHParamFacet)
    }
    
    return(lenWeightPlot)
  }
  
  output$lengthWeightTable <- renderTable({
    req(input$LHParamFile)
    LWeq <- function(x, a, b){a*x^b}
    lwData <- dataInv$lenWeightParams
    lwParamDF <- data.frame(row = as.character(1:length(lwData$a)), lwData)
    return(lwParamDF)
  })
  
  output$natMortPlot <- renderPlot({
    natMPlot()
  })
  
  natMPlot <- function(){
    req(input$LHParamFile)
    natMortData <- dataInv$natMortParams
    natMortDF <- data.frame(natMortData)
    
    natMortPlot <- ggplot(natMortDF, aes(x=Mortality)) + #xlim(0,1) +
      labs(title="Natural Mortality Density Plot", x = "Natural Mortality", y="Density")

    if(input$LHParamFacet != "None"){
      facet <- sym(input$LHParamFacet)
      mu <- natMortDF %>% group_by(!!facet) %>% summarize(grp.mean=mean(Mortality))
      natMortPlot <- natMortPlot + geom_density(alpha=.5) + aes(fill=!!facet) + 
        geom_vline(data=mu, aes(xintercept=grp.mean, color=!!facet), linetype="dashed")
    } else{
      mu <- natMortDF %>% summarize(grp.mean=mean(Mortality))
      natMortPlot <- natMortPlot + geom_density(alpha=.5, fill="lightblue") + 
        geom_vline(data=mu, aes(xintercept=grp.mean), linetype="dashed")
    }
    
    return(natMortPlot)
  }
  
  output$natMortTable <- renderTable({
    req(input$LHParamFile)
    natMortData <- dataInv$natMortParams
    natMortDF <- data.frame(row = as.character(1:length(natMortData$Sex)), natMortData)
    return(natMortDF)
  })


  tmp2 <- reactive({

    lcomps <- read.csv(here::here("data","BiometricosTable.csv")) %>%
      janitor::clean_names()

  })

  observeEvent(input$example,{

    if (input$example == TRUE){

    ldata$lcomps <- tmp2()

    ldata$oglcomps <- tmp2()
    }

  })



  output$max_length <- renderUI({

    if (input$example == FALSE){
    req(input$file)
    }
    sliderInput(
      "max_length",
      "Select Max. Realistic Length",
      min = 0,
      max = max(ldata$lcomps[, input$select_ldata], na.rm = TRUE),
      value =  max(ldata$lcomps[, input$select_ldata], na.rm = TRUE)
    )

  })

  output$max_length_factor <- renderUI({

    if (input$example == FALSE){
      req(input$file)
    }
    ldat <- ldata$lcomps[[input$select_ldata]]

    ratio_guess <-
      plyr::round_any(mean(ldat[ldat > input$max_length], na.rm = TRUE) / mean(ldat[ldat < input$max_length], na.rm = TRUE), 10)

    numericInput(
      "max_length_factor",
      "Enter Max Length Conversion",
      min = 0,
      max = NA,
      value =  ratio_guess
    )

  })

  output$min_length <- renderUI({
    sliderInput(
      "min_length",
      "Select Min. Realistic Length",
      min = 0,
      max = max(ldata$lcomps[, input$select_ldata], na.rm = TRUE),
      value = 0
    )
  })

  output$min_length_ratio <- renderUI({
    ldat <- ldata$lcomps[[input$select_ldata]]

    ratio_guess <-
      plyr::round_any(mean(ldat[ldat < input$min_length], na.rm = TRUE) / mean(ldat[ldat > input$min_length], na.rm = TRUE), 10)

    numericInput(
      "min_length_factor",
      "Enter Min Length Conversion",
      min = 0,
      max = NA,
      value =  ratio_guess
    )

  })


observeEvent(input$correct_units, {

    clcomps <- ldata$lcomps

    tmp <- clcomps[[input$select_ldata]]

    clcomps[tmp > input$max_length & !is.na(tmp) , input$select_ldata] <-
      clcomps[tmp > input$max_length & !is.na(tmp), input$select_ldata] / input$max_length_factor

    ldata$lcomps <- clcomps

    updateTabsetPanel(session, inputId="inspect", selected = "Plot Raw Data")

  })

observeEvent(input$reset, {

  ldata$lcomps <- ldata$oglcomps

})


  # output$cleaned_lcomps <-
  #   renderDataTable(clcomps(),  options = list(pageLength = 5))
  #
  #

 # observeEvent(input$correct_units,{lcomps <- clcomps()})


output$colnames <- renderUI({

  if (input$example == FALSE){
    req(input$file)
  }
  selectInput("colnames", "Select Columns To View", choices = colnames(ldata$lcomps), selected = colnames(ldata$lcomps)[1:pmin(10, ncol(ldata$lcomps))],multiple = TRUE, selectize = TRUE)

})

  output$lcomps <-
    DT::renderDataTable({

      if (input$example == FALSE){
        req(input$file)
      }
      ldata$lcomps[,input$colnames]},
                    filter = "top",
                    options = list(pageLength = 5, autoWidth = TRUE))
  

####**********multivariate plotting (adapted from raw_plot)************
  # inspect raw data
  
  output$multi_plot_x <- renderUI({
    vars <- c(NA, colnames(dataStore$d))
    selectInput("multi_x",
                "Select first variable:",
                vars,
                multiple = FALSE)
  })
  
  output$multi_plot_y <- renderUI({
    vars <- c(NA,  colnames(dataStore$d))
    selectInput("multi_y",
                "Select second variable:",
                vars,
                multiple = FALSE)
  })
  
  output$multi_plot_factor_x <- renderUI({
    vars <- c(NA,  colnames(dataStore$d))
    selectInput("multi_factor_x",
                "Choose factor for X axis (box and violin plots only)",
                vars,
                multiple = FALSE)
  })
  
  # output$multi_plot_type <- renderUI({
  #   vars <- c("Histogram", "Density", "Box", "Violin")
  #   if(is.na(input$multi_y)){
  #     vars <- c("Scatterplot")
  #   }
  #   selectInput("multi_type",
  #               "Choose what type of plot",
  #               vars,
  #               multiple = FALSE)
  # })
  plotChoiceTrigger <- reactive({
    list(input$multi_x, input$multi_y)
  })
  
  observeEvent(ignoreInit = TRUE, c(input$multi_factor_x, input$multi_x, input$multi_y),{

    if (input$multi_factor_x != "NA"){
      vars <- c("Box", "Violin")
    } else if(input$multi_y != "NA" && input$multi_x != "NA"){
      vars <- c("Scatterplot", "Line Graph", "Stacked Bar Graph")
    } else {
      vars <- c("Histogram", "Density", "Box", "Violin")
    }
    updateSelectInput(session, "multi_plot_type", choices = vars)
  })
  
  output$multi_plot_fill <- renderUI({
    vars <- c(NA, colnames(dataStore$d))
    selectInput("multi_fill",
                "Choose what to color by",
                vars,
                multiple = FALSE)
  })
  
  output$multi_plot_facet <- renderUI({
    vars <- c(NA, colnames(dataStore$d))
    selectInput("multi_facet",
                "Choose what to facet by",
                vars,
                multiple = FALSE)
  })
  
  multi_plot <- eventReactive(input$multi_plot_button, {
    grouped_plotter(
      dataStore$d,
      x = input$multi_x,
      y = input$multi_y,
      fill = input$multi_fill,
      facet = input$multi_facet,
      factorX = input$multi_factor_x,
      factorfill = input$multi_factorfill,
      scales = input$multi_scales,
      plotType = input$multi_plot_type
    )
  })
  
  hMultiPlot <- reactive(gg_facet_nrow(multi_plot()))
  
  output$multi_plot <- renderPlot({
    withProgress(message = 'Generating plot', value = 0, {
      multi_plot()
      
    })
  }, height = function(){200 + hMultiPlot()*200})
  
  # output$download_multi_plot <- downloadHandler(
  #   filename = function() {
  #     "multivariate-plot.png"
  #   },
  #   content = function(file) {
  #     ggsave(file, plot = multi_plot(), device = "png")
  #   }
  # )
  
  multi_table <- eventReactive(input$multi_plot_button,{
    pt <- multi_table_raw()
    pivottabler(pt)
  })
  
  multi_table_raw <- function(){
    mainVar <- input$multi_x


    pt <- PivotTable$new()
    pt$addData(removeNAsByCol(dataStore$d, mainVar))
    if(input$multi_fill != "NA"){
      pt$addRowDataGroups(input$multi_fill)
    }
    if(input$multi_facet != "NA"){
      pt$addColumnDataGroups(input$multi_facet)
    }
    
    pt$defineCalculation(calculationName="Count", summariseExpression="n()")
    
    # if(input$pivotTableStatInput == "mean"){
      pt$defineCalculation(calculationName="Mean", summariseExpression=paste0("round(mean(",mainVar,"),2)"))
    # } else if(input$pivotTableStatInput == "quantiles"){
      pt$defineCalculation(calculationName="Min", summariseExpression=paste0("min(",mainVar,")"))
      pt$defineCalculation(calculationName="Median", summariseExpression=paste0("median(",mainVar,")"))
      pt$defineCalculation(calculationName="Max", summariseExpression=paste0("max(",mainVar,")"))
    # } else if(input$pivotTableStatInput == "sd"){
      pt$defineCalculation(calculationName="Std. Dev.", summariseExpression=paste0("round(sd(",mainVar,"),2)"))
    # }
    
    pt$evaluatePivot()
    return(pt)
  }
  
  output$multi_table <- renderPivottabler({
    withProgress(message = 'Generating table', value = 0, {
      multi_table()
    })
    
  })

  output$dl_multi_table <- downloadHandler(
    filename = function(){"multi-table.xlsx"},
    content = function(file) {
      wb <- createWorkbook()
      addWorksheet(wb, "Data")
      pt <- multi_table_raw()
      pt$writeToExcelWorksheet(wb=wb, wsName="Data",
                               topRowNumber=2, leftMostColumnNumber=2, applyStyles=TRUE)
      saveWorkbook(wb, file=file, overwrite = TRUE)
    }
  )
  

####end multivariate plotting


  # inspect raw data

  output$raw_plot_x <- renderUI({
    vars <- c("Select one" = "", colnames(ldata$lcomps))
    selectInput("raw_x",
                "Choose what to plot on x-axis",
                vars,
                multiple = FALSE,
                selected = input$select_ldata)
  })

  output$raw_plot_y <- renderUI({
    vars <- c(NA,  colnames(ldata$lcomps))
    selectInput("raw_y",
                "Choose what to plot on y-axis",
                vars,
                multiple = FALSE)
  })

  output$raw_plot_fill <- renderUI({
    vars <- c(NA, colnames(ldata$lcomps))
    selectInput("raw_fill",
                "Choose what to color by",
                vars,
                multiple = FALSE)
  })

  output$raw_plot_facet <- renderUI({
    vars <- c(NA, colnames(ldata$lcomps))
    selectInput("raw_facet",
                "Choose what to facet by",
                vars,
                multiple = FALSE)
  })

  raw_plot <- eventReactive(input$plot_raw, {
    grouped_plotter(
      ldata$lcomps,
      x = input$raw_x,
      y = input$raw_y,
      fill = input$raw_fill,
      facet = input$raw_facet,
      factorX = input$multi_factor_x,
      factorfill = input$raw_factorfill,
      scales = "free"
    )
  })

  output$raw_plot <- renderPlot(raw_plot())



  #  output$inspectplot_x <- renderUI({
  #   vars <- colnames(ldata$lcomps[map_lgl(ldata$lcomps, is.numeric)])
  #   selectInput("inspectplot_x",
  #                  "Select X Variable from raw data to plot",
  #                  vars,
  #                  selected = input$select_ldata)
  # })
  #
  # output$inspectplot_y <- renderUI({
  #   vars <- c(NA, colnames(ldata$lcomps[map_lgl(ldata$lcomps, is.numeric)]))
  #   selectInput("inspectplot_y",
  #                  "Select Y Variable from raw data to plot",
  #                  vars)
  # })



  # plot raw data data

  output$inspectplot <-
    renderPlot(inspect_plot(
      ldata$lcomps,
      x = input$inspectplot_x,
      y = input$inspectplot_y
    ))

  # assess data coverage

  output$dataCoverageVar <- renderUI({
    vars <- c("Select one" = "",colnames(dataStore$d))
    selectizeInput("dataCoverageVar",
                   "Select what to assess data coverage for: ",
                   vars)
  })
  
  output$dataCoverageGroup1 <- renderUI({
    vars <- colnames(dataStore$d)
    selectizeInput("dataCoverageGroup1",
                   "Select 1st variable to assess coverage",
                   vars)
  })
  
  output$dataCoverageGroup2 <- renderUI({
    vars <- c(NA, colnames(dataStore$d))
    
    selectizeInput("dataCoverageGroup2",
                   "Select 2nd variable to assess coverage",
                   vars)
  })
  
  
  output$dataCoverageMetric <- renderUI({
    
    req(input$dataFile)
    req(input$dataCoverageVar)
    tmp <- assess_coverage(
      dataStore$d,
      group_var1 = input$dataCoverageGroup1,
      group_var2 = input$dataCoverageGroup2,
      length_col = input$dataCoverageVar,
      n_col = NA
    ) %>%
      dplyr::rename(
        "# of Observations" = n,
        "% of Total Observations" = pn,
        "# of Non-Missing Observations" = n_present,
        "% of Observations Missing" = p_missing
      )
    
    
    vars <-
      c(colnames(tmp)[(which(colnames(tmp) == "# of Observations"):ncol(tmp))])
    
    selectInput("dataCoverageMetric",
                "Select coverage metric to plot",
                vars,
                selected = "# of Observations")
  })
  
  output$data_tally_plot <- renderPlot({
    dataTallyPlot()
  })
    
  dataTallyPlot <- function(){
    req(input$dataFile)
    req(input$dataCoverageVar)
    tmp <- assess_coverage(
      dataStore$d,
      group_var1 = input$dataCoverageGroup1,
      group_var2 = input$dataCoverageGroup2,
      length_col = input$dataCoverageVar,
      n_col = NA #input$select_tally
    ) %>%
      dplyr::rename(
        "# of Observations" = n,
        "% of Total Observations" = pn,
        "# of Non-Missing Observations" = n_present,
        "% of Observations Missing" = p_missing
      )
    
    axis_vars <-
      colnames(tmp)[1:(which(colnames(tmp) == "# of Observations") - 1)]
    
    if (length(axis_vars) == 1) {
      outplot <- tmp %>%
        ggplot(aes(.data[[axis_vars[1]]], .data[[input$dataCoverageMetric]])) +
        geom_col()
      
    } else {
      outplot <- tmp %>%
        ggplot(aes(.data[[axis_vars[1]]], .data[[axis_vars[2]]], fill = .data[[input$dataCoverageMetric]])) +
        geom_tile() +
        scale_fill_viridis_c()
      
      
    }
    
    
    outplot
    
  }
  
  cov_data <-   reactive({
    
    req(input$dataFile)
    
    assess_coverage(
      dataStore$d,
      group_var1 = input$dataCoverageGroup1,
      group_var2 = input$dataCoverageGroup2,
      length_col = input$dataCoverageVar
    ) %>%
      dplyr::mutate(pn = scales::percent(pn / 100, accuracy = 0.01)) %>%
      dplyr::mutate(p_missing = scales::percent(p_missing / 100, accuracy = 0.01)) %>%
      dplyr::rename(
        "# of Observations" = n,
        "% of Total Observations" = pn,
        "# of Non-Missing Observations" = n_present,
        "% of Observations Missing" = p_missing
      )
  })
  
  output$data_tally <- DT::renderDataTable(
    {
      
      req(input$dataFile)
      req(input$dataCoverageVar)
      cov_data()
    },
    filter = "top",
    options = list(pageLength = 5, autoWidth = TRUE)
  )
  
  output$download_coverage_table <- downloadHandler(
    filename = function() {
      "data-coverage-table.csv"
    },
    content = function(file) {
      vroom::vroom_write(cov_data(), file, ",")
    }
  )
  
  # output$download_coverage_plot <- downloadHandler(
  #   filename = function() {
  #     "data-coverage-plot.png"
  #   },
  #   content = function(file) {
  #     ggsave(file, plot = dataTallyPlot(), device = "png")
  #   }
  # )
  
  output$cov_var_1 <- renderUI({
    vars <- colnames(ldata$lcomps)
    selectizeInput("cov_var_1",
                   "Select 1st variable to assess coverage",
                   vars)
  })

  output$cov_var_2 <- renderUI({
    vars <- c(NA, colnames(ldata$lcomps))

    selectizeInput("cov_var_2",
                   "Select 2nd variable to assess coverage",
                   vars)
  })


  output$cov_var_3 <- renderUI({

    if (input$example == FALSE){
      req(input$file)
    }
    tmp <- assess_coverage(
      ldata$lcomps,
      group_var1 = input$cov_var_1,
      group_var2 = input$cov_var_2,
      length_col = input$select_ldata,
      n_col = input$select_tally
    ) %>%
      dplyr::rename(
        "# of Length Observations" = n,
        "% of Total Length Observations" = pn,
        "# of Non-Missing Length Observations" = n_present,
        "% of Length Observations Missing" = p_missing
      )


    vars <-
      c(colnames(tmp)[(which(colnames(tmp) == "# of Length Observations"):ncol(tmp))])

    selectInput("cov_var_3",
                "Select coverage metric to plot",
                vars,
                selected = "# of Length Observations")
  })


  output$data_tally_plot_old <- renderPlot({

    if (input$example == FALSE){
      req(input$file)
    }
    tmp <- assess_coverage(
      ldata$lcomps,
      group_var1 = input$cov_var_1,
      group_var2 = input$cov_var_2,
      length_col = input$select_ldata,
      n_col = input$select_tally
    ) %>%
      dplyr::rename(
        "# of Length Observations" = n,
        "% of Total Length Observations" = pn,
        "# of Non-Missing Length Observations" = n_present,
        "% of Length Observations Missing" = p_missing
      )

    axis_vars <-
      colnames(tmp)[1:(which(colnames(tmp) == "# of Length Observations") - 1)]

    if (length(axis_vars) == 1) {
      outplot <- tmp %>%
        ggplot(aes(.data[[axis_vars[1]]], .data[[input$cov_var_3]])) +
        geom_col()

    } else {
      outplot <- tmp %>%
        ggplot(aes(.data[[axis_vars[1]]], .data[[axis_vars[2]]], fill = .data[[input$cov_var_3]])) +
        geom_tile() +
        scale_fill_viridis_c()


    }


    outplot

  })

  cov_data_old <-   reactive({

    if (input$example == FALSE){
      req(input$file)
    }
    assess_coverage(
    ldata$lcomps,
    group_var1 = input$cov_var_1,
    group_var2 = input$cov_var_2,
    length_col = input$select_ldata
  ) %>%
    dplyr::mutate(pn = scales::percent(pn / 100, accuracy = 0.01)) %>%
    dplyr::mutate(p_missing = scales::percent(p_missing / 100, accuracy = 0.01)) %>%
    dplyr::rename(
      "# of Observations" = n,
      "% of Total Observations" = pn,
      "# of Non-Missing Observations" = n_present,
      "% of Observations Missing" = p_missing
    )})

  output$data_tally_old <- DT::renderDataTable(
    {

      if (input$example == FALSE){
        req(input$file)
      }
      cov_data_old()
      },
    filter = "top",
    options = list(pageLength = 5, autoWidth = TRUE)
  )

  output$download_coverage_old <- downloadHandler(
    filename = function() {
      "data-coverage.csv"
    },
    content = function(file) {
      vroom::vroom_write(cov_data_old(), file, ",")
    }
  )


  #
  #   output$data_tally_plot <- renderPlot({
  #
  #     assess_coverage(lcomps,
  #                     group_var1 = input$cov_var_1,
  #                     group_var2 = input$cov_var_2,
  #                     length_col = input$select_ldata) %>%
  #       dplyr::mutate(pn = scales::percent(pn, accuracy = 0.01)) %>%
  #       dplyr::mutate(p_missing = scales::percent(p_missing, accuracy = 0.01)) %>%
  #       dplyr::rename("# of Observations" = n,
  #                     "% of Total Observations" = pn,
  #                     "# of Non-Missing Observations" = n_present,
  #                     "% of Observations Missing" = p_missing) %>%
  #       ggplot(aes())
  #
  #
  #   })

  # if the thing is numeric, bin it. if it's discrete, convert things to "other" with less than a few observations.





  # aggregate length data

  output$select_ldata <- renderUI({

    vars <- colnames(ldata$lcomps[map_lgl(ldata$lcomps, is.numeric)])

    guess <- vars[stringr::str_detect(vars,"(length)|(longitud)")][1]

    vars <- c(NA,vars)

    selectizeInput("select_ldata",
                   "Select the column with length data in it",
                   vars,
                   selected = guess)
  })

  output$select_tally <- renderUI({
    vars <- c(NA, colnames( ldata$lcomps[map_lgl( ldata$lcomps, is.numeric)]))
    selectizeInput("select_tally",
                   "Select the column with the numbers per length bin in it",
                   vars)
  })

  output$select_groupers <- renderUI({
    vars <- c("Choose Grouping Variables" = "", colnames(ldata$lcomps)[!colnames(ldata$lcomps) %in% c(input$select_ldata, input$select_tally,"n","length_bin")])
    selectInput("select_groupers",
                "Select the variables to group by",
                vars,
                multiple = TRUE)
  })


  glcmps <- eventReactive(input$group, {
    grouped_lcomps <-
      counter(
        ldata$lcomps,
        group_var = input$select_groupers,
        length_col = input$select_ldata,
        bin_width = input$bin_width,
        n_col = input$select_tally
      )
  })

  #https://mastering-shiny.org/action-transfer.html
  # remember tomorrow that you can return output of render / observed event as thing()
  output$grouped_lcomps <-
    DT::renderDataTable(glcmps(), filter = "top", options = list(pageLength = 5, autoWidth = TRUE))

  output$download_grouper <- downloadHandler(
    filename = function() {
      "aggregated-lcomps.csv"
    },
    content = function(file) {
      vroom::vroom_write(glcmps(), file, ",")
    }
  )

  output$grouped_plot_x <- renderUI({

    if (input$example == FALSE){
      req(input$file)
    }
    vars <- c("Select one" = "", colnames(glcmps()))
    selectInput("grouped_x",
                "Choose what to plot on x-axis",
                vars,
                multiple = FALSE)
  })

  output$grouped_plot_y <- renderUI({

    if (input$example == FALSE){
      req(input$file)
    }
    vars <- c(NA, colnames(glcmps())[!colnames(glcmps()) %in% c(input$select_ldata, input$select_tally,"n","length_bin")])
    selectInput("grouped_y",
                "Choose what to plot on y-axis",
                vars,
                multiple = FALSE)
  })

  output$grouped_plot_fill <- renderUI({

    if (input$example == FALSE){
      req(input$file)
    }
    vars <- c(NA, colnames(glcmps())[!colnames(glcmps()) %in% c(input$select_ldata, input$select_tally,"n","length_bin")])
    selectInput("grouped_fill",
                "Choose what to color by",
                vars,
                multiple = FALSE)
  })

  output$grouped_plot_facet <- renderUI({

    if (input$example == FALSE){
      req(input$file)
    }
    vars <- c(NA, colnames(glcmps())[!colnames(glcmps()) %in% c(input$select_ldata, input$select_tally,"n","length_bin")])

    selectInput("grouped_facet",
                "Choose what to facet by",
                vars,
                multiple = FALSE)
  })

  group_plot <- eventReactive(input$plot_groupers, {
    grouped_plotter(
      glcmps(),
      x = input$grouped_x,
      y = input$grouped_y,
      fill = input$grouped_fill,
      facet = input$grouped_facet,
      factorfill = input$factorfill,
      scales = "free"
    )
  })

  output$group_plot <- renderPlot(group_plot())


  lcomp_plot <- eventReactive(input$plot_groupers, {
    lcomp_plotter(
      glcmps(),
      lbin = "length_bin",
      n = "n",
      fill = input$grouped_fill,
      facet = input$grouped_facet,
      factorfill = input$factorfill,
      scales = "free"
    )

  })

  output$lcomp_plot <- renderPlot(lcomp_plot())

}
