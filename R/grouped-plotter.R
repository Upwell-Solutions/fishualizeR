grouped_plotter <- function(data,
                            x = NA,
                            y = NA,
                            fill = NA,
                            facet = NA,
                            scales = "free_y",
                            factorX = NA,
                            factorfill = FALSE,
                            plotType = "Histogram") {
  facet <- ifelse(facet == "NA", NA, facet)
  fill <- ifelse(fill == "NA", NA, fill)
  factorX <- ifelse(factorX == "NA", NA, factorX)

  x <- ifelse(x == "NA", NA, x)
  y <- ifelse(y == "NA", NA, y)
  
  if (factorfill == TRUE & all(!is.na(fill))){

    if (!is.factor(data[[fill]])){

      data[[fill]] <- as.factor(data[[fill]])
    }


  }
  
  if (is.na(facet)) {
    data$facet <-  ""
    facet <- "facet"
  }
  
  if(plotType == "Scatterplot"){
    if (!is.na(fill)) {
      plot <- data %>%
        ggplot(aes(
          x = .data[[x]],
          y = .data[[y]],
          fill = .data[[fill]]
        )) +
        geom_point(shape = 21, size = 4) 
      
    } else {
      plot <- data %>%
        ggplot(aes(x = .data[[x]],
                   y = .data[[y]])) +
        geom_point(shape = 21, size = 4) 
      
    }
  } else {
    mainVar <- x
    
    if (!is.na(fill)) {
      plot <- data %>%
        ggplot(aes(x = .data[[mainVar]], fill = .data[[fill]]))
        
    } else  {
      plot <- data %>%
        ggplot(aes(x = .data[[mainVar]]))
    }

    if(plotType == "Histogram"){
      if (is.numeric(data[[mainVar]])){
        stat_name = "bin"
      } else {
        stat_name = "count"
      }
      plot <- plot + geom_histogram(stat = stat_name)
    } else if(plotType == "Density"){
      plot <- plot + geom_density()  
    } else { #either box or violin plot
      if(!is.na(factorX)){
        if(!is.na(fill)){
          plot <- data %>% 
            ggplot(aes(x=factor(.data[[factorX]]), y=.data[[mainVar]], fill=.data[[fill]]))
        } else {
          plot <- data %>% 
            ggplot(aes(x=factor(.data[[factorX]]), y=.data[[mainVar]])) #+ 
            # scale_x_discrete(breaks = NULL) +
            # theme(axis.title.x=element_blank())
        }
        plot <- plot + labs(x=factorX)
      } else {
        if(!is.na(fill)){
          plot <- data %>% 
            ggplot(aes(x=factor(0), y=.data[[mainVar]], fill=.data[[fill]])) 
        } else {
          plot <- data %>% 
            ggplot(aes(x=factor(0), y=.data[[mainVar]])) + 
          scale_x_discrete(breaks = NULL) +
          theme(axis.title.x=element_blank())
        }
       # plot <- plot + labs(x="")
          #scale_x_discrete(breaks = NULL) +
          #theme(axis.title.x=element_blank()) 
      }
      
      if(plotType == "Box"){
        plot <- plot + geom_boxplot() 
        
      } else if(plotType == "Violin"){
        plot <- plot + geom_violin()
        # plot <- data %>% 
        #   ggplot(aes(x=factor(0), y=.data[[x]]))  +
        #   scale_x_discrete(breaks = NULL)
      }
     
    }
  }
  
  plot <- plot +
    facet_wrap(vars(.data[[facet]]), scales = scales) +
    theme_minimal()
  
  return(plot)

  #univariate plotting  
  # if(plotType == "Histogram"){
  #   plot <- ggplot(df_NoNAs, aes(x=!!sym(input$selectUnivariateNumberCol))) + geom_histogram()
  # } else if(plotType == "Box"){
  #   plot <- ggplot(df_NoNAs, aes(x=factor(0), y=!!sym(input$selectUnivariateNumberCol))) + geom_boxplot() +
  #     scale_x_discrete(breaks = NULL) +
  #     xlab(NULL)
  # } else if(plotType == "Violin"){
  #   plot <- ggplot(df_NoNAs, aes(x=factor(0), y=!!sym(input$selectUnivariateNumberCol))) + geom_violin() +
  #     scale_x_discrete(breaks = NULL) +
  #     xlab(NULL)
  # } else if(plotType == "Density"){
  #   plot <- ggplot(df_NoNAs, aes(x=!!sym(input$selectUnivariateNumberCol))) + geom_density() 
  # }

  #x and y variables
  # if (!is.na(x) & !is.na(y)) {
  #   #color groupiing
  #   if (!is.na(fill)) {
  #     data %>%
  #       ggplot(aes(
  #         x = .data[[x]],
  #         y = .data[[y]],
  #         fill = .data[[fill]]
  #       )) +
  #       geom_point(shape = 21, size = 4) +
  #       facet_wrap(vars(.data[[facet]]), scales = scales) +
  #       theme_minimal()
  # 
  #   } else {
  #     data %>%
  #       ggplot(aes(x = .data[[x]],
  #                  y = .data[[y]])) +
  #       geom_point(shape = 21, size = 4) +
  #       facet_wrap(vars(.data[[facet]]), scales = scales) +
  #       theme_minimal()
  # 
  #   }
  # 
  # 
  # 
  # } else {
  # 
  #   if (is.numeric(data[[x]])){
  #     stat_name = "bin"
  #   } else {
  #     stat_name = "count"
  #   }
  # 
  #   if (!is.na(fill)) {
  #     data %>%
  #       ggplot(aes(x = .data[[x]], fill = .data[[fill]])) +
  #       geom_histogram(stat = stat_name) +
  #       facet_wrap(vars(.data[[facet]]), scales = scales) +
  #       theme_minimal()
  #   } else  {
  #     data %>%
  #       ggplot(aes(x = .data[[x]])) +
  #       geom_histogram(stat = stat_name) +
  #       facet_wrap(vars(.data[[facet]]), scales = scales) +
  #       theme_minimal()
  # 
  # 
  #   }
  # }
}
