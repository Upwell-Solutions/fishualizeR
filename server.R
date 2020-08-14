

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

# filter and or subset data


# plot data

  output$inspectplot <-
    renderPlot(inspect_plot(lcomps, x = input$inspectplot_x, y = input$inspectplot_y))

  # aggregate length data

  output$select_ldata <- renderUI({
    vars <- c(NA,colnames(lcomps[map_lgl(lcomps, is.numeric)]))
    selectizeInput(
      "select_ldata",
      "Select the column with length data in it",
      vars
    )
  })

  output$select_tally <- renderUI({
    vars <- c(NA,colnames(lcomps[map_lgl(lcomps, is.numeric)]))
    selectizeInput(
      "select_tally",
      "Select the column with the numbers per length bin in it",
      vars
    )
  })

  output$select_groupers <- renderUI({
    vars <- c("Choose Grouping Variables" = "",colnames(lcomps))
    selectInput(
      "select_groupers",
      "Select the variables to group by",
      vars,
      multiple = TRUE
    )
  })

  counter <- function(data, group_var = NULL, length_col,n_col = NA, type = "obs") {

    group_var = ifelse(is.na(group_var), NULL, group_var)

    group_var <- c(group_var, length_col)

    if (type == "obs") {

      out <- data %>%
        group_by(across({{ group_var }})) %>%
        count()
    } else if (type == "counts"){

      out <- data %>%
        dplyr::group_by(dplyr::across({{ group_var }})) %>%
        dplyr::summarise(n = sum(.data[[n_col]]))

    }

    return(ungroup(out))


  }


  grouped_lcomps <-  observeEvent(input$group,{

    grouped_lcomps <- counter(lcomps, group_var = input$select_groupers,length_col = input$select_ldata)

    output$grouped_lcomps <- renderDataTable(grouped_lcomps,  options = list(pageLength = 5))

  })




}
