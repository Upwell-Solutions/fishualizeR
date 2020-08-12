inspect_plot <- function(data,
                    x = NA,
                    y = NA,
                    fill = NA) {

  y <- ifelse(y == "NA", NA, y)

  if (!is.na(x) & !is.na(y)) {
    if (!is.na(fill)) {
      ggplot(data) +
        geom_point(aes(.data[[x]], .data[[y]], fill = as.factor(.data[[fill]])), shape = 21)

    } else {
      ggplot(data) +
        geom_point(aes(.data[[x]], .data[[y]]), shape = 21, fill = "black")

    }


    # geom_point(aes(.data[[x]], .data[[y]]), fill = as.factor(.data[[fill]]), shape = 21)
    #
  } else {
    if (!is.na(fill)) {
      ggplot(data) +
        geom_histogram(aes(.data[[x]], fill = as.factor(.data[[fill]])), position = "dodge")

    } else {
      ggplot(data) +
        geom_histogram(aes(.data[[x]]), position = "dodge")

    }

  }

}
