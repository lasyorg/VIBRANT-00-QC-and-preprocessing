check_transformation <- function(t, transf = "asinh") {
  if (transf == "none") {
    transformed_data <- assay(t, "conc")
    xlab <- "mean(X)"
    ylab <- "variance(X)"
  } else {
    transf_fun <- get(transf)
    data <- assay(t, "conc")
    transformed_data <- data |> transf_fun()
    xlab <- str_c("mean(", transf, "(X))")
    ylab <- str_c("variance(", transf, "(X))")
  }

  g <-
    tibble(
      mean = apply(transformed_data, 1, mean, na.rm = TRUE),
      var = apply(transformed_data, 1, var, na.rm = TRUE)
    ) |>
    ggplot(aes(x = mean, y = var)) +
    geom_point() +
    geom_smooth(method = "lm", formula = 'y ~ x') +
    labs(
      title = str_c("Tranformation: ", transf),
      subtitle = t@metadata$name,
      caption = "Each dot is an analyte.\nThe x-axis shows the mean for each analyte across samples.\nThe y-axis shows the variance across samples."
    ) +
    xlab(xlab) +
    ylab(ylab)

  if (transf %in% c("none", "sqrt")) {
    g <- g + scale_x_log10() + scale_y_log10()
  }

  g
}
