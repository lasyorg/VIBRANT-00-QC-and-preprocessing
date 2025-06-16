
plot_standard_curves <- function(t) {
  t |>
    filter(sample_type == "Standard") |>
    mutate(standard_nb = sample_id |> parse_number()) |>
    ggplot(aes(x = exp_conc |> log2(), y = FI |> asinh(), col = plate_name)) +
    geom_hline(
      data = t |> filter(sample_type == "Blank") |> as_tibble(),
      aes(yintercept = FI |> asinh(), col = plate_name), alpha = 0.5
    ) +
    geom_text(aes(label = standard_nb), alpha = 0.5) +
    facet_wrap(.feature ~ ., scales = "free", ncol = 10) +
    ggtitle(t$metadata$name) +
    ylab("asinh(Fluorescence Intensity)") +
    xlab("log2(Expected concentration)") +
    labs(caption = "Horizontal lines show the FI for Blank samples on the same plate.")
}



plot_samples_on_standard_curves <- function(se, feature){
  se_f <- se |> filter(.feature == feature) 
  standards <- se_f |> filter(sample_type == "Standard") |> as_tibble()
  samples <- se_f |> filter(sample_type == "Sample") |> as_tibble()
  blanks <- se_f |> filter(sample_type == "Blank") |> as_tibble()
  samples |> 
    ggplot() +
    aes(
      x = unadj_conc |> log10(), 
      y = FI |> asinh(),
      col = value_type, shape = value_type
    ) +
    facet_wrap(. ~ plate_name, nrow = 2) +
    geom_hline(
      data = blanks,
      aes(yintercept = FI |> asinh()), alpha = 0.5
    ) +
    geom_point(
      data = standards, aes(x = exp_conc |> log10()), 
      col = "green2", size = 2, alpha = 0.6
      ) +
    geom_text(
      data =
        standards |> select(sample_id, exp_conc) |> distinct() |> 
        mutate(sample_id = sample_id |> str_remove("S"), value_type = NA),
      aes(x = exp_conc |> log10(), y = -Inf, label = sample_id), 
      col = "green2", vjust = 0, size = 3
    ) +
    geom_point(size = 0.8, alpha = 0.5) +
    scale_color_manual("", breaks = value_types_levels(), values = value_types_colors()) +
    scale_shape_manual("", values = value_types_shapes()) +
    labs(
      title = feature,
      caption = 
        str_c("Green dots show Standards, for which the expected concentrations are shown on the x-axis.\n",
              "Horizontal likes show the FI for Blank samples on the same plate.")
    ) 
}