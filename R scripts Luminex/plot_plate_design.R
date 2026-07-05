plot_plate_design <- function(t) {
  tmp <-
    t |>
    as_tibble() |>
    select(
      any_of(c("plate_nb", "plate_name")),
      plate_row,
      plate_col,
      .sample,
      sample_id,
      sample_type
    ) |>
    distinct() |>
    mutate(
      plate = case_when(
        "plate_name" %in% colnames(t@colData) ~ plate_name,
        TRUE ~ NA_character_
      )
    )

  res <-
    map(
      tmp$plate |> unique(),
      function(p, tmp) {
        tmp |>
          filter(plate == p) |>
          ggplot(aes(
            x = plate_col,
            y = plate_row |> fct_rev(),
            fill = sample_type
          )) +
          geom_tile(color = "white", linewidth = 1, alpha = 0.5) +
          geom_text(aes(label = sample_id), size = 2.5) +
          xlab("") +
          ylab("") +
          scale_fill_brewer("Sample type", type = "qual", palette = 7) +
          facet_wrap(plate ~ ., ncol = 2) +
          ggtitle(t@metadata$name) +
          theme(panel.grid = element_blank(), legend.position = "bottom")
      },
      tmp = tmp
    )
  res
}
