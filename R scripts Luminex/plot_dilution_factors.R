plot_dilution_factors <- function(t){
  t |>
    as_tibble() |> 
    select(plate_name, .sample, plate_row, plate_col, dilution) |>
    distinct() |>
    ggplot(aes(x = plate_col, y = plate_row |> fct_rev(), fill = dilution)) +
    geom_tile() +
    geom_text(aes(label = dilution |> round(2), col = -dilution), size = 3) +
    coord_fixed() +
    facet_wrap(plate_name ~ ., ncol = 5) +
    xlab("Well column") + ylab("Well row") +
    guides(col = "none") +
    ggtitle(t@metadata$name)
}
