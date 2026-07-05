get_simulated_data_dir <- function() {
  if (str_detect(getwd(), "laurasymul")) {
    data_dir <- "/Users/laurasymul/OneDrive - UCL/Academia/Research/VIBRANT data UCLouvain/"
  } else if (str_detect(getwd(), "vermeren")) {
    data_dir <- "/Users/lvermeren/OneDrive - UCL/VIBRANT data UCLouvain/"
  } else {
    stop("You need to specify the path to the data directory in `R/data_dir.R`")
  }

  data_dir <- data_dir |> str_c("simulated data/")
  data_dir <- fs::dir_ls(data_dir) |>
    sort(decreasing = TRUE) |>
    magrittr::extract(1) |>
    str_c("/")
  data_dir
}
