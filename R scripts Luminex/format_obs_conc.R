value_types_levels <- function(){
  c(
    "Below LLOQ", "Extrapolated (~ LLOQ)",
    "Observed concentration",  "Extrapolated",
    "Extrapolated (~ ULOQ)", "Above ULOQ",
    "Not available"
  )
}


value_types_colors <- function(){
  c(
    "steelblue1", "steelblue3",
    "black", "gray",
    "red3", "red",
    "gray80"
  )
}


value_types_shapes <- function(){
  c(
    "Below LLOQ" = 6,
    "Extrapolated (~ LLOQ)" = 8,
    "Observed concentration" = 16, 
    "Extrapolated (~ ULOQ)" = 8,
    "Above ULOQ" = 2,
    "Not available" = 1
  )
}


format_obs_conc <- function(se){
  
  median_FI <-
    se |> 
    as_tibble() |> 
    filter(sample_type == "Standard") |>
    group_by(.feature, plate_nb) |>
    summarize(med_FI = median(FI_wo_background, na.rm = TRUE), .groups = "drop") 
  
  
  
  tmp <-
    se |>
    as_tibble() |> 
    left_join(median_FI, by = join_by(.feature, plate_nb)) |> 
    mutate(
      value_type = case_when(
        is.na(raw_obs_conc) | (raw_obs_conc == "***") ~ "Not available",
        (raw_obs_conc == "OOR <") ~ "Below LLOQ",
        (raw_obs_conc == "OOR >") ~ "Above ULOQ",
        str_detect(raw_obs_conc, "^\\*") & (FI_wo_background <= med_FI) ~ "Extrapolated (~ LLOQ)",
        str_detect(raw_obs_conc, "^\\*") & (FI_wo_background > med_FI) ~ "Extrapolated (~ ULOQ)",
        TRUE ~ "Observed concentration"
      ) |>
        factor(levels = value_types_levels()),
      tmp_obs_conc =
        raw_obs_conc |>
        str_replace(",","\\.") |> 
        str_remove_all("\\*") |>
        str_replace_all("OOR [<>]","") |>
        as.numeric(),
      raw_obs_conc_num = 
        case_when(
          (value_type == "Below LLOQ") ~ 0,
          (value_type == "Above ULOQ") ~ Inf,
          TRUE ~ tmp_obs_conc
        )
    )
  
  se <- se |> mutate(raw_obs_conc_num = tmp$raw_obs_conc_num)
  se <- se |> mutate(value_type = tmp$value_type)
  
  cat(
    str_c(
      se@metadata$name,"\n\t",
      "Added 2 assays:\n\t\t",
      "-`raw_obs_conc_num` with the raw observed concentrations in numerical (rather than character) format\n\t\t",
      "-`value_type` with the type of value (observed, extrapolated, below LLOQ, above ULOQ)\n"
    )
  )
  se
  
}
