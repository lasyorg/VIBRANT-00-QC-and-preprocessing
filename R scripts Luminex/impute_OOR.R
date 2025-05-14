

impute_OOR <- function(se, LLOQ_factor = 0.5, ULOQ_factor = 1.1){
  
  # We first compute the lowest and highest (extrapolated) values from the samples concentrations
  extremes <- 
    se |> 
    as_tibble() |> 
    filter(
      is.finite(unadj_conc),  (unadj_conc > 0), !is.na(unadj_conc),
      !(value_type %in% c("Below LLOQ", "Above ULOQ", "Not available"))
      ) |> 
    select(plate_nb, .feature, .sample, sample_id, dilution_luminex, unadj_conc) |>
    group_by(plate_nb, .feature) |>
    summarize(
      min_unadj_conc = min(unadj_conc),
      max_unadj_conc = max(unadj_conc),
      .groups = "drop"
    )
  
  # We check that we have values for all analytes and all plates
  if (nrow(extremes) != (length(se@NAMES) * length(unique(se$plate_nb)))) 
    warning("ULOQ and LLOQ could not be computed for all analytes\n")
  
  tmp <-
    se |>
    as_tibble() |>
    left_join(extremes, by = join_by(.feature, plate_nb)) |>
    mutate(
      unadj_conc_imp =
        case_when(
          is.na(unadj_conc) ~ NA_real_,
          (raw_obs_conc == "OOR <") ~ min_unadj_conc * LLOQ_factor,
          (raw_obs_conc == "OOR >") ~ max_unadj_conc * ULOQ_factor,
          TRUE ~ unadj_conc
        )
    )
  
  # tmp |> select(.feature, .sample, sample_id, sample_type, value_type, dilution, raw_obs_conc,raw_obs_conc_num, obs_conc, min_obs, max_obs) |> filter(sample_type == "Sample")
  
  se <- se |> mutate(unadj_conc_imp = tmp$unadj_conc_imp)
  
  # We now add the extremes to the se.
  # We do not add it to the rowData since we have one value per plate and analyte
  # We add it to the metadata instead
  se@metadata$extremes <- extremes

  cat(
    str_c(
      se@metadata$name,"\n\t",
      "Added 1 assays:\n\t\t",
      "-`unadj_conc_imp` with the imputed unadjusted concentrations\n\t\t",
      "and 1 table to @metadata:\n\t\t",
      "-`extremes` with the minimum and maximum observed values for each analyte and plate\n"
    )
  )
  se
}

impute_OOR_deprecated <- function(se, max_log2_fc_exp_vs_obs = 1, LLOQ_factor = 0.5, ULOQ_factor = 1.1){

  # We first compute the lowest and highest extrapolated values from the samples concentrations

  standards <- 
    se |> 
    as_tibble() |> 
    filter(sample_type == "Standard") |> 
    select(plate_nb, .feature, .sample, sample_id, FI, FI_wo_background, raw_obs_conc, exp_conc) |>
    mutate(
      obs_conc = 
        raw_obs_conc |> 
        str_replace(",","\\.") |> 
        str_remove("\\*") |> 
        str_replace_all("OOR [<>]","") |>
        as.numeric(), # 
      obs_over_exp = obs_conc / exp_conc,
      obs_match_exp = !is.na(obs_conc) & (abs(log2(obs_over_exp)) < max_log2_fc_exp_vs_obs) 
    ) |> 
    group_by(plate_nb, .feature) |>
    mutate(
      LLOQ = min(exp_conc[obs_match_exp]),
      ULOQ = max(exp_conc[obs_match_exp]),
      standard_category = 
        case_when(
          (exp_conc == LLOQ) ~ "LLOQ",
          (exp_conc == ULOQ) ~ "ULOQ",
          obs_match_exp ~ "Observed match expected",
          TRUE ~ "Observed does not match expected"
        )
    )
  
  ranges <-
    standards |> 
    select(.feature, plate_nb, LLOQ, ULOQ) |>
    distinct() |> 
    mutate(mid = (LLOQ + ULOQ) / 2) 

  # We check that we have values for all analytes and all plates
  if (nrow(ranges) != (length(se@NAMES) * length(unique(se$plate_nb)))) 
    warning("ULOQ and LLOQ could not be computed for all analytes\n")

  tmp <-
    se |>
    as_tibble() |>
    left_join(ranges, by = join_by(.feature, plate_nb)) |>
    mutate(
      tmp_obs_conc =
        raw_obs_conc |>
        str_replace(",","\\.") |> 
        str_remove_all("\\*") |>
        str_replace_all("OOR [<>]","") |>
        as.numeric(),
      obs_conc =
        case_when(
          is.na(raw_obs_conc) ~ NA_real_,
          raw_obs_conc == "***" ~ NA_real_,
          (raw_obs_conc == "OOR <") ~ LLOQ * LLOQ_factor,
          (raw_obs_conc == "OOR >") ~ ULOQ * ULOQ_factor,
          str_detect(raw_obs_conc, "^\\*[0-9]") ~ tmp_obs_conc,
          TRUE ~ tmp_obs_conc
        ),
      value_type = case_when(
        is.na(raw_obs_conc) | (raw_obs_conc == "***") ~ "Not available",
        (raw_obs_conc == "OOR <") ~ str_c("Below LLOQ (→ LLOQ x ", LLOQ_factor, ")"),
        (raw_obs_conc == "OOR >") ~ str_c("Above ULOQ (→ ULOQ x ", ULOQ_factor, ")"),
        str_detect(raw_obs_conc, "^\\*") & (obs_conc <= mid) ~ "Extrapolated (~ LLOQ)",
        str_detect(raw_obs_conc, "^\\*") & (obs_conc > mid) ~ "Extrapolated (~ ULOQ)",
        TRUE ~ "Observed concentration"
      ) |>
        factor(levels = value_types_levels())
    )

  se <- se |> mutate(conc = tmp$obs_conc)
  se <- se |> mutate(value_type = tmp$value_type)
  # the following does not work :(
  # assay(se, "value_type_fct") <-
  #   assay(se, "value_type") |>
  #   as.data.frame() |>
  #   mutate(across(everything(), function(x) factor(x, levels = value_types_levels())))
  
  # We now add the ranges data to the se.
  # We do not add it to the rowData since we have one value per plate and analyte
  # rowData(se) <- ranges |> column_to_rownames(".feature")
  # We add it to the metadata instead
  se@metadata$ranges <- ranges
  se@metadata$standards <- standards

  cat(
    str_c(
      se@metadata$name,"\n\t",
      "Added 2 assays:\n\t\t",
      "-`conc` with the imputed concentrations\n\t\t",
      "-`value_type` with the type of value (observed, extrapolated, below LLOQ, above ULOQ)\n\t",
      "and 2 elements to @metadata:\n\t\t",
      "-`ranges` with ULOQ and LLOQ values for each analyte and plate\n\t\t",
      "-`standards` with additional information on each standard samples (useful for diagnostic plots)\n"
    )
  )
  se
}
