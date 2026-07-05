identify_samples_to_impute <- function(se_imputed, modality) {
  tmp <-
    se_imputed@colData |>
    as_tibble() |>
    select(uid, exclude, exclude_reason) |>
    left_join(
      mae@colData |> as_tibble() |> select(uid, pid, study_day, visit_code, visit_type),
      by = join_by(uid)
    ) |>
    filter(!is.na(pid))

  if (str_detect(modality, "amplicon")) {
    tmp <-
      tmp |>
      left_join(
        se_imputed |>
          assay("rel_ab") |>
          apply(2, function(x) any(is.na(x))) |>
          enframe(name = "uid", value = "missing"),
        by = join_by(uid)
      )
  } else if (str_detect(modality, "qpcr")) {
    tmp <-
      tmp |>
      left_join(
        se_imputed@colData |>
          as_tibble() |>
          select(uid, qpcr_16S) |>
          mutate(missing = is.na(qpcr_16S)),
        by = join_by(uid)
      )
  } else {
    stop('`modality` must be "amplicon" or "qpcr".')
  }

  tmp <-
    tmp |>
    full_join(
      tmp |>
        select(pid) |>
        distinct() |>
        expand_grid(
          study_day = seq(1, 35, by = 1)
        ),
      by = join_by(pid, study_day)
    ) |>
    arrange(pid, study_day) |>
    mutate(
      missing = missing |> replace_na(TRUE),
      uid = case_when(
        is.na(uid) ~ str_c(pid, "_", study_day, "_imputed"),
        TRUE ~ uid
      )
    )

  exclude_reason_pattern <-
    if (str_detect(modality, "amplicon")) {
      "amplicon|relative|^absolute"
    } else if (str_detect(modality, "qpcr")) {
      "qPCR|^absolute"
    } else {}

  tmp <-
    tmp |>
    mutate(
      imputation_candidate = case_when(
        exclude & str_detect(exclude_reason, pattern = exclude_reason_pattern) ~ TRUE,
        missing ~ TRUE,
        TRUE ~ FALSE
      )
    )

  # for each imputation candidate, check if there is a sample available the previous or following day (and no data on the same day)
  tmp <-
    tmp |>
    left_join(
      tmp |>
        filter(imputation_candidate) |>
        left_join(
          tmp |>
            filter(!missing, !exclude) |>
            select(pid, study_day, uid) |>
            dplyr::rename(study_day_2 = study_day, uid_2 = uid) |>
            distinct(),
          by = join_by(pid)
        ) |>
        filter(
          (study_day == study_day_2 + 1) |
            (study_day == study_day_2 - 1) |
            ((study_day == study_day_2) & (uid != uid_2))
        ) |>
        group_by(uid) |>
        summarize(
          has_data_on_same_day = any(study_day == study_day_2),
          has_data_before_and_after = any(study_day == study_day_2 + 1) &
            any(study_day == study_day_2 - 1),
          .groups = "drop"
        ),
      by = join_by(uid)
    )

  tmp <-
    tmp |>
    mutate(
      impute = imputation_candidate & has_data_before_and_after & !has_data_on_same_day,
      impute = impute |> replace_na(FALSE),
      impute_reason = case_when(
        impute & !is.na(visit_code) ~ "technical failure/outlier",
        impute & is.na(visit_code) ~ "missing sample",
        TRUE ~ ""
      )
    ) |>
    filter(!is.na(exclude) | impute)

  tmp <-
    tmp |>
    mutate(
      visit_code_imputed = case_when(
        is.na(visit_code) & (lag(visit_code) == "1100") & (pid == "068100061") ~ "1008_imputed",
        is.na(visit_code) & (lag(visit_code) == "1100") ~ "1006",
        is.na(visit_code) & (lag(visit_code) == "1007") ~ "1101",
        is.na(visit_code) ~ lag(visit_code) |> as.numeric() |> add(1) |> as.character(),
        TRUE ~ visit_code
      ),
      uid = case_when(
        str_detect(uid, "imputed") ~ str_c(pid, "_", visit_code_imputed),
        TRUE ~ uid
      ),
      visit_type = case_when(
        is.na(visit_type) ~ "Home (imputed sample)",
        TRUE ~ visit_type
      )
    )

  tmp |>
    select(
      uid,
      pid,
      study_day,
      visit_code_imputed,
      visit_type,
      exclude,
      exclude_reason,
      impute,
      impute_reason
    )
}


show_imputation_selection <- function(samples_to_impute, exclude_reason_pattern, xlim = NULL) {
  if (is.null(xlim)) {
    xlim <- c(
      (samples_to_impute$study_day - 1) |> min() |> floor(),
      (samples_to_impute$study_day + 1) |> max() |> ceiling()
    )
  }

  samples_to_impute |>
    mutate(
      exclude_rel_ab = str_detect(exclude_reason, exclude_reason_pattern),
      sample_category = case_when(
        !exclude_rel_ab & !impute ~ "Observed data preserved",
        (is.na(exclude_rel_ab) | !exclude_rel_ab) & impute ~ "Imputed missing sample",
        exclude_rel_ab & !impute ~ "Excluded data",
        exclude_rel_ab & impute ~ "Imputed technical failure/outlier",
        TRUE ~ "???"
      )
    ) |>
    ggplot() +
    aes(x = study_day, y = pid, color = sample_category, shape = visit_type) +
    geom_point(alpha = 0.5) +
    scale_color_manual(
      "",
      values = c(
        "Observed data preserved" = "black",
        "Imputed missing sample" = "steelblue1",
        "Excluded data" = "red",
        "Imputed technical failure/outlier" = "orange"
      )
    ) +
    scale_shape_discrete("Visit type") +
    scale_x_continuous(
      "Study day",
      breaks = seq(-10 * 7, 20 * 7, by = 7),
      minor_breaks = seq(-10 * 7, 20 * 7, by = 1),
      limits = xlim
    ) +
    scale_y_discrete("Participant ID", breaks = NULL)
}

imputation_reason_table <- function(samples_to_impute = samples_to_impute_rel_ab) {
  samples_to_impute |>
    filter(impute) |>
    dplyr::count(impute_reason) |>
    arrange(-n) |>
    gt() |>
    gt::cols_label(
      impute_reason = "Reason for imputation",
      n = "Number of samples"
    ) |>
    gt::tab_header(
      title = "Number of samples flagged for imputation by reason"
    )
}


add_imputed_samples <- function(se_imputed, samples_to_impute) {
  # add colData for the imputed samples
  coldata_to_add <-
    colData(se_imputed) |>
    as_tibble() |>
    filter(uid %in% samples_to_impute$original_uid) |>
    dplyr::rename(original_uid = uid) |>
    full_join(samples_to_impute, by = "original_uid") |>
    mutate(
      exclude = FALSE,
      exclude_reason = "",
      ampl_total_reads = ifelse(str_detect(modality, "amplicon"), NA_real_, ampl_total_reads),
      qpcr_16S = ifelse(str_detect(modality, "qPCR"), NA_real_, qpcr_16S),
      qpcr_source_mae_exp = ifelse(str_detect(modality, "qPCR"), "imputed", qpcr_source_mae_exp),
      qpcr_comment = ifelse(is.na(qpcr_comment), "", qpcr_comment),
      total_conc = NA_real_,
      imputed = TRUE,
      imputed_data = modality |> factor(levels = se_imputed$imputed_data |> levels()),
      imputed_reason = impute_reason
    ) |>
    select(-modality, -impute_reason)

  new_coldata <-
    colData(se_imputed) |>
    as_tibble() |>
    bind_rows(coldata_to_add) |>
    distinct() |>
    select(uid, starts_with("exclude"), starts_with("imputed"), everything()) |>
    mutate(original_uid = ifelse(is.na(original_uid), uid, original_uid))

  # expand assays
  new_assays <- assays(se_imputed)[c("counts", "rel_ab")]
  new_assays <- lapply(new_assays, function(current_assay) {
    # first we subset the samples that already had data but that we want to replace
    j <- which(colnames(current_assay) %in% samples_to_impute$original_uid)
    new_samples <- current_assay[, j, drop = FALSE]
    # we then join them with the new samples we want to add (those that were missing)
    new_samples <-
      bind_cols(
        new_samples |>
          set_colnames(colnames(new_samples) |> str_c("_imputed")),
        matrix(NA, nrow = nrow(new_samples), ncol = nrow(samples_to_impute) - length(j)) |>
          set_colnames(samples_to_impute$uid[
            samples_to_impute$impute_reason |> str_detect("missing")
          ])
      ) |>
      as.matrix() |>
      set_rownames(rownames(current_assay))
    cbind(current_assay, new_samples)
  })

  # create new SummarizedExperiment object
  se_imputed_expanded <- SummarizedExperiment(
    assays = new_assays,
    rowData = rowData(se_imputed),
    colData = new_coldata |> DataFrame(),
    metadata = metadata(se_imputed)
  )
}


add_imputed_se <- function(mae, experiment_name, se_imputed, samples_to_impute) {
  ### first we remove any existing experiment with the same name and warn the user that we are overwriting it
  if (experiment_name %in% names(mae)) {
    mae <- mae[,, -which(names(mae) == experiment_name)]
    message(
      "Overwriting existing experiment `",
      experiment_name,
      "` in the MultiAssayExperiment object."
    )
  }

  ### Then, we update the colData with the new samples

  # We identify the new colData rows that need to be added to the mae@colData
  # they are initialize to the uids of the samples that were imputed ("imputed" flag is TRUE)
  new_colData_rows <- data.frame(uid = colData(se_imputed)[se_imputed$imputed, "uid"])
  # but some of these uids may already exist in the mae@colData, so we need to filter them out
  new_colData_rows <- new_colData_rows |> filter(!uid %in% mae@colData$uid)
  # we fill the new colData rows with the necessary information
  coldata_visit_data_cols <-
    c(
      "PIPV",
      "visit",
      "visit_number",
      "study_week",
      "n_distinct_study_days",
      "visit_attended",
      "visit_planned",
      "crf_plates",
      "sample_type",
      "control_type",
      "assays",
      "screening_visit"
    )
  new_colData_rows <-
    new_colData_rows |>
    left_join(
      samples_to_impute |>
        select(uid, pid, visit_code = visit_code_imputed, study_day, visit_type),
      by = "uid"
    ) |>
    left_join(
      mae@colData |> as_tibble() |> select(uid, all_of(coldata_visit_data_cols)),
      by = "uid"
    ) |>
    left_join(
      mae@colData |>
        as_tibble() |>
        select(pid, site:n_study_product_doses_exposures) |>
        distinct(),
      by = "pid"
    )
  remaining_colums <- setdiff(colnames(mae@colData), colnames(new_colData_rows))
  for (col in remaining_colums) {
    new_colData_rows[[col]] <- NA
  }
  new_colData_rows <-
    new_colData_rows |>
    select(colnames(mae@colData)) |>
    DataFrame()
  rownames(new_colData_rows) <- new_colData_rows$uid

  mae@colData <-
    mae@colData |>
    rbind(new_colData_rows)

  #### We next update the sampleMap with the new samples
  mae@sampleMap <-
    mae@sampleMap |>
    rbind(
      DataFrame(
        assay = experiment_name,
        primary = se_imputed$uid,
        colname = se_imputed$uid
      )
    )
  #### Finally, we add the new SummarizedExperiment object to the MultiAssayExperiment object
  mae <- c(mae, list(se_imputed) |> set_names(experiment_name))
  mae
}


impute_samples <- function(se_imputed, modality, mae = mae) {
  modality <- match.arg(modality, c("amplicon", "qPCR"))

  # 1. identify samples to impute and filter to only include these samples and the surrounding days samples
  uids_to_impute <- se_imputed$uid[se_imputed$imputed_data |> str_detect(modality)]
  if (length(uids_to_impute) == 0) {
    message("No samples to impute for modality ", modality)
    return(se_imputed)
  } else {
    message("Imputing ", length(uids_to_impute), " samples for modality `", modality, "`")
  }
  selected_uids <-
    mae@colData |>
    as_tibble() |>
    filter()

  # 2. define the features to group by

  # 3. define the values to impute (rel_ab, counts, and ampl_total_reads for amplicon, and qpcr_16S for qPCR) and the transformations if needed

  # 4. aggregate duplicated samples (e.g., multiple swabs on the same day) by taking the mean of the duplicated samples

  # 5. impute the missing values by taking the mean of the surrounding days samples

  # 6. add the imputed samples back to the original SummarizedExperiment object

  se_tibble <-
    se_imputed |>
    as_tibble() |>
    left_join(
      colData(mae) |> as_tibble() |> select(uid, pid, study_day),
      by = join_by(uid)
    )
}
