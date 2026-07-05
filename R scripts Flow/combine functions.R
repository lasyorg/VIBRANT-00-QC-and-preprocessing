standardize_gates <- function(data_list, site_name, gate_mapping) {
  # Initialize an empty data frame to store the combined results
  combined_data <- data.frame()

  # Process each element in the list
  for (i in 1:length(data_list)) {
    item <- data_list[[i]]

    # Extract the population_stats data
    df <- as.data.frame(item$population_stats)

    if (nrow(df) == 0) {
      next # Skip if there are no population stats
    }

    # Rename the 'pop' column to 'gate' to match your function's expectations
    names(df)[names(df) == "pop"] <- "gate"

    # Add pid and visit_code from the list item
    df$pid <- item$pid
    df$visit_code <- item$visit_code

    # Add to the combined data
    combined_data <- rbind(combined_data, df)
  }

  # Now proceed with the original function logic on the data frame
  data <- combined_data

  # Filter mapping for this site and remove rows without standardized names
  site_mapping <-
    gate_mapping |>
    dplyr::filter(site == site_name, !is.na(standardized_name))

  # Add site column
  data$site <- site_name

  # Ensure consistent data types
  data$pid <- as.character(data$pid) # as.character(data$sample_id)
  data$visit_code <- as.character(data$visit_code)

  # Initialize columns for standardized names and cell types
  data$standardized_name <- NA
  data$cell_type <- NA

  # Match gates with the mapping
  for (i in 1:nrow(site_mapping)) {
    # Find rows where the gate matches
    matches <- data$gate == site_mapping$original_gate[i]

    # Update standardized name and cell type for matched rows
    data$standardized_name[matches] <- site_mapping$standardized_name[i]
    data$cell_type[matches] <- site_mapping$cell_type[i]
  }

  # Filter to keep only rows with a standardized name and valid visit
  data <-
    data |>
    dplyr::filter(!is.na(standardized_name)) |>
    dplyr::filter(!is.na(visit_code) & visit_code != "")

  # Initialize percent column if it doesn't exist
  if (!"percent" %in% names(data)) {
    data$percent <- NA
  }

  # Calculate percentages where missing
  data <-
    data |>
    group_by(pid, visit_code) |>
    mutate(
      parent_gate = ifelse(gate == "/", NA, dirname(gate)),
      parent_count = NA
    ) |>
    ungroup()

  # Fill in parent counts
  for (i in 1:nrow(data)) {
    if (!is.na(data$parent_gate[i])) {
      # Find the parent gate's count
      parent_row <- which(
        data$gate == data$parent_gate[i] &
          data$pid == data$pid[i] &
          data$visit_code == data$visit_code[i]
      )

      if (length(parent_row) > 0) {
        data$parent_count[i] <- data$count[parent_row[1]]
      }
    }
  }

  # Calculate percentage where it's missing
  data <-
    data |>
    mutate(
      percentage = ifelse(
        is.na(percent) & !is.na(parent_count) & parent_count > 0,
        count / parent_count * 100,
        percent
      )
    )

  return(data)
}


aggregate_flow_data <- function(mgh_file, sa_file, gate_mapping, output_dir) {
  # Load the data
  load(mgh_file, verbose = TRUE)
  mgh_all_results <- all_results

  load(sa_file, verbose = TRUE)
  sa_all_results <- all_results

  # # Check and print column types to help with debugging
  # cat("MGH data column types:\n")
  # print(sapply(mgh_all_results, class))
  #
  # cat("SA data column types:\n")
  # print(sapply(sa_all_results, class))

  cat("Processing MGH data...\n")
  mgh_standardized <- standardize_gates(mgh_all_results, "MGH", gate_mapping)

  cat("Processing SA data...\n")
  sa_standardized <- standardize_gates(sa_all_results, "CAP", gate_mapping)

  # Ensure consistent data types across all columns before combining
  common_cols <- intersect(names(mgh_standardized), names(sa_standardized))

  for (col in common_cols) {
    # Convert to the most flexible type (usually character)
    if (class(mgh_standardized[[col]]) != class(sa_standardized[[col]])) {
      cat("Converting column", col, "to consistent type\n")

      # Choose conversion based on column
      if (
        col %in%
          c(
            "pid",
            "visit_code",
            "gate",
            "standardized_name",
            "cell_type",
            "parent_gate"
          )
      ) {
        # Convert to character
        mgh_standardized[[col]] <- as.character(mgh_standardized[[col]])
        sa_standardized[[col]] <- as.character(sa_standardized[[col]])
      } else if (col %in% c("count", "percent", "percentage", "parent_count")) {
        # Convert to numeric
        mgh_standardized[[col]] <- as.numeric(mgh_standardized[[col]])
        sa_standardized[[col]] <- as.numeric(sa_standardized[[col]])
      } else if (col == "executed") {
        # Convert to logical
        mgh_standardized[[col]] <- as.logical(mgh_standardized[[col]])
        sa_standardized[[col]] <- as.logical(sa_standardized[[col]])
      }
    }
  }

  # Combine the datasets
  all_data <- bind_rows(mgh_standardized, sa_standardized)

  # Create a summary table
  summary_table <-
    all_data |>
    group_by(site, standardized_name, cell_type) |>
    summarize(
      mean_count = mean(count, na.rm = TRUE),
      median_count = median(count, na.rm = TRUE),
      sd_count = sd(count, na.rm = TRUE),
      mean_percentage = mean(percentage, na.rm = TRUE),
      median_percentage = median(percentage, na.rm = TRUE),
      sd_percentage = sd(percentage, na.rm = TRUE),
      n_samples = n(),
      .groups = "drop"
    )

  # Create a per-sample summary
  sample_summary <-
    all_data |>
    group_by(site, pid, visit_code, standardized_name, cell_type) %>%
    summarize(
      count = dplyr::first(count),
      percentage = dplyr::first(percentage),
      .groups = "drop"
    )

  # Write results to CSV
  write.csv(
    all_data,
    file.path(output_dir, "aggregated_cell_counts_all.csv"),
    row.names = FALSE
  )
  write.csv(
    summary_table,
    file.path(output_dir, "cell_population_summary.csv"),
    row.names = FALSE
  )
  write.csv(
    sample_summary,
    file.path(output_dir, "sample_cell_counts.csv"),
    row.names = FALSE
  )

  # Save as RData for future use
  save(
    all_data,
    summary_table,
    sample_summary,
    file = file.path(output_dir, "combined_flow_results.RData")
  )

  cat("Results saved to:", output_dir, "\n")

  return(list(
    all_data = all_data,
    summary = summary_table,
    sample_summary = sample_summary
  ))
}


create_SE_from_harmonized_flow_data <- function(flow_data) {
  # we create a unique identifier for each participant x visit
  flow_data <-
    flow_data |>
    mutate(
      pid = pid |> as.character() |> str_replace("^68", "068"),
      uid = str_c(pid, "_", visit_code)
    )

  ## colData
  # we first check that there is only one `sample` per `uid`
  tmp <-
    flow_data |>
    select(uid, sample) |>
    distinct() |>
    group_by(uid) |>
    mutate(n = n()) |>
    ungroup() |>
    arrange(-n)

  if (any(tmp$n > 1)) {
    warning("There are multiple samples for the same uid.")
    tmp |> dplyr::filter(n > 1) |> print()
    cat("Taking the first sample for these uid(s).\n")
  }

  se_coldata <-
    flow_data |>
    select(uid, sample, machine) |>
    distinct() |>
    group_by(uid) |>
    slice_head(n = 1) |>
    ungroup() |>
    mutate(rownames = uid) |>
    as.data.frame() |>
    column_to_rownames("rownames")

  flow_data <-
    se_coldata |>
    dplyr::left_join(flow_data, by = join_by(uid, sample))

  # rowData
  se_rowdata <-
    flow_data |>
    select(cell_type, parent_gate) |>
    mutate(
      parent_cell_type = parent_gate |>
        str_remove(".*/") |>
        str_replace("HLA-DR", "HLA") |>
        str_replace("Comp-LiveDead Aqua-A, SSC-A subset", "Live cells") |>
        str_replace("Singlets", "Single cells") |>
        str_replace("Single Cells", "Single cells")
    ) |>
    select(cell_type, parent_cell_type) |>
    distinct() |>
    mutate(
      parent_cell_type = parent_cell_type |>
        factor(
          levels = c(
            "Single cells",
            "Live cells",
            "Non T cells",
            "HLA, SSC-A subset",
            "APC",
            "T cells",
            "CD4 T cells",
            "CD8 T cells"
          )
        ),
      cell_type_label = cell_type |>
        str_replace("B_", "B ") |>
        str_replace("DblePos", "Double positive ") |>
        str_replace("Live", "Live cells")
    ) |>
    arrange(parent_cell_type, cell_type) |>
    mutate(cell_type_label = cell_type_label |> fct_inorder()) |>
    mutate(rownames = cell_type) |>
    as.data.frame() |>
    column_to_rownames("rownames")

  features <- se_rowdata$cell_type |> unique()

  ## Assays
  count_assay <-
    flow_data |>
    select(uid, cell_type, count) |>
    pivot_wider(names_from = uid, values_from = count) |>
    column_to_rownames("cell_type") |>
    as.matrix()
  count_assay <- count_assay[features, se_coldata$uid]

  percentage_assay <-
    flow_data |>
    select(uid, cell_type, percentage) |>
    pivot_wider(names_from = uid, values_from = percentage) |>
    column_to_rownames("cell_type") |>
    as.matrix()
  percentage_assay <- percentage_assay[features, se_coldata$uid]

  # Create the SummarizedExperiment object
  se <- SummarizedExperiment(
    assays = SimpleList(count = count_assay, percentage = percentage_assay),
    colData = se_coldata,
    rowData = se_rowdata
  )

  se
}
