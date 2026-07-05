check_se <- function(se, suffix = "assay") {
  # class check
  if (!inherits(se, "SummarizedExperiment")) {
    stop("The input object is not a SummarizedExperiment.")
  }

  # uid checks
  if (!"uid" %in% colnames(colData(se))) {
    stop("The 'uid' column is missing from the SummarizedExperiment object.")
  }
  if (!all(colnames(se) == se$uid)) {
    stop(
      "The colnames of the SummarizedExperiment do not match the 'uid' column."
    )
  }

  # check if pid, visit_code, site, location exist in colData, and if so, provide a warning and rename them
  cols_to_remove <- c("pid", "visit_code", "site", "location")
  if (any(cols_to_remove %in% colnames(colData(se)))) {
    cols_to_remove <- cols_to_remove[cols_to_remove %in% colnames(colData(se))]
    warning(
      "The following columns are present in the SummarizedExperiment colData: ",
      paste(cols_to_remove, collapse = ", "),
      "\nThey are removed from the colData and renamed with suffix '",
      suffix,
      "'."
    )
    for (col in cols_to_remove) {
      new_col_name <- str_c(col, "_", suffix)
      if (new_col_name %in% colnames(colData(se))) {
        stop(str_c(
          "The column '",
          new_col_name,
          "' already exists in the colData."
        ))
      }
      colData(se)[, new_col_name] <- colData(se)[, col]
    }
    # remove the original columns
    # we do not want to keep them in the colData
    # as they will be in the MAE colData
    # (to avoid duplicates and annoying behaviors when merging)

    colData(se) <- colData(se)[, !colnames(colData(se)) %in% cols_to_remove]
  }

  # check if the columns `sample_type` and `control_type` exist in colData.
  required_cols <- c("sample_type", "control_type")
  if (!all(required_cols %in% colnames(colData(se)))) {
    stop(
      "The 'sample_type' and 'control_type' columns are required in the SummarizedExperiment colData."
    )
  }
  # check if sample_type is valid
  valid_sample_types <-
    c(
      "Clinical sample",
      "Clinical sample (other study)",
      "Test sample",
      "Positive control",
      "Negative control",
      "Biological control",
      "Standard"
    )
  if (
    !all(
      (colData(se)$sample_type %in% valid_sample_types) |
        is.na(colData(se)$sample_type)
    )
  ) {
    stop(
      "Invalid 'sample_type' values found in the SummarizedExperiment colData. Valid types are: \n\t\t",
      paste(valid_sample_types, collapse = ",\n\t\t"),
      "\nFound types are: \n\t\t",
      paste(unique(colData(se)$sample_type), collapse = ",\n\t\t")
    )
  }
  # check if control_type is valid
  valid_control_types <-
    c(
      "Unused swab + C2",
      "Nuclease-free water",
      "Mock 1",
      "Mock 2",
      ""
    )
  if (
    !all(
      (colData(se)$control_type %in% valid_control_types) |
        is.na(colData(se)$control_type)
    )
  ) {
    stop(
      "Invalid 'control_type' values found in the SummarizedExperiment colData. Valid types are: \n\t\t",
      paste(valid_control_types, collapse = ",\n\t\t"),
      "\nFound types are: \n\t\t",
      paste(unique(colData(se)$control_type), collapse = ",\n\t\t")
    )
  }

  se
}
