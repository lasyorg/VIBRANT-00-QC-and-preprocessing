# Proven extract_gating_structure function for this type of data and packages
extract_gating_structure <- function(wsp_file, fcs_file = NULL) {
  tryCatch(
    {
      # Parse the WSP file
      ws <- CytoML::open_flowjo_xml(wsp_file)

      # Whether this file had successful gate execution
      executed_successfully <- FALSE

      # If FCS file is provided, try to execute gates
      if (!is.null(fcs_file) && file.exists(fcs_file)) {
        # Try to execute gates and get population counts
        tryCatch(
          {
            gs <- CytoML::flowjo_to_gatingset(
              ws,
              1,
              execute = TRUE,
              path = dirname(fcs_file)
            )
            executed_successfully <- TRUE
          },
          error = function(e) {
            # If execution fails for any reason, just extract the structure
            cat(
              "Gate execution failed for",
              basename(wsp_file),
              "- extracting structure only\n"
            )
            gs <<- CytoML::flowjo_to_gatingset(ws, 1, execute = FALSE)
          }
        )
      } else {
        # No FCS file provided, just get structure
        gs <- CytoML::flowjo_to_gatingset(ws, 1, execute = FALSE)
      }

      # Extract gating hierarchy
      gating_tree <- flowWorkspace::gs_get_pop_paths(gs)

      result <- list(
        file = wsp_file,
        gating_tree = data.frame(node = gating_tree, stringsAsFactors = FALSE),
        executed = executed_successfully
      )

      # Only try to get population stats if gates were executed successfully
      if (executed_successfully) {
        pop_stats <- flowWorkspace::gs_pop_get_stats(gs)
        result$population_stats <- pop_stats

        # Additionally, try to get cell counts per population
        tryCatch(
          {
            # Get counts for each population
            counts_list <- list()
            for (pop_name in gating_tree) {
              if (pop_name != "root") {
                # Skip root population
                counts <- flowWorkspace::gh_pop_get_count(gs[[1]], pop_name)
                counts_list[[pop_name]] <- counts
              }
            }
            result$cell_counts <- counts_list
          },
          error = function(e) {
            cat(
              "Failed to extract detailed cell counts for",
              basename(wsp_file),
              "\n"
            )
          }
        )
      }

      return(result)
    },
    error = function(e) {
      cat("Error processing", wsp_file, ":", conditionMessage(e), "\n")
      return(NULL)
    }
  )
}


### Pinpointed error for 3 files V9: 61, 62, 63 to the spreadsheet where it says: BV650-CD19 unavailable this day so it was not used

######### Again missing channels, but with creation of a dummy fcs file

extract_gating_structure_special <- function(
  wsp_file,
  fcs_file = NULL,
  missing_channels = NULL
) {
  tryCatch(
    {
      # Parse the WSP file
      ws <- CytoML::open_flowjo_xml(wsp_file)

      # Get available sample groups
      sample_groups_df <- CytoML::fj_ws_get_sample_groups(ws)
      cat("Sample groups data structure:", class(sample_groups_df), "\n")
      cat(
        "Sample groups data columns:",
        paste(names(sample_groups_df), collapse = ", "),
        "\n"
      )

      # Show the first few rows for debugging
      if (nrow(sample_groups_df) > 0) {
        cat("First few rows of sample groups data:\n")
        print(head(sample_groups_df))
      }

      # Get unique group names
      unique_groups <- unique(as.character(sample_groups_df$groupName))
      cat(
        "Available sample groups in",
        basename(wsp_file),
        ":",
        paste(unique_groups, collapse = ", "),
        "\n"
      )

      # Whether this file had successful gate execution
      executed_successfully <- FALSE

      # If FCS file is provided, try to execute gates
      if (!is.null(fcs_file) && file.exists(fcs_file)) {
        # Get actual FCS file names from the workspace for diagnostics
        tryCatch(
          {
            fj_samples <- CytoML::fj_ws_get_samples(ws)
            if (is.list(fj_samples)) {
              # Handle case where fj_samples is a list
              fj_samples_str <- paste(
                sapply(fj_samples, function(x) {
                  if (is.character(x)) {
                    return(x)
                  } else {
                    return(paste("(non-character data)"))
                  }
                }),
                collapse = ", "
              )
              cat("FCS files in workspace (list format):", fj_samples_str, "\n")
            } else {
              # Handle case where fj_samples is a vector
              cat(
                "FCS files in workspace:",
                paste(head(fj_samples), collapse = ", "),
                "\n"
              )
            }
          },
          error = function(e) {
            cat(
              "Could not get sample list from workspace:",
              conditionMessage(e),
              "\n"
            )
          }
        )

        # Choose the appropriate group index based on available groups
        group_idx <- 1 # Default to first group (All Samples)
        if ("Samples" %in% unique_groups) {
          group_idx <- which(unique_groups == "Samples")
          cat("Using 'Samples' group (index", group_idx, ")\n")
        } else {
          cat("Using 'All Samples' group (index", group_idx, ")\n")
        }

        # Load the original FCS file to examine channels
        tryCatch(
          {
            fcs_data <- flowCore::read.FCS(fcs_file, transformation = FALSE)
            fcs_channels <- flowCore::colnames(fcs_data)
            cat(
              "Original FCS file channels:",
              paste(head(fcs_channels), collapse = ", "),
              "...\n"
            )
            cat(
              "Total channels in original FCS file:",
              length(fcs_channels),
              "\n"
            )
          },
          error = function(e) {
            cat("Failed to read original FCS file:", conditionMessage(e), "\n")
          }
        )

        # Try multiple approaches to handle different types of issues

        # Approach 1: Basic attempt with original file
        tryCatch(
          {
            cat("Approach 1: Basic attempt with original file\n")
            gs <- CytoML::flowjo_to_gatingset(
              ws,
              group_idx,
              execute = TRUE,
              path = dirname(fcs_file)
            )
            executed_successfully <- TRUE
            cat("Approach 1 succeeded!\n")
          },
          error = function(e) {
            cat("Approach 1 error:", conditionMessage(e), "\n")

            # Approach 2: Try with a simplified workflow to handle missing channels
            tryCatch(
              {
                cat(
                  "Approach 2: Creating a modified FCS file with dummy channels\n"
                )

                # Create a new modified FCS file from scratch
                temp_dir <- tempdir()
                temp_fcs_file <- file.path(temp_dir, basename(fcs_file))

                # Copy the original FCS file
                file.copy(fcs_file, temp_fcs_file, overwrite = TRUE)

                # Load the FCS file directly with flowCore
                fcs_data <- flowCore::read.FCS(
                  temp_fcs_file,
                  transformation = FALSE
                )

                # Add dummy channels for missing channels
                if (!is.null(missing_channels)) {
                  for (channel in missing_channels) {
                    if (!(channel %in% colnames(fcs_data))) {
                      cat("Adding dummy channel:", channel, "\n")

                      # Create a matrix of zeros for the new channel
                      zeros <- matrix(0, nrow = nrow(fcs_data), ncol = 1)
                      colnames(zeros) <- channel

                      # Add the new channel to the FCS data
                      fcs_data_new <- flowCore::fr_append_cols(fcs_data, zeros)
                      fcs_data <- fcs_data_new
                    }
                  }
                }

                # Write the modified FCS file
                flowCore::write.FCS(fcs_data, temp_fcs_file)
                cat(
                  "Modified FCS file created with channels:",
                  paste(head(colnames(fcs_data)), collapse = ", "),
                  "...\n"
                )
                cat(
                  "Total channels in modified FCS file:",
                  length(colnames(fcs_data)),
                  "\n"
                )

                # Try to load with the modified file
                gs <<- CytoML::flowjo_to_gatingset(
                  ws,
                  group_idx,
                  execute = TRUE,
                  path = temp_dir,
                  fcs_files = basename(temp_fcs_file),
                  channel_mapping = FALSE,
                  ignore.channel.mismatch = TRUE
                )
                executed_successfully <<- TRUE
                cat("Approach 2 succeeded!\n")
              },
              error = function(e2) {
                cat("Approach 2 error:", conditionMessage(e2), "\n")

                # Approach 3: Skip specific channels/gates
                tryCatch(
                  {
                    cat("Approach 3: Using skip_gating approach\n")

                    # Get the list of gates to skip if they involve the missing channels
                    gates_to_skip <- character(0)

                    # Try to get a list of gates from the WSP file to identify gates to skip
                    gs_structure <- CytoML::flowjo_to_gatingset(
                      ws,
                      group_idx,
                      execute = FALSE
                    )
                    gates_info <- flowWorkspace::gs_get_pop_paths(gs_structure)

                    cat("Total gates in WSP file:", length(gates_info), "\n")

                    # Extract structure only at this point
                    gs <<- CytoML::flowjo_to_gatingset(
                      ws,
                      group_idx,
                      execute = FALSE
                    )

                    # Fallback to structure-only since we can't execute gates
                    cat("Execution not possible - using structure only\n")
                  },
                  error = function(e3) {
                    cat("Approach 3 error:", conditionMessage(e3), "\n")

                    # Final fallback: Just extract structure
                    cat("All approaches failed - extracting structure only\n")
                    gs <<- CytoML::flowjo_to_gatingset(
                      ws,
                      group_idx,
                      execute = FALSE
                    )
                  }
                )
              }
            )
          }
        )
      } else {
        # No FCS file provided, just get structure
        gs <- CytoML::flowjo_to_gatingset(ws, 1, execute = FALSE) # Use first group by default
      }

      # Extract gating hierarchy
      gating_tree <- flowWorkspace::gs_get_pop_paths(gs)

      result <- list(
        file = wsp_file,
        fcs_file = fcs_file,
        gating_tree = data.frame(node = gating_tree, stringsAsFactors = FALSE),
        executed = executed_successfully
      )

      # Only try to get population stats if gates were executed successfully
      if (executed_successfully) {
        cat("Extraction succeeded! Getting population statistics...\n")

        tryCatch(
          {
            pop_stats <- flowWorkspace::gs_pop_get_stats(gs)
            result$population_stats <- pop_stats

            # Additionally, try to get cell counts per population
            counts_list <- list()
            for (pop_name in gating_tree) {
              if (pop_name != "root") {
                # Skip root population
                counts <- flowWorkspace::gh_pop_get_count(gs[[1]], pop_name)
                counts_list[[pop_name]] <- counts
              }
            }
            result$cell_counts <- counts_list
          },
          error = function(e) {
            cat(
              "Failed to extract population statistics:",
              conditionMessage(e),
              "\n"
            )
          }
        )
      }

      return(result)
    },
    error = function(e) {
      cat("Error processing", wsp_file, ":", conditionMessage(e), "\n")
      return(NULL)
    }
  )
}


extract_gating_structure_068100058_V2 <- function(wsp_file, fcs_file = NULL) {
  tryCatch(
    {
      # Parse the WSP file
      ws <- CytoML::open_flowjo_xml(wsp_file)

      # Get available sample groups
      sample_groups_df <- CytoML::fj_ws_get_sample_groups(ws)
      cat("Sample groups data structure:", class(sample_groups_df), "\n")
      cat(
        "Sample groups data columns:",
        paste(names(sample_groups_df), collapse = ", "),
        "\n"
      )

      # Show the first few rows for debugging
      if (nrow(sample_groups_df) > 0) {
        cat("First few rows of sample groups data:\n")
        print(head(sample_groups_df))
      }

      # Get unique group names
      unique_groups <- unique(as.character(sample_groups_df$groupName))
      cat(
        "Available sample groups in",
        basename(wsp_file),
        ":",
        paste(unique_groups, collapse = ", "),
        "\n"
      )

      # Whether this file had successful gate execution
      executed_successfully <- FALSE

      # If FCS file is provided, try to execute gates
      if (!is.null(fcs_file) && file.exists(fcs_file)) {
        # Try several different approaches to execute the gates

        # Approach 1: Try using "Samples" group with default settings
        tryCatch(
          {
            cat("Approach 1: Using 'Samples' group with default settings\n")
            # gs <- CytoML::flowjo_to_gatingset(ws, sample_group = 3, execute = TRUE, path = dirname(fcs_file))
            gs <- CytoML::flowjo_to_gatingset(
              ws,
              3,
              execute = TRUE,
              path = dirname(fcs_file)
            )
            executed_successfully <- TRUE
            cat("Approach 1 succeeded!\n")
          },
          error = function(e) {
            cat("Approach 1 error:", conditionMessage(e), "\n")

            # Approach 2: Try with channel_mapping = FALSE
            tryCatch(
              {
                cat(
                  "Approach 2: Using 'Samples' group with channel_mapping = FALSE\n"
                )
                gs <<- CytoML::flowjo_to_gatingset(
                  ws,
                  sample_group = 3,
                  execute = TRUE,
                  path = dirname(fcs_file),
                  channel_mapping = FALSE
                )
                executed_successfully <<- TRUE
                cat("Approach 2 succeeded!\n")
              },
              error = function(e2) {
                cat("Approach 2 error:", conditionMessage(e2), "\n")

                # Approach 3: Try a different sample within the Samples group
                tryCatch(
                  {
                    # Get a list of sample IDs in the "Samples" group
                    samples_in_group <- sample_groups_df$sampleID[
                      sample_groups_df$groupName == "Samples"
                    ]
                    if (length(samples_in_group) > 0) {
                      cat(
                        "Approach 3: Trying individual samples in 'Samples' group\n"
                      )

                      for (sample_id in samples_in_group) {
                        cat("  Trying sample ID:", sample_id, "\n")
                        tryCatch(
                          {
                            gs <<- CytoML::flowjo_to_gatingset(
                              ws,
                              sample = sample_id,
                              execute = TRUE,
                              path = dirname(fcs_file),
                              channel_mapping = FALSE
                            )
                            executed_successfully <<- TRUE
                            cat("  Sample", sample_id, "succeeded!\n")
                            break # Exit the loop if successful
                          },
                          error = function(e3) {
                            cat(
                              "  Sample",
                              sample_id,
                              "error:",
                              conditionMessage(e3),
                              "\n"
                            )
                          }
                        )
                      }
                    }

                    if (!executed_successfully) {
                      cat("Approach 3: All individual samples failed\n")
                    }
                  },
                  error = function(e3) {
                    cat("Approach 3 error:", conditionMessage(e3), "\n")
                  }
                )

                # Approach 4: Try with additional parameters
                if (!executed_successfully) {
                  tryCatch(
                    {
                      cat("Approach 4: Using additional parameters\n")
                      gs <<- CytoML::flowjo_to_gatingset(
                        ws,
                        sample_group = 3,
                        execute = TRUE,
                        path = dirname(fcs_file),
                        channel_mapping = FALSE,
                        ignore.channel.mismatch = TRUE,
                        transform = FALSE
                      )
                      executed_successfully <<- TRUE
                      cat("Approach 4 succeeded!\n")
                    },
                    error = function(e4) {
                      cat("Approach 4 error:", conditionMessage(e4), "\n")

                      # Final fallback: Extract structure only
                      cat(
                        "All execution approaches failed - extracting structure only\n"
                      )
                      gs <<- CytoML::flowjo_to_gatingset(
                        ws,
                        sample_group = 3,
                        execute = FALSE
                      )
                    }
                  )
                }
              }
            )
          }
        )
      } else {
        # No FCS file provided, just get structure
        gs <- CytoML::flowjo_to_gatingset(ws, sample_group = 3, execute = FALSE)
      }

      # Extract gating hierarchy
      gating_tree <- flowWorkspace::gs_get_pop_paths(gs)

      result <- list(
        file = wsp_file,
        gating_tree = data.frame(node = gating_tree, stringsAsFactors = FALSE),
        executed = executed_successfully
      )

      # Only try to get population stats if gates were executed successfully
      if (executed_successfully) {
        cat("Extraction succeeded! Getting population statistics...\n")

        tryCatch(
          {
            pop_stats <- flowWorkspace::gs_pop_get_stats(gs)
            result$population_stats <- pop_stats

            # Additionally, try to get cell counts per population
            counts_list <- list()
            for (pop_name in gating_tree) {
              if (pop_name != "root") {
                # Skip root population
                counts <- flowWorkspace::gh_pop_get_count(gs[[1]], pop_name)
                counts_list[[pop_name]] <- counts
              }
            }
            result$cell_counts <- counts_list
          },
          error = function(e) {
            cat(
              "Failed to extract population statistics:",
              conditionMessage(e),
              "\n"
            )
          }
        )
      }

      return(result)
    },
    error = function(e) {
      cat("Error processing", wsp_file, ":", conditionMessage(e), "\n")
      return(NULL)
    }
  )
}


# Function to process a batch of WSP files
process_batch <- function(
  inventory_df,
  start_idx,
  end_idx,
  output_prefix,
  out_dir
) {
  # Initialize results list
  results_list <- list()

  # Process each file in the batch
  for (i in start_idx:end_idx) {
    if (i > nrow(inventory_df)) {
      break
    }

    wsp_path <- inventory_df$wsp_file[i]
    fcs_path <- inventory_df$fcs_file[i]

    # Skip if WSP file doesn't exist
    if (!file.exists(wsp_path)) {
      cat("WSP file doesn't exist:", wsp_path, "\n")
      next
    }

    # Extract sample ID and visit from the file path
    # sample_id <- str_extract(basename(wsp_path), "[0-9]{3}_[0-9]{2}_[0-9]{4}")
    # visit <- str_extract(basename(wsp_path), "V[0-9]")

    pid <- inventory_df$pid[i]
    visit_code <- inventory_df$visit_code[i]

    cat(
      sprintf(
        "Processing file %d of %d: %s (Participant: %s, Visit: %s)\n",
        i,
        end_idx,
        basename(wsp_path),
        pid,
        visit_code
      )
    )

    # Process the file
    if (!is.na(inventory_df$missing_channels[i])) {
      result <-
        extract_gating_structure_special(
          wsp_path,
          fcs_path,
          missing_channels = inventory_df$missing_channels[i] |>
            str_split(" \\| ") |>
            unlist()
        )
    } else if (
      str_detect(as.character(pid), "100058$") & (visit_code == 1100)
    ) {
      # Special case for 100058 visit 2 (1100)
      result <- extract_gating_structure_068100058_V2(wsp_path, fcs_path)
    } else {
      result <-
        extract_gating_structure(wsp_path, fcs_path)
    }

    # Add to results if successful
    if (!is.null(result)) {
      # Add sample information
      result$pid <- pid
      result$visit_code <- visit_code
      results_list[[length(results_list) + 1]] <- result
      cat("Successfully processed\n")
    } else {
      cat("Failed to process\n")
    }
  }

  # Save the batch results
  batch_filename <- sprintf(
    "%s_batch_%d_to_%d.RData",
    output_prefix,
    start_idx,
    end_idx
  )
  save(results_list, file = str_c(out_dir, batch_filename))

  cat("Saved batch results to", batch_filename, "\n")

  return(results_list)
}

# Function to convert results to a tidy data frame
results_to_dataframe <- function(results_list) {
  # Initialize an empty data frame
  all_data <- data.frame()

  # Process each result
  for (i in 1:length(results_list)) {
    result <- results_list[[i]]

    # Skip if result is NULL
    if (is.null(result)) {
      next
    }

    # Extract file info
    file_info <- data.frame(
      wsp_file = result$file,
      pid = result$pid,
      visit_code = result$visit_code,
      executed = result$executed,
      stringsAsFactors = FALSE
    )

    # Process cell counts if available
    if (result$executed && !is.null(result$cell_counts)) {
      # Convert the counts list to a data frame
      counts_df <- data.frame(
        gate = names(result$cell_counts),
        count = as.numeric(unlist(result$cell_counts)),
        stringsAsFactors = FALSE
      )

      # Calculate percentages from the population stats if available
      if (!is.null(result$population_stats)) {
        # Match percentages to gates
        counts_df$percent <- NA
        for (gate in counts_df$gate) {
          # Find this gate in population stats
          idx <- which(result$population_stats$Population == gate)
          if (length(idx) > 0) {
            counts_df$percent[
              counts_df$gate == gate
            ] <- result$population_stats$Percent[idx]
          }
        }
      }

      # Combine with file info
      gate_data <- cbind(file_info[rep(1, nrow(counts_df)), ], counts_df)

      # Add to the overall data frame
      all_data <- rbind(all_data, gate_data)
    } else {
      # Just add the file info with NA for counts
      gate_data <- cbind(
        file_info,
        data.frame(
          gate = NA_character_,
          count = NA_integer_,
          percent = NA_real_,
          stringsAsFactors = FALSE
        )
      )
      all_data <- rbind(all_data, gate_data)
    }
  }

  return(all_data)
}


# Main processing function
process_inventory <- function(
  inventory_file,
  batch_size = 10,
  test_mode = TRUE,
  out_dir = ""
) {
  # Load the inventory
  inventory <- read.csv(
    inventory_file,
    stringsAsFactors = FALSE,
    strip.white = TRUE
  )

  # Determine how many files to process
  if (test_mode) {
    max_files <- min(5, nrow(inventory))
    cat("TEST MODE: Processing only first", max_files, "files\n")
  } else {
    max_files <- nrow(inventory)
  }

  # Calculate number of batches
  n_batches <- ceiling(max_files / batch_size)

  # Process in batches
  all_results <- list()

  for (batch in 1:n_batches) {
    start_idx <- (batch - 1) * batch_size + 1
    end_idx <- min(batch * batch_size, max_files)

    cat("\n==== Processing Batch", batch, "of", n_batches, "====\n")
    cat("Files", start_idx, "to", end_idx, "of", max_files, "\n")

    # Process this batch
    batch_results <- process_batch(
      inventory,
      start_idx,
      end_idx,
      "gating_results",
      out_dir = out_dir
    )

    # Add to overall results
    all_results <- c(all_results, batch_results)

    # Save overall progress
    save(all_results, file = str_c(out_dir, "gating_results_all.RData"))
    cat("Updated overall results file: gating_results_all.RData\n")

    # If in test mode, just do one batch
    if (test_mode && batch == 1) {
      cat("Test mode completed. Stopping after first batch.\n")
      break
    }
  }

  # Convert results to a data frame
  results_df <- results_to_dataframe(all_results)

  # Save the data frame
  write.csv(
    results_df,
    str_c(out_dir, "gating_results_all.csv"),
    row.names = FALSE
  )

  # Generate a summary
  summary_df <-
    results_df |>
    dplyr::filter(!is.na(gate)) |>
    group_by(pid, visit_code) |>
    summarize(
      executed = dplyr::first(executed),
      total_gates = n_distinct(gate),
      total_cells = max(count, na.rm = TRUE),
      gates_with_counts = sum(!is.na(count)),
      .groups = "drop"
    )

  # Save the summary
  write.csv(summary_df, str_c(out_dir, "gating_summary.csv"), row.names = FALSE)

  # Return the full results data frame
  return(results_df)
}
