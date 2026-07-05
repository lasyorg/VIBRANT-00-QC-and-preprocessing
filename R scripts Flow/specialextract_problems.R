####For file "MGH_068_10_0058_V2_1100/MGH_Cyto_068_10_0058_V2_1100.fcs": the following worked

extract_gating_structure_special <- function(wsp_file, fcs_file = NULL) {
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
            gs <- CytoML::flowjo_to_gatingset(
              ws,
              sample_group = 3,
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
  ###the above worked on the first file
}


# fcsfile <- file.path(mgh_base_dir,"MGH_068_10_0058_V2_1100/MGH_Cyto_068_10_0058_V2_1100.fcs")
# wspfile <- file.path(mgh_base_dir,"MGH_068_10_0058_V2_1100/068_10_0058_V2_1100.wsp")
# res_58_V2 <- extract_gating_structure_special(wspfile,fcsfile)

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

# fcsfile61 <- file.path(mgh_base_dir,"MGH_068_10_0061_V9_2120/MGH_Cyto_068_10_0061_V9_2120.fcs")
# wspfile61 <- file.path(mgh_base_dir,"MGH_068_10_0061_V9_2120/068_10_0061_V9_2120.wsp")
#
# result61 <- extract_gating_structure_special(
#   wspfile61,
#   fcsfile61,
#   missing_channels = c("BV650-A", "Comp-BV650-A")  # Just the basic missing channels
# )
#
# fcsfile62 <- file.path(mgh_base_dir,"MGH_068_10_0062_V9_2120/MGH_Cyto_068_10_0062_V9_2120.fcs")
# wspfile62 <- file.path(mgh_base_dir,"MGH_068_10_0062_V9_2120/068_10_0062_V9_2120.wsp")
#
# result62 <- extract_gating_structure_special(
#   wspfile62,
#   fcsfile62,
#   missing_channels = c("BV650-A", "Comp-BV650-A")  # Just the basic missing channels
# )
#
# fcsfile63 <- file.path(mgh_base_dir,"MGH_068_10_0063_V9_2120/MGH_Cyto_068_10_0063_V9_2120.fcs")
# wspfile63 <- file.path(mgh_base_dir,"MGH_068_10_0063_V9_2120/068_10_0063_V9_2120.wsp")
#
# result63 <- extract_gating_structure_special(
#   wspfile63,
#   fcsfile63,
#   missing_channels = c("BV650-A", "Comp-BV650-A")  # Just the basic missing channels
# )
