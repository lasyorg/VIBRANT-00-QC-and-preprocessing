# Function to standardize visit format
process_visit <- function(visit) {
  # Remove all spaces and trim
  visit <- trimws(gsub("\\s+", "", visit))

  # Make sure it's in the format V# by removing any non-alphanumeric chars except V
  visit <- gsub("[^V0-9]", "", visit)

  # Ensure it starts with V
  if (!startsWith(visit, "V")) {
    visit <- paste0("V", visit)
  }

  # Standardize to just two characters (V followed by a single digit)
  # Extract just the first digit after V
  visit_num <- str_extract(visit, "(?<=V)[0-9]")
  if (!is.na(visit_num)) {
    visit <- paste0("V", visit_num)
  }

  return(visit)
}


# Function to map visit to the correct code
get_visit_code <- function(visit) {
  # First standardize the visit format
  visit <- process_visit(visit)

  codes <- c(
    "V1" = "1000",
    "V2" = "1100",
    "V3" = "1200",
    "V6" = "1500",
    "V7" = "1700",
    "V9" = "2120"
  )

  # Check if the visit exists in our mapping
  if (visit %in% names(codes)) {
    return(codes[visit])
  } else {
    cat("Warning: Unknown visit type:", visit, "\n")
    return("0000") # Default code for unknown visit types
  }
}


# Create a function to extract sample info that works on a single string
extract_sample_info_single <- function(path) {
  # Try different pattern matching approaches

  # Try to match the full pattern: MGH_068_10_0004_V1_1000
  full_pattern <- str_extract(
    path,
    "MGH_[0-9]{3}_[0-9]{2}_[0-9]{4}_V[0-9]_[0-9]{4}"
  )

  # If that fails, try to match just the sample ID and visit: 068_10_0004_V1
  if (is.na(full_pattern)) {
    sample_visit <- str_extract(path, "[0-9]{3}_[0-9]{2}_[0-9]{4}_V[0-9]")

    # If that fails, try to match just the sample ID and code: 068_10_0004_1000
    if (is.na(sample_visit)) {
      sample_code <- str_extract(path, "[0-9]{3}_[0-9]{2}_[0-9]{4}_[0-9]{4}")

      # If that fails, just extract the sample ID: 068_10_0004
      if (is.na(sample_code)) {
        sample_id <- str_extract(path, "[0-9]{3}_[0-9]{2}_[0-9]{4}")
        return(sample_id)
      }
      return(sample_code)
    }
    return(sample_visit)
  }
  return(full_pattern)
}

# Vectorized version using sapply
extract_sample_info <- function(paths) {
  sapply(paths, extract_sample_info_single)
}


# Function to check if a sample exists in inventory with more flexible matching
check_sample_exists <- function(subject, visit, visit_code, inventory_data) {
  # Create different pattern variations to try matching
  patterns <- c(
    paste0("MGH_", subject, "_", visit, "_", visit_code, collapse = ""), # Full pattern with MGH prefix
    paste0(subject, "_", visit, "_", visit_code, collapse = ""), # Full pattern without MGH prefix
    paste0(subject, "_", visit, collapse = ""), # Just subject and visit
    subject # Just the subject ID
  )

  # Check if any pattern is found in folder_id, wsp_id, or fcs_id
  for (pattern in patterns) {
    if (
      any(
        str_detect(
          inventory_data$folder_id,
          fixed(pattern, ignore_case = TRUE)
        ) |
          str_detect(
            inventory_data$wsp_id,
            fixed(pattern, ignore_case = TRUE)
          ) |
          str_detect(inventory_data$fcs_id, fixed(pattern, ignore_case = TRUE))
      )
    ) {
      return(TRUE)
    }
  }

  return(FALSE)
}


############################
## SA data

# Function to parse SA folder names and extract subject ID and visit code
parse_sa_folder <- function(folder_name) {
  # Extract components from the folder name
  # Format: DATE_SUBJECTID_VISITCODE
  parts <- strsplit(folder_name, "_")[[1]]

  # Ensure we have at least 3 parts
  if (length(parts) >= 3) {
    date <- parts[1]
    subject_id <- parts[2]
    visit_code <- parts[3]

    return(list(
      date = date,
      subject_id = subject_id,
      visit_code = visit_code
    ))
  } else {
    return(NULL)
  }
}

# Function to create inventory for SA samples
create_sa_inventory <- function(sa_base_dir, out_dir) {
  # List all sample folders
  all_folders <- list.dirs(sa_base_dir, recursive = FALSE)

  # Initialize inventory data frame
  inventory <- data.frame(
    folder = character(),
    subject_id = character(),
    visit_code = character(),
    wsp_file = character(),
    fcs_file = character(),
    stringsAsFactors = FALSE
  )

  # Process each folder
  for (folder in all_folders) {
    folder_name <- basename(folder)
    folder_info <- parse_sa_folder(folder_name)

    if (!is.null(folder_info)) {
      # Find WSP files in this folder
      wsp_files <- list.files(folder, pattern = "\\.wsp$", full.names = FALSE)

      # Find FCS files in this folder
      fcs_files <- list.files(folder, pattern = "\\.fcs$", full.names = FALSE)

      # If both file types exist, create inventory entries
      if (length(wsp_files) > 0 && length(fcs_files) > 0) {
        for (wsp_file in wsp_files) {
          for (fcs_file in fcs_files) {
            inventory <-
              rbind(
                inventory,
                data.frame(
                  folder = folder,
                  subject_id = folder_info$subject_id,
                  visit_code = folder_info$visit_code,
                  wsp_file = file.path(folder, wsp_file),
                  fcs_file = file.path(folder, fcs_file),
                  stringsAsFactors = FALSE
                )
              )
          }
        }
      } else if (length(wsp_files) > 0) {
        # Only WSP files exist
        for (wsp_file in wsp_files) {
          inventory <- rbind(
            inventory,
            data.frame(
              folder = folder,
              subject_id = folder_info$subject_id,
              visit_code = folder_info$visit_code,
              wsp_file = file.path(folder, wsp_file),
              fcs_file = "", # Empty FCS file
              stringsAsFactors = FALSE
            )
          )
        }
      } else if (length(fcs_files) > 0) {
        # Only FCS files exist
        for (fcs_file in fcs_files) {
          inventory <- rbind(
            inventory,
            data.frame(
              folder = folder,
              subject_id = folder_info$subject_id,
              visit_code = folder_info$visit_code,
              wsp_file = "", # Empty WSP file
              fcs_file = file.path(folder, fcs_file),
              stringsAsFactors = FALSE
            )
          )
        }
      } else {
        # No files found, just record the folder
        inventory <- rbind(
          inventory,
          data.frame(
            folder = folder,
            subject_id = folder_info$subject_id,
            visit_code = folder_info$visit_code,
            wsp_file = "",
            fcs_file = "",
            stringsAsFactors = FALSE
          )
        )
      }
    }
  }

  inventory <- inventory |> mutate(pid = subject_id)

  # Save the inventory
  write.csv(
    inventory,
    str_c(out_dir, "sa_sample_inventory.csv"),
    row.names = FALSE
  )

  # Print summary
  cat("Found", length(all_folders), "folders\n")
  cat("Created", nrow(inventory), "inventory entries\n")
  cat("Found", length(unique(inventory$subject_id)), "unique subject IDs\n")
  cat("Inventory saved to sa_sample_inventory.csv\n")

  # Detailed summary
  cat("\nDetailed summary:\n")
  cat(
    "Folders with both WSP and FCS files:",
    sum(inventory$wsp_file != "" & inventory$fcs_file != ""),
    "\n"
  )
  cat(
    "Folders with only WSP files:",
    sum(inventory$wsp_file != "" & inventory$fcs_file == ""),
    "\n"
  )
  cat(
    "Folders with only FCS files:",
    sum(inventory$wsp_file == "" & inventory$fcs_file != ""),
    "\n"
  )
  cat(
    "Folders with no files:",
    sum(inventory$wsp_file == "" & inventory$fcs_file == ""),
    "\n"
  )

  return(inventory)
}

# Function to check for potential file matching issues
check_file_matching <- function(inventory, out_dir) {
  # Group by folder and check if there are multiple WSP or FCS files
  folder_summary <-
    inventory |>
    group_by(folder) |>
    summarize(
      wsp_count = n_distinct(wsp_file[wsp_file != ""]),
      fcs_count = n_distinct(fcs_file[fcs_file != ""]),
      .groups = "drop"
    ) |>
    dplyr::filter(wsp_count > 1 | fcs_count > 1)

  if (nrow(folder_summary) > 0) {
    cat(
      "Found",
      nrow(folder_summary),
      "folders with multiple WSP or FCS files\n"
    )
    print(folder_summary)

    # For each folder with multiple files, print the details
    cat("\nDetails of folders with multiple files:\n")
    for (i in 1:nrow(folder_summary)) {
      folder <- folder_summary$folder[i]
      cat("\nFolder:", folder, "\n")

      folder_entries <- inventory %>% filter(folder == !!folder)

      # Print WSP files
      wsp_files <- unique(folder_entries$wsp_file[
        folder_entries$wsp_file != ""
      ])
      if (length(wsp_files) > 0) {
        cat("WSP files:\n")
        for (file in wsp_files) {
          cat("  -", basename(file), "\n")
        }
      }

      # Print FCS files
      fcs_files <- unique(folder_entries$fcs_file[
        folder_entries$fcs_file != ""
      ])
      if (length(fcs_files) > 0) {
        cat("FCS files:\n")
        for (file in fcs_files) {
          cat("  -", basename(file), "\n")
        }
      }
    }

    # Save the list of folders with multiple files
    write.csv(
      folder_summary,
      str_c(out_dir, "folders_with_multiple_files.csv"),
      row.names = FALSE
    )
    cat(
      "List of folders with multiple files saved to folders_with_multiple_files.csv\n"
    )
  } else {
    cat("No folders with multiple WSP or FCS files found\n")
  }

  return(folder_summary)
}

# Function to clean up the inventory by selecting the best file matches
clean_inventory <- function(inventory, out_dir) {
  # Initialize the cleaned inventory
  cleaned_inventory <- data.frame()

  # Get unique folders
  unique_folders <- unique(inventory$folder)

  # Process each folder
  for (folder in unique_folders) {
    folder_entries <- inventory |> dplyr::filter(folder == !!folder)

    # Get unique WSP and FCS files for this folder
    wsp_files <- unique(folder_entries$wsp_file[folder_entries$wsp_file != ""])
    fcs_files <- unique(folder_entries$fcs_file[folder_entries$fcs_file != ""])

    # If there's exactly one WSP and one FCS file, use those
    if (length(wsp_files) == 1 && length(fcs_files) == 1) {
      cleaned_inventory <- rbind(
        cleaned_inventory,
        data.frame(
          folder = folder,
          subject_id = folder_entries$subject_id[1],
          visit_code = folder_entries$visit_code[1],
          wsp_file = wsp_files[1],
          fcs_file = fcs_files[1],
          stringsAsFactors = FALSE
        )
      )
    } else if (length(wsp_files) > 0 && length(fcs_files) > 0) {
      # If there are multiple WSP or FCS files, select the first one of each
      cleaned_inventory <- rbind(
        cleaned_inventory,
        data.frame(
          folder = folder,
          subject_id = folder_entries$subject_id[1],
          visit_code = folder_entries$visit_code[1],
          wsp_file = wsp_files[1],
          fcs_file = fcs_files[1],
          stringsAsFactors = FALSE
        )
      )
    } else if (length(wsp_files) > 0) {
      # If there's only WSP files, use the first one
      cleaned_inventory <- rbind(
        cleaned_inventory,
        data.frame(
          folder = folder,
          subject_id = folder_entries$subject_id[1],
          visit_code = folder_entries$visit_code[1],
          wsp_file = wsp_files[1],
          fcs_file = "",
          stringsAsFactors = FALSE
        )
      )
    } else if (length(fcs_files) > 0) {
      # If there's only FCS files, use the first one
      cleaned_inventory <- rbind(
        cleaned_inventory,
        data.frame(
          folder = folder,
          subject_id = folder_entries$subject_id[1],
          visit_code = folder_entries$visit_code[1],
          wsp_file = "",
          fcs_file = fcs_files[1],
          stringsAsFactors = FALSE
        )
      )
    } else {
      # If there are no files, just add the folder info
      cleaned_inventory <- rbind(
        cleaned_inventory,
        data.frame(
          folder = folder,
          subject_id = folder_entries$subject_id[1],
          visit_code = folder_entries$visit_code[1],
          wsp_file = "",
          fcs_file = "",
          stringsAsFactors = FALSE
        )
      )
    }
  }

  cleaned_inventory <- cleaned_inventory |>
    mutate(pid = subject_id, missing_channels = NA)

  # Save the cleaned inventory
  write.csv(
    cleaned_inventory,
    str_c(out_dir, "sa_sample_inventory_cleaned.csv"),
    row.names = FALSE
  )

  cat("Cleaned inventory contains", nrow(cleaned_inventory), "entries\n")
  cat("Cleaned inventory saved to sa_sample_inventory_cleaned.csv\n")

  return(cleaned_inventory)
}

# Main function to process SA samples
process_sa_samples <- function(sa_base_dir, out_dir) {
  cat("Processing South Africa samples from:", sa_base_dir, "\n")

  # Create initial inventory
  inventory <- create_sa_inventory(sa_base_dir, out_dir = out_dir)

  # Check for potential file matching issues
  multiple_files <- check_file_matching(inventory, out_dir = out_dir)
  cat("there\n")

  # Clean up the inventory
  cleaned_inventory <- clean_inventory(inventory, out_dir = out_dir)

  # Return the cleaned inventory
  return(cleaned_inventory)
}
