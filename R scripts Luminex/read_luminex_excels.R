
read_luminex_excels <- function(dirs, pattern = "\\.xlsx$", name = "Experiment Name"){
  files <- list.files(dirs, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) stop("No excel files found in the directories")
  
  se <-
    purrr::map(files, read_luminex_excel) |>
    set_names(files) |>
    combine_luminex_se()
  se@metadata$name <- name
  se
}

read_luminex_excel <- function(file){
  library(readxl)
  
  cat(str_c("Reading data from \n\t", file |> basename(), "\n"))
  
  # checks
  # 1. we check that the file contains at least the following sheets:
  # - "FI"
  # - "FI - Bkgd"
  # - "Obs Conc"
  # - "Exp Conc"
  expected_sheets <- c("FI", "FI - Bkgd", "Obs Conc", "Exp Conc")
  if (!all(expected_sheets %in% excel_sheets(path = file)))
    stop(str_c("The excel file does not contain the expected sheets: ", expected_sheets |> str_c(collapse = ", ")))
  
  # 2. we identify the data rows and columns in the excel file
  lc <- identify_data_range(file)
  

  
  # 3. we extract the sample information
  sample_info_data_range <- str_c("A", lc$top_line - 1, ":", get_letter(lc$first_column_num - 1), lc$bottom_line)
  sample_info <- read_excel(file, sheet = "FI", range = sample_info_data_range, col_names = TRUE)
  if (!("Well" %in% colnames(sample_info))) stop("The sample information does not contain a 'Well' column")
  if (!("Type" %in% colnames(sample_info))) stop("The sample information does not contain a 'Type' column")
  if (!("Description" %in% colnames(sample_info))) warning("The sample information does not contain a 'Description' column")
  
  
  # 4. We read the sheets values
  FI <-
    read_excel(file, sheet = "FI", range = lc$excel_data_range, col_names = lc$analytes) |>
    as.data.frame() |>
    set_rownames(sample_info$Well)
  
  # for all other assays, we first need to check that the cytokines are located at the right place:
  data_range <- get_data_range_in_sheet(file, sheet = "FI - Bkgd", lc$excel_data_range, lc$analytes)
  FI_wo_background <-
    read_excel(file, sheet = "FI - Bkgd", range = data_range, col_names = lc$analytes) |>
    as.data.frame() |>
    set_rownames(sample_info$Well)
  
  data_range <- get_data_range_in_sheet(file, sheet = "Obs Conc", lc$excel_data_range, lc$analytes)
  raw_obs_conc <-
    read_excel(file, sheet = "Obs Conc", range = data_range, col_names = lc$analytes) |>
    as.data.frame() |>
    set_rownames(sample_info$Well)
  
  data_range <- get_data_range_in_sheet(file, sheet = "Exp Conc", lc$excel_data_range, lc$analytes)
  exp_conc <-
    readxl::read_excel(file, sheet = "Exp Conc", range = data_range, col_names = lc$analytes) |>
    as.data.frame() |>
    set_rownames(sample_info$Well)
  
  # If there is a dilution sheet, we load the values
  if (any(str_detect(readxl::excel_sheets(path = file), "Dil*ution"))) {
    dilution_sheet <- str_subset(readxl::excel_sheets(path = file), "Dil*ution")
    data_range <- get_data_range_in_sheet(file, sheet = dilution_sheet, lc$excel_data_range, lc$analytes)
    d <- read_excel(file, sheet = dilution_sheet, range = data_range, col_names = lc$analytes) 
    
    if (d |>  t() |> janitor::remove_constant() |> ncol() != 0)
      stop("The dilution values appear to be different for the same sample but different analytes\n")
    d <- tibble(well = sample_info$Well, dilution = d[,1] |> unlist() |> str_replace(",","\\.") |> as.numeric())
  } else {
    d <- tibble(dilution = 1)
  }
  
  se_coldata <- sample_info
  if (! any(str_detect(colnames(se_coldata), "Description")))
    se_coldata <- se_coldata |> mutate(Description = NA)
  
  se_coldata <- se_coldata |> select(Well, Type, Description, everything())
  colnames(se_coldata) <- janitor::make_clean_names(colnames(se_coldata))
  
  se_coldata <-
    se_coldata |>
    mutate(
      plate_row = well |> str_extract("[A-Z]+"),
      plate_col = well |> str_extract("[0-9]+") |> as.integer() |> factor(),
      sample_type =
        case_when(
          (type == "B") ~ "Blank",
          str_detect(type, "S[0-9]") ~ "Standard",
          str_detect(type, "C[0-9]") ~ "Manuf. control",
          str_detect(description, "control") ~ "Positive control",
          str_detect(type, "X[0-9]") ~ "Sample",
          TRUE ~ "Error"
        ) |>
        factor(
          levels =
            c("Standard", "Blank", "Manuf. control", "Positive control",
              "Sample", "Error")
        ),
      sample_id =
        case_when(
          is.na(description) ~ type,
          TRUE ~ description
        )
    ) |>
    left_join(d, by = "well") |>
    select(
      well, plate_row, plate_col,
      sample_type, type, description, sample_id, dilution,
      everything()
    ) |>
    arrange(plate_col, plate_row) |>
    distinct() |>
    as.data.frame() |>
    column_to_rownames(var = "well")
  
  # Assays
  assay_FI <- 
    FI[rownames(se_coldata),] |> 
    mutate(across(everything(), function(x) x |> str_replace(",", "\\.") |> str_replace("\\*\\*\\*","") |> as.double())) |> 
    t()
  assay_FI_wo_bkgd <- 
    FI_wo_background[rownames(se_coldata),] |> 
    mutate(across(everything(), function(x) x |> str_replace(",", "\\.") |> str_replace("\\*\\*\\*","") |> as.double())) |> 
    t()
  assay_raw_obs_conc <- raw_obs_conc[rownames(se_coldata),] |> t()
  assay_exp_conc <- 
    exp_conc[rownames(se_coldata),] |> 
    mutate(across(everything(), function(x) x |> str_replace(",", "\\.") |> as.double())) |> 
    t()
  
  se <-
    SummarizedExperiment(
      assays =
        list(
          FI = assay_FI,
          FI_wo_background = assay_FI_wo_bkgd,
          raw_obs_conc = assay_raw_obs_conc,
          exp_conc = assay_exp_conc
        ),
      colData = se_coldata,
      metadata = list(
        name = file |> basename() |> str_remove("\\.xlsx$"),
        file = file,
        excel_data_range = data_range
      )
    )
  
  se
}


identify_data_range <- function(file){
  first_column <- read_excel(file, sheet = "FI", range = "A1:A300")
  top_line <- (first_column == "Type") |> which() |> extract(2) |> add(2)
  bottom_line <- which(is.na(first_column)) 
  bottom_line <- bottom_line[bottom_line > top_line] |> extract(1)
  header_row <- 
    read_excel(
      file, sheet = "FI", range = str_c("A",top_line - 2,":BZ", top_line - 2), 
      col_names = (1:(3*26)) |> as.character()
    )
  first_column <- header_row |> is.na() |> which()
  first_column <- first_column[first_column < 10] |> max() |> add(1)
  last_column <- header_row |> is.na() |> which()
  last_column <- last_column[last_column > 10] |> extract(1) |> add(-1)
  analytes <- header_row[!is.na(header_row)]
  lc <- list(
    top_line = top_line, bottom_line = bottom_line,
    n_lines = bottom_line - top_line + 1,
    first_column_num = first_column, last_column_num = last_column,
    n_cols = last_column - first_column + 1,
    first_column = get_letter(first_column), last_column = get_letter(last_column),
    excel_data_range = 
      str_c(
        get_letter(first_column), top_line, ":", 
        get_letter(last_column), bottom_line
        ),
    analytes = analytes
  )
  lc
}


get_letter <- function(num) {
  str_c(c("",LETTERS)[((num-1) %/% 26) + 1], LETTERS[((num-1) %% 26) + 1])
}

get_data_range_in_sheet <- function(file, sheet, data_range_in_FI, cytokine_names){
  if (!check_cytokine_names(cytokine_names, file, sheet, data_range_in_FI)){
    lc <- extract_lines_cols_from_range(data_range_in_FI)
    colnames_range <- str_c("A", lc$top_line - 2, ":", "BZ", lc$top_line - 2)
    full_colnames <- read_excel(file, sheet = sheet, range = colnames_range, col_names = as.character(1:78))
    matching_cols_num <- match(cytokine_names, full_colnames)
    if (any(is.na(matching_cols_num)) | (length(matching_cols_num) != length(cytokine_names)))
      stop(str_c("Some cytokines were not found in the sheet ", sheet))
    
    matching_cols_num <- matching_cols_num |> range()
    
    first_col <- get_letter(matching_cols_num[1])
    last_col <-  get_letter(matching_cols_num[2])
    data_range <- str_c(first_col, lc$top_line, ":", last_col, lc$bottom_line)
  } else {
    data_range = data_range_in_FI
  }
  data_range
}

check_cytokine_names <- function(cytokine_names, file, sheet, data_range_in_FI){
  cytokine_names_range <- get_cytokine_names_range(data_range_in_FI)
  cytokine_names_check <- read_excel(file, sheet = sheet, range = cytokine_names_range) |> colnames()
  (cytokine_names_check == cytokine_names) |> all()
}

get_cytokine_names_range <- function(data_range){
  lc <- extract_lines_cols_from_range(data_range)
  str_c(lc$first_col, lc$top_line - 2,":", lc$last_col, lc$top_line - 2)
}


extract_lines_cols_from_range <- function(data_range){
  top_line <- data_range |> parse_number()
  bottom_line <- data_range |> str_remove(".*:") |> parse_number()
  n_lines <- bottom_line - top_line + 1
  
  first_col <- data_range |> str_extract("^[A-Z]+")
  last_col <- data_range |> str_extract(":[A-Z]+") |> str_remove(":")
  
  first_col_num <- first_col |> str_split("") |> unlist() |> match(LETTERS) |> multiply_by(26 ^ ((str_length(first_col)-1):0)) |> sum()
  last_col_num <- last_col |> str_split("") |> unlist() |> match(LETTERS) |> multiply_by(26 ^ ((str_length(last_col)-1):0)) |> sum()
  n_cols <- last_col_num - first_col_num + 1
  list(
    top_line = top_line, bottom_line = bottom_line, n_lines = n_lines,
    first_col = first_col, last_col = last_col, n_cols = n_cols,
    first_col_num = first_col_num, last_col_num = last_col_num
  )
}



combine_luminex_se <- function(se_list){
  
 
  
  
  SummarizedExperiment(
    assays =
      list(
        FI = combine_se_assay(se_list, "FI"),
        FI_wo_background = combine_se_assay(se_list, "FI_wo_background"),
        raw_obs_conc = combine_se_assay(se_list, "raw_obs_conc"),
        exp_conc = combine_se_assay(se_list, "exp_conc")
      ),
    colData = combine_se_coldata(se_list),
    metadata = list(
      n_plates = length(se_list),
      names = map(se_list, ~ .x@metadata$name) |> unlist() |> set_names(NULL),
      file = map(se_list, ~ .x@metadata$file) |> unlist() |> set_names(NULL) ,
      excel_data_range = se_list[[1]]@metadata$excel_data_range
    )
  )
  
}


combine_se_coldata <- function(se_list){
  map_dfr(
    seq_along(se_list),
    ~ se_list[[.x]] |>
      colData() |>
      as.data.frame() |>
      mutate(plate_nb = .x, plate_name = names(se_list)[.x] |> basename() |> str_remove("\\.xlsx$")) |>
      select(plate_nb, plate_name, everything()) |>
      set_rownames(str_c(str_pad(.x, width = 3, pad = "0"),"_", colnames(se_list[[.x]])))
  ) |>
    bind_rows()
}

combine_se_assay <- function(se_list, assay_name){
  # check if the feature names are the same
  feature_names <- 
    map(
      seq_along(se_list),
      ~ tibble(i = .x, feature = se_list[[.x]]@NAMES) 
    ) |> bind_rows() |> 
    mutate(tmp = TRUE) |> 
    pivot_wider(id_cols = feature, names_from = i, values_from = tmp, values_fill = FALSE) |> 
    as.data.frame() |> 
    column_to_rownames(var = "feature") 
    
  map(
    seq_along(se_list),
    ~ se_list[[.x]] |>
      assay(assay_name) |>
      set_colnames(
        str_c(str_pad(.x, width = 3, pad = "0"),"_", colnames(se_list[[.x]]))
      ) |> 
      extract(rownames(feature_names),)
  ) |> bind_cols() |>
    as.data.frame() |>
    set_rownames(rownames(feature_names))
}








