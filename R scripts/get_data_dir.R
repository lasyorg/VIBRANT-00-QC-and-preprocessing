
# SOURCE DATA

## Clinical data

get_clinical_data_dir <- function(){
  
  if (str_detect(getwd(), "laurasymul"))
    data_dir <- "/Users/laurasymul/OneDrive - UCL/Academia/Research/VIBRANT clinical data UCLouvain/"
  else if (str_detect(getwd(), "/laura/"))
    data_dir <- "/Users/laura/OneDrive - UCL/Fichiers de Laura Symul - VIBRANT clinical data UCLouvain/"
  else
    stop(
      str_c(
        "You need to specify the path to the data directory in `R/get_data_dir.R`.\n",
        "Note that you may not have access to the raw clinical data.\n",
        "Please contact Caroline Mitchell and Laura Symul to request access."
      )
    )
  
  data_dir <- str_c(data_dir, "Data/")
  data_dir
}

## Assay data

get_data_dir <- function() {
  get_VIBRANT_Dropbox_dir() |> str_c("90_VIBRANT_consolidated_data/")
}

# OUTPUT DATA

get_uclouvain_data_dir <- function(){
  
  if (str_detect(getwd(), "laurasymul"))
    data_dir <- "/Users/laurasymul/OneDrive - UCL/Academia/Research/VIBRANT data UCLouvain/"
  else if (str_detect(getwd(), "/laura/"))
    data_dir <- "/Users/laura/OneDrive - UCL/VIBRANT data UCLouvain/"
  else
    stop("You need to specify the path to the data directory in `R/get_data_dir.R`")
  
  data_dir <- str_c(data_dir, "actual data/")
  data_dir
}


get_output_dir <- function(){
  get_uclouvain_data_dir()
}

get_01_output_dir <- function(){
  get_output_dir() |> str_c("01 Preprocessed and QCed/")
}

get_02_output_dir <- function(){
  get_output_dir() |> str_c("02 MAEs/")
}

get_03_output_dir <- function(){
  get_output_dir() |> str_c("03 QCed MAEs/")
}

get_04_output_dir <- function(){
  get_output_dir() |> str_c("04 unblinded MAEs/")
}

get_05_output_dir <- function(){
  get_output_dir() |> str_c("05 subsetted MAEs/")
}




######## DEPRECATED FUNCTIONS



get_output_dir_deprecated <- function(data_source = "real"){
  
  if (str_detect(getwd(), "laurasymul"))
    output_dir <- "/Users/laurasymul/OneDrive - UCL/Academia/Research/VIBRANT data UCLouvain/"
  else if (str_detect(getwd(), "vermeren"))
    output_dir <- "/Users/lvermeren/OneDrive - UCL/VIBRANT data UCLouvain/"
  else
    stop("You need to specify the path to the data directory in `R/get_data_dir.R`")
  
  if (data_source == "simulated"){
    output_dir <- str_c(output_dir, "simulated data/")
    output_dir <- fs::dir_ls(output_dir) |> sort(decreasing = TRUE) |> magrittr::extract(1) |> str_c("/")
  }
  else if (data_source == "real")
    output_dir <- str_c(output_dir, "actual data/01 Preprocessed and QCed/")
  else
    stop("data_source must be either 'simulated' or 'real'")
  
  output_dir
}



