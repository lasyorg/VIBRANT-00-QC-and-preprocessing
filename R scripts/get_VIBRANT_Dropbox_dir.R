get_VIBRANT_Dropbox_dir <- function(){
  
  if(str_detect(getwd(), "laurasymul")){
    dir <- "/Users/laurasymul/Dropbox/Academia/Projects/VIBRANT Study Files/"
  } 
  else if (str_detect(getwd(), "/laura/"))
    dir <- "/Users/laura/Dropbox/VIBRANT Study Files/"
  else {
    stop(
      "
      Hello new script runner!\t
      We do not know you or your machine yet, so we do not know where the VIBRANT Dropbox is on your computer. \t
      You can easily fix that by opening the `R scripts/VIBRANT_Dropbox_dir/R` file and modify the `VIBRANT_Dropbox_dir` function so that it returns the right path for the Dropbox directory.
      ")
  }
  dir
}
