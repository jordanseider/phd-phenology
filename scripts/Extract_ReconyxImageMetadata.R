# Extract metadata from Reconyx images
# Functions work with Reconyx Hyperfire 4K Professional cameras

library(magick) # image processing
library(tesseract) # extracts text from images
library(tidyverse)

get_datetime <- function(image){
  
  crop <- image_crop(image, "900x100+0+2095") %>% 
    image_negate() %>% 
    image_blur(1.5, 2) %>% 
    image_contrast()
  
  datetime <- image_flatten(c(image_blank(width = 900, height = 100, color = "white"), 
                              crop)) %>% 
    image_ocr(options = list(tessedit_char_whitelist = paste(
      c(as.character(0:9), "-", ":", " "), collapse = ""))) %>%
    gsub(pattern = "\n", replacement = "", x = .) %>% 
    gsub(pattern = "S", replacement = "5", x = .) %>% 
    gsub(pattern = "—", replacement = "-", x = .) %>%
    
    as.POSIXct(format = "%Y-%m-%d %T")
  
  return(datetime)
  
}

get_temperature <- function(image){
  
  crop <- image_crop(image, "145x100+3300+2095") %>% 
    image_negate() %>% 
    image_blur(1.5, 2) %>% 
    image_contrast()
  
  temp <- image_flatten(c(image_blank(width = 300, height = 100, color = "white"), 
                          crop)) %>% 
    image_ocr(options = list(tessedit_char_whitelist = paste(c(0:9, "-"), collapse = ""))) %>% 
    gsub(pattern = "\n", replacement = "", x = .) %>% 
    gsub(pattern = "S", replacement = "5", x = .) %>% 
    gsub(pattern = "—", replacement = "-", x = .) 
  
  return(as.numeric(temp))
  
}

extract_metadata <- function(image_dir, recursive = TRUE){
  
  image_files <- list.files(
    path = image_dir, 
    pattern = "\\.(jpg|JPG)$", 
    full.names = TRUE, 
    recursive = recursive # will search all subdirectories within image_dir and compile all images into one output (does not distinguish between sites, except as reported filename)
  )
  
  if (length(image_files) == 0) {
    stop("No JPEG images found in the specified directory.")
  }
  
  metadata_df <- map_dfr(image_files, 
                         
                         function(file_path){

    tryCatch({
      img <- image_read(file_path)
      dt <- get_datetime(img)
      tmp <- get_temperature(img)
      
      tibble(
        file_path = file_path,
        image_name = basename(file_path),
        date = if(!is.na(dt)) format(dt, "%Y-%m-%d") else NA_character_,
        time = if(!is.na(dt)) format(dt, "%H:%M:%S") else NA_character_,
        temperature = tmp
      )},
      
      error = function(e){
        tibble(
          file_path   = file_path,
          image_name  = basename(file_path),
          date        = NA_character_,
          time        = NA_character_,
          temperature = NA_real_
        )}
      
      )}, .progress = TRUE)
  
  return(metadata_df)
}

