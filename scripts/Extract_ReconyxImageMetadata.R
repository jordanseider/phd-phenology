# Extract metadata from Reconyx images
# Functions work with Reconyx Hyperfire 4K Professional cameras

library(magick) # image processing
library(tesseract) # extracts text from images
library(exifr)
library(tidyverse)

get_temperature <- function(image){
  
  image %>% 
    image_crop("145x100+3300+2096") %>%         # Crop image to only the temperature
    image_convert(colorspace = "gray") %>%      # Convert to grayscale
    image_negate() %>%                          # Invert colours (black text on white background)
    image_resize("250%") %>%                    # Enlarge image
    image_border("white", "30x30") %>%          # Add a white border
    image_ocr(options = list(
      tessedit_char_whitelist = "0123456789-",  # Only use these characters
      tessedit_pageseg_mode = 8)) %>%           # treats crop as single word
      gsub("[^0-9-]", "", .) %>%                # Replace any other characters with empty ""
    as.numeric()
  
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
  
  dt_metadata <- read_exif(image_files, tags = "CreateDate") %>% 
    transmute(
      file_path = SourceFile,
      image_name = basename(SourceFile),
      dt_parsed = ymd_hms(as.character(CreateDate)),
      date = if_else(!is.na(dt_parsed), format(dt_parsed, "%Y-%m-%d"), NA_character_),
      time = if_else(!is.na(dt_parsed), format(dt_parsed, "%H:%M:%S"), NA_character_)
    ) %>%
    select(-dt_parsed)
  
  temp_metadata <- map_dfr(image_files, function(file_path){
    tryCatch({
      img <- image_read(file_path)
      tmp <- get_temperature(img)
      
      tibble(
        file_path = file_path,
        temperature = tmp
      )
    },
    error = function(e){
      tibble(
        file_path = file_path,
        temperature = NA_real_
      )
    })
  }, .progress = TRUE)
  
  metadata_df <- left_join(dt_metadata, temp_metadata, by = "file_path")
  
  return(metadata_df)
}

data <- extract_metadata("C:/Users/jseider.stu/Desktop/trial/")

