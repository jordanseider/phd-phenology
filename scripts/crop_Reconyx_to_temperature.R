library(magick)
library(exifr)
library(tidyverse)
library(furrr)
library(progressr)

crop_reconyx <- function(root_dir, output_dir = NULL){
  
  if(is.null(output_dir)) {
    output_dir <- paste0(root_dir, "_temps")
  }
  
  if(!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  site_name <- basename(root_dir)
  image_files <- list.files(root_dir, pattern = "(?i)\\.JPG$", recursive = TRUE, full.names = TRUE)
  
  if(length(image_files) == 0){
    stop("No JPG files found in specified directory: ", root_dir)
  }
  
  cat(sprintf("Found %d images. Extracting EXIF metadata...\n", length(image_files)))
  
  exif_data <- read_exif(image_files, tags = "CreateDate")
  
  plan(multisession, workers = 6)
  handlers(global = TRUE)
  
  message("Processing images.")
  with_progress({
    p <- progressor(steps = length(image_files))
    
    future_walk2(image_files, exif_data$CreateDate, ~ {
      
      p()
      
      date_str <- if(is.na(.y) || .y == ""){
        "UnknownDate"
      } else {
        paste0(gsub(":", "-", substr(.y, 1, 10)), "_", gsub(":", "-", substr(.y, 12, 19)))
      }
      
      orig_name <- tools::file_path_sans_ext(basename(.x))
      new_filename <- paste0(site_name, "_", date_str, "_", orig_name, ".JPG")
      output_path <- file.path(output_dir, new_filename)
      
      tryCatch({
        image_read(.x) %>%
          image_crop("145x100+3300+2096") %>%
          image_convert(colorspace = "gray") %>%
          image_negate() %>%
          image_resize("250%") %>%
          image_border("white", "30x30") %>%
          image_write(output_path)
      }, error = function(e) {
        warning("Failed on: ", basename(.x))
      })
      
    }, 
    .options = furrr_options(seed = TRUE, packages = "magick"))
  })
  
  plan(sequential)
  message("Finished! Saved to: ", output_dir)
}
