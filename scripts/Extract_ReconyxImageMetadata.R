# Extract metadata from Reconyx images
# Functions work with Reconyx Hyperfire 4K Professional cameras

library(magick) # image processing
library(tesseract) # extracts text from images
library(tidyverse)

get_temperature <- function(image){
  
  crop <- image_crop(image, "300x100+3300+2095") %>% 
    image_negate() %>% 
    image_blur(1.5, 2) %>% 
    image_contrast()
  
  temp <- image_flatten(c(image_blank(width = 200, height = 100, color = "white"), 
                          crop)) %>% 
    image_ocr(options = list(tessedit_char_whitelist = paste(c(0:9, "-"), collapse = ""))) %>% 
    gsub(pattern = "\n", replacement = "", x = .) %>% 
    gsub(pattern = "S", replacement = "5", x = .) %>% 
    gsub(pattern = "—", replacement = "-", x = .)
  
  return(as.numeric(temp))
  
}

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
