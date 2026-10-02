library(magick)
library(reticulate)
library(exifr)

extract_metadata <- function(root_dir,
                             crop ="145x100+3300+2095",
                             pattern = "\\.JPG$",
                             recursive = FALSE,
                             chunk = 50) {
  
  # Must be set before paddle is imported (restart R if Python is already loaded)
  Sys.setenv(FLAGS_enable_pir_api = "0", FLAGS_use_mkldnn = "0")
  py_require(c("paddleocr", "paddlepaddle"))
  
  files <- list.files(root_dir, 
                      pattern, 
                      recursive = TRUE, 
                      full.names = TRUE, 
                      ignore.case = TRUE)
  exif <- read_exif(files, tags = "CreateDate")   # one row per file, so dates can't misalign
  
  rec <- import("paddleocr")$TextRecognition()   # load the model once
  tmp <- tempfile(fileext = ".jpg")
  on.exit(unlink(tmp))
  
  text <- character(length(files))
  pb <- txtProgressBar(max = length(files), style = 3)
  for (i in seq_along(files)) {
    image_read(files[i]) %>% image_crop(crop) %>% image_write(tmp)
    text[i] <- as.character(rec$predict(tmp)[[1]][["rec_text"]])[1]
    setTxtProgressBar(pb, i)
  }
  close(pb)
  
  dt <- as.POSIXct(exif$CreateDate, format = "%Y:%m:%d %H:%M:%S")
  
  data.frame(
    filepath     = exif$SourceFile,
    date         = as.Date(dt),
    time         = format(dt, "%H:%M:%S"),   # character, base R has no time-of-day class
    temp_text_QC = text,   # raw OCR string, kept for QC
    temp_c       = suppressWarnings(as.numeric(gsub("[^0-9-]", "", text)))
    }
}