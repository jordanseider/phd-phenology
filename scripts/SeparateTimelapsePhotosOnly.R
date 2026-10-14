library(dplyr)

data_dir <- "S:/lab/jseider/CaribouCams_2025"
out_dir  <- "S:/lab/jseider/CaribouCams_2025/_timelapse"

files <- list.files(data_dir, recursive = TRUE, full.names = TRUE)
times <- as.POSIXlt(file.mtime(files), tz = "YST")

# Select files exactly on the hour (minute 0, second 0)
timelapse <- which(times$min == 0 & times$sec == 0)

for (i in timelapse) {
  # Extract path components relative to the source folder
  parts <- strsplit(substring(files[i], nchar(data_dir) + 2), .Platform$file.sep)[[1]]
  
  site_name <- parts[1]
  file_name <- tail(parts, 1)
  
  # Places files directly into dest/SiteName/filename
  # (If multiple cameras have identical filenames, include parts[2] as a prefix to avoid overwriting)
  dst_path <- file.path(out_dir, site_name, file_name)
  
  dir.create(dirname(dst_path), recursive = TRUE, showWarnings = FALSE)
  file.copy(files[i], dst_path, overwrite = TRUE)
}