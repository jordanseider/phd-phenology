## TOMST data processing
library(tidyverse)
library(grid)
library(scales)

#tomst_dir <- "C:/Users/jseider.stu/Sync/Data/Phenology/QHI_TOMST_Data_2025"
tomst_dir <- "/Users/jordanseider/Library/CloudStorage/Sync/Data/_FieldData/2026FieldData/TomstDataRAW"

# Create list of all TOMST loggers' csv data files
data.list <- list.files(path = tomst_dir,
                        pattern = "*.csv",
                        full.names = TRUE) 

data.all <- data.frame(); for (file in data.list) {
  
  temp.data <- read.csv(file, sep = ";", header = FALSE)
  
  temp.data$source <- sub(".*_", "", sub("\\.csv$", "", file))
  
  names(temp.data) <- c("measurement",
                        "datetime_UTC",
                        "timezone",
                        "temp_below",
                        "temp_surface",
                        "temp_air",
                        "moisture",
                        #"logger",
                        "shake",
                        "errorflag",
                        "site_id")
  
  temp.data$datetime_UTC = as.POSIXct(temp.data$datetime_UTC, 
                                      format = "%Y.%m.%d %H:%M", 
                                      tz     = "UTC")
  
  temp.data$datetime_YST = as.POSIXct(temp.data$datetime_UTC, 
                                      format = "%Y.%m.%d %H:%M", 
                                      tz     = "America/Whitehorse")
  
  data.all <- rbind(data.all, temp.data)
}

data_1400 <- data.all %>%
  filter(str_detect(datetime_YST, " 14:00:00$"))

fr_cam <- read.csv("/Users/jordanseider/Library/CloudStorage/Sync/Data/Phenology/CaribouCam_Temperatures/FirthRiver_temps.csv") %>%
  mutate(date = as.POSIXct(date, tz = "America/Whitehorse"))

fr_1400 <- data_1400 %>% 
  filter(site_id == "FIRTH-RIVER") %>%
  left_join(fr_cam %>% select(date, temp_c), by = c("datetime_YST" = "date")) %>%
  select(site_id, datetime_YST, temp_air, temp_c) %>%
  na.omit()

plot(temp_c ~ temp_air, data = fr_1400)

fr_lm <- lm(temp_c ~ temp_air, data = fr_1400)

