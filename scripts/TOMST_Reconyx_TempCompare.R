## TOMST data processing
library(tidyverse)
library(grid)
library(scales)
library(lubridate)
library(ggplot2)

setwd("C:/Users/jseider.stu")
#setwd("/Users/jordanseider/Library/CloudStorage")

tomst_dir <- "./Sync/Data/_FieldData/2026FieldData/TomstDataRAW"

## Sites with available data:
# "BACKHOUSE-FOOTHILLS" "MALCOLM-ALPINE"      "MALCOLM-SOUTH"       "KOMAKUK-FOOTHILLS"  
# "MOUNT-CONYBEARE"     "SPRING-RIVER-NORTH"  "ROLAND-CREEK"        "SPRING-RIVER-SOUTH" 
# "TULAGAQ-RIVER"       "STOKES-POINT"        "FIRTH-RIVER"         "LONEY-CREEK"        
# "WHALE-LOWLAND"       "BACKHOUSE-VALLEY"   
site_name <- "BACKHOUSE-VALLEY"

# Dynamically construct the file path based on site_name
site_tag <- str_remove_all(str_to_lower(site_name), "-")
cam_file_path <- paste0("./Sync/Data/Phenology/CaribouCam_Temperatures/", site_tag, "_temps.csv")

# Create list of all TOMST loggers' csv data files
data.list <- list.files(path = tomst_dir,
                        pattern = "*.csv",
                        full.names = TRUE) 

data.all <- map_dfr(data.list, function(file) {
  
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
  
  temp.data$datetime_YST = lubridate::with_tz(temp.data$datetime_UTC, tzone = "America/Whitehorse")
  
  return(temp.data)
})

data_1400 <- data.all %>%
  filter(hour(datetime_YST) == 14 & minute(datetime_YST) == 0)

cam_data <- read.csv(cam_file_path) %>%
  mutate(date = as.POSIXct(date, tz = "America/Whitehorse"))

cam_1400 <- data_1400 %>% 
  filter(site_id == site_name) %>%
  left_join(cam_data %>% select(date, temp_c), by = c("datetime_YST" = "date")) %>%
  select(site_id, datetime_YST, temp_air, temp_c) %>%
  na.omit()

plot(temp_c ~ temp_air, data = cam_1400, main = site_name, xlab = "TOMST Air Temperature", ylab = "Reconyx Temperature")

model_lm <- lm(temp_c ~ temp_air, data = cam_1400)
model_summary <- summary(model_lm)

# Extract statistics for annotation label
intercept <- coef(model_lm)[1]
slope <- coef(model_lm)[2]
r_squared <- model_summary$r.squared

# Format the equation label
eq_label <- sprintf("y = %.2fx + %.2f\nR² = %.2f", slope, intercept, r_squared)
# Calculate dynamic inset coordinates for the annotation label
annot_x <- min(cam_1400$temp_air, na.rm = TRUE) + 0.5
annot_y <- max(cam_1400$temp_c, na.rm = TRUE) - 0.5

# Create publication-ready ggplot with model overlay
ggplot(cam_1400, aes(x = temp_air, y = temp_c)) +
  geom_hline(yintercept = 0, linewidth = 0.6) +
  geom_vline(xintercept = 0, linewidth = 0.6) +
  geom_point(alpha = 0.5, color = "steelblue4", size = 1.5) +
  geom_smooth(method = "lm", color = "firebrick", fill = "firebrick", alpha = 0.2, linewidth = 1) +
  annotate("label", x = annot_x, y = annot_y, hjust = 0, vjust = 1, 
           label = eq_label, size = 4, fill = "white", linewidth = 0.2) +
  labs(
    title = paste("CaribouCam Site:", site_name),
    subtitle = "Comparing TOMST air temperature against Reconyx camera sensor data at 14:00 YST",
    x = "TOMST Air Temperature (°C)",
    y = "Reconyx Temperature (°C)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank()
  )
