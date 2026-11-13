setwd("C:/Users/jseider.stu/Sync")

library(terra)
library(ggplot2)
library(dplyr)

# 1. Define your paths
raster_paths <- c(
  "C:/Users/jseider.stu/Sync/Data/ArcticDEM/IvvavikNP/ArcticDEM_IvvavikNP_10m.tif",
  "C:/Users/jseider.stu/Sync/Data/ArcticDEM/IvvavikNP/ArcticDEM_IvvavikNP_twi_10m.tif",
  "C:/Users/jseider.stu/Sync/Data/ArcticDEM/IvvavikNP/distance_to_coast_m.tif",
  "C:/Users/jseider.stu/Sync/Data/ArcticDEM/IvvavikNP/geomorphons_0_40.tif",
  "C:/Users/jseider.stu/Sync/Data/ABoVE_Data/AnnualPFT_ABoVE_Macander/IvvavikNP_2015_TopCover.tif",
  "C:/Users/jseider.stu/Sync/Data/Climate/Ivvavik_dayl_hrs_150.tif",
  "C:/Users/jseider.stu/Sync/Data/Climate/Ivvavik_gdd_doy150.tif",
  "C:/Users/jseider.stu/Sync/Data/Climate/Ivvavik_monthly_mean_tmax.tif",
  "C:/Users/jseider.stu/Sync/Data/Climate/meanSWE_doy100_120.tif",
  "C:/Users/jseider.stu/Sync/Data/Climate/meanSWE_doy121_150.tif",
  "C:/Users/jseider.stu/Sync/Data/Phenology/MS-LSP/eviarea_ivvavik.tif",
  "C:/Users/jseider.stu/Sync/Data/Phenology/MS-LSP/ogi_ivvavik.tif",
  "C:/Users/jseider.stu/Sync/Data/Phenology/MS-LSP/phenology_EVIarea.tif"
)

# 2. Load reference template and build stack
template_raster <- rast(raster_paths[1])
raster_list <- lapply(raster_paths, rast)

raster_list[[3]] <- project(raster_list[[3]], template_raster, method = "bilinear")
raster_list[[13]] <- project(raster_list[[13]], template_raster, method = "bilinear")

ivvavik_stack <- rast(raster_list)

# 3. Load camera locations (make sure Project is included)
cameras_csv <- read.csv("./Data/Cameras_Partners/PhenologyCameraLocations.csv") %>% 
  filter(Project %in% c("CaribouCam Ivvavik NP", "Ivvavik National Park")) %>% 
  select(Project, Site, Latitude, Longitude)

cameras <- vect(cameras_csv, 
                geom = c("Longitude", "Latitude"),
                crs = "EPSG:4326") %>% 
  project(ivvavik_stack) %>% 
  buffer(50)

data <- terra::extract(ivvavik_stack, cameras, 
                       fun = mean, na.rm = TRUE, 
                       bind = TRUE)

# 4. Updated plotting function with project-specific colors
plot_coverage_comparison <- function(stack, extracted_data, layer_name) {
  # Extract full study area values for the specified layer
  study_vals <- as.numeric(values(stack[[layer_name]], na.rm = TRUE))
  study_vals <- study_vals[!is.na(study_vals)]
  df_study <- data.frame(Value = study_vals)
  
  # Extract camera network values
  df_extracted <- as.data.frame(extracted_data)
  if (!layer_name %in% names(df_extracted)) {
    stop("Layer name not found in the extracted data frame. Check names(ivvavik_stack).")
  }
  
  camera_vals <- df_extracted[[layer_name]]
  
  # Compute density curve height for each camera value
  d <- density(study_vals)
  dens_fun <- approxfun(d$x, d$y, rule = 2)
  
  df_cameras <- data.frame(
    Value = camera_vals,
    Site = if("Site" %in% names(df_extracted)) df_extracted$Site else seq_along(camera_vals),
    Project = if("Project" %in% names(df_extracted)) df_extracted$Project else "Unknown",
    YEnd = dens_fun(camera_vals)
  )
  df_cameras <- df_cameras[!is.na(df_cameras$Value), ]
  
  # Generate plot
  p <- ggplot() +
    geom_density(data = df_study, aes(x = Value, fill = "Entire Study Area"), alpha = 0.4, color = "grey45", linewidth = 0.8) +
    geom_segment(data = df_cameras, aes(x = Value, xend = Value, y = 0, yend = YEnd, color = Project), linewidth = 1.0, alpha = 0.9) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold")
    ) +
    labs(
      title = paste0("Coverage Comparison: ", layer_name),
      subtitle = "Vertical lines show camera placement along the study area gradient by project",
      x = "Value",
      y = "Density",
      fill = NULL,
      color = "Project"
    ) +
    scale_fill_manual(values = c("Entire Study Area" = "grey70")) +
    scale_color_manual(values = c(
      "Ivvavik National Park" = "#01BCBF",
      "CaribouCam Ivvavik NP" = "#CD9300"
    ))
  
  return(p)
}

# Example usage:
plot_coverage_comparison(ivvavik_stack, data, "dem_10m")
