# Phenology Plots

library(ggplot2)
library(dplyr)
library(forcats)
library(lubridate)
library(ggnewscale)
library(grid)

# Helper function for NULL values in custom key glyphs
`%||%` <- function(a, b) if (!is.null(a)) a else b

# Custom key glyph for 100% Green (shorter vertical line)
draw_key_short_vline <- function(data, params, size) {
  grid::segmentsGrob(
    x0 = 0.5, y0 = 0.28,
    x1 = 0.5, y1 = 0.72,
    gp = grid::gpar(
      col = data$colour %||% "black",
      lwd = (data$linewidth %||% 1) * .pt,
      lty = data$linetype %||% 1,
      lineend = "butt"
    )
  )
}

# Custom key glyph for Peak Greenness (taller vertical line)
draw_key_tall_vline <- function(data, params, size) {
  grid::segmentsGrob(
    x0 = 0.5, y0 = 0.08,
    x1 = 0.5, y1 = 0.92,
    gp = grid::gpar(
      col = data$colour %||% "black",
      lwd = (data$linewidth %||% 1) * .pt,
      lty = data$linetype %||% 1,
      lineend = "butt"
    )
  )
}

# Load data directly from local file
df <- read.csv("C:/Users/jseider.stu/Desktop/phenology.csv")

# -----------------------------------------------------------------------------
# Data Preparation
# -----------------------------------------------------------------------------
df_plot <- df %>%
  mutate(
    caribou_date      = ymd(X1st.Spring.Caribou),
    snow_10_date      = ymd(First.Ground.Appearance),
    snow_50_date      = ymd(X50..Snow.Free),
    snow_90_date      = ymd(X.90..Snow.Free..Least.Snow.),
    green_100_date    = ymd(X100..Green),
    peak_green_date   = ymd(Peak.Greenness),
    camera_death_date = ymd(Camera.Death),
    
    # Suppress Camera Death X for cameras that died before March 1, 2026
    camera_death_plot = if_else(
      !is.na(camera_death_date) & camera_death_date >= ymd("2026-03-01"),
      camera_death_date,
      as.Date(NA)
    ),
    
    # Offsets relative to 50% snowmelt
    caribou_rel      = as.numeric(caribou_date - snow_50_date),
    snow_10_rel      = as.numeric(snow_10_date - snow_50_date),
    snow_90_rel      = as.numeric(snow_90_date - snow_50_date),
    
    # Categorize snowmelt bar type for legend mapping
    snow_bar_type = if_else(!is.na(caribou_rel) | !is.na(caribou_date), "Caribou Present", "No Caribou Sighted"),
    
    # Flag sites with partial snowmelt records (has start date but missing 90% snow-free date)
    missing_snow_end = !is.na(snow_10_date) & is.na(snow_90_date),
    
    # Append custom notes dynamically to site names
    Site_name_display = case_when(
      Site_name == "Mount Conybeare" ~ "Mount Conybeare*",
      missing_snow_end              ~ paste0(Site_name, "**"),
      TRUE                          ~ Site_name
    ),
    
    # Reorder sites by Latitude while keeping all sites on the axis
    Site_name_display = fct_reorder(as.factor(Site_name_display), Latitude)
  )

# -----------------------------------------------------------------------------
# Dynamic Y-Axis Label Formatting (Red & Italic for Missing 50% Snowmelt Sites)
# -----------------------------------------------------------------------------
missing_snow_sites <- df_plot %>%
  group_by(Site_name_display) %>%
  summarise(missing_50 = is.na(first(snow_50_date)), .groups = "drop")

# Align formatting vectors directly with the ordered factor levels (bottom to top)
axis_formatting <- data.frame(Site_name_display = levels(df_plot$Site_name_display)) %>%
  left_join(missing_snow_sites, by = "Site_name_display")

axis_colors <- if_else(axis_formatting$missing_50, "red", "black")
axis_faces  <- if_else(axis_formatting$missing_50, "italic", "plain")


# -----------------------------------------------------------------------------
# Smooth Polygon Slices for Greenness Transition Bar (Sloped Trapezia)
# -----------------------------------------------------------------------------
n_slices <- 150

df_green_bar <- df_plot %>%
  filter(!is.na(green_100_date) & !is.na(peak_green_date) & green_100_date <= peak_green_date) %>%
  group_by(Site_name_display) %>%
  reframe(
    y_num   = as.numeric(Site_name_display),
    slice   = 0:(n_slices - 1),
    p_start = slice / n_slices,
    p_end   = (slice + 1.01) / n_slices,
    p_mid   = (p_start + p_end) / 2,
    x1      = green_100_date + (as.numeric(peak_green_date - green_100_date) * p_start),
    x2      = green_100_date + (as.numeric(peak_green_date - green_100_date) * p_end),
    h1      = 0.15 + (p_start * (0.25 - 0.15)),
    h2      = 0.15 + (p_end   * (0.25 - 0.15)),
    progress = p_mid
  ) %>%
  group_by(Site_name_display, slice) %>%
  reframe(
    poly_id  = paste(Site_name_display, slice, sep = "_"),
    progress = progress,
    x        = c(x1, x2, x2, x1),
    y        = c(y_num - h1, y_num - h2, y_num + h2, y_num + h1)
  ) %>%
  ungroup()


# -----------------------------------------------------------------------------
# Chart 1: Absolute Date Plot
# -----------------------------------------------------------------------------
p_absolute <- ggplot(df_plot, aes(y = Site_name_display)) +
  # 1a. Translucent snowmelt duration bars (coalesces missing xend to x to render partial data)
  geom_segment(
    aes(
      x = snow_10_date, 
      xend = coalesce(snow_90_date, snow_10_date), 
      y = Site_name_display, 
      yend = Site_name_display, 
      color = snow_bar_type
    ),
    linewidth = 6,
    alpha = 0.45,
    na.rm = TRUE
  ) +
  
  # 1b. Vertical tick for First Ground Appearance (snow_10_date) ensuring single-point snowmelt renders
  geom_point(
    aes(
      x = snow_10_date,
      color = snow_bar_type
    ),
    shape = 124,
    size = 5,
    na.rm = TRUE
  ) +
  scale_color_manual(
    values = c("Caribou Present" = "skyblue3", "No Caribou Sighted" = "gray80"),
    name = "Snowmelt Window (10%-90%)",
    guide = guide_legend(order = 1)
  ) +
  
  # 2. Translucent sloped polygon gradient between 100% Green and Peak Greenness
  geom_polygon(
    data = df_green_bar,
    aes(
      x = x,
      y = y,
      group = poly_id,
      fill = progress
    ),
    alpha = 0.45,
    inherit.aes = FALSE
  ) +
  scale_fill_gradient(
    low = "#7CCC85",
    high = "#0A3610",
    guide = "none"
  ) +
  
  # Reset color scale for point, tick, and death markers
  new_scale_color() +
  
  # 3. Point for first caribou sighting on absolute date
  geom_point(
    aes(
      x = caribou_date, 
      color = "First Caribou Sighting"
    ),
    size = 3,
    key_glyph = draw_key_point,
    na.rm = TRUE
  ) +
  
  # 4. Dark red X for Camera Death date (rendered only if March 1, 2026 or later)
  geom_point(
    aes(
      x = camera_death_plot,
      color = "Camera Death"
    ),
    shape = 4,
    size = 3.5,
    stroke = 1.5,
    na.rm = TRUE
  ) +
  
  # 5. Small vertical line tick for 100% Green
  geom_segment(
    aes(
      x = green_100_date,
      xend = green_100_date,
      y = as.numeric(Site_name_display) - 0.15,
      yend = as.numeric(Site_name_display) + 0.15,
      color = "100% Green"
    ),
    linewidth = 1.2,
    key_glyph = draw_key_short_vline,
    na.rm = TRUE
  ) +
  
  # 6. Small vertical line tick for Peak Greenness
  geom_segment(
    aes(
      x = peak_green_date,
      xend = peak_green_date,
      y = as.numeric(Site_name_display) - 0.25,
      yend = as.numeric(Site_name_display) + 0.25,
      color = "Peak Greenness"
    ),
    linewidth = 1.2,
    key_glyph = draw_key_tall_vline,
    na.rm = TRUE
  ) +
  scale_color_manual(
    values = c(
      "First Caribou Sighting" = "#D95F02",
      "Camera Death"           = "darkred",
      "100% Green"             = "#7CCC85",
      "Peak Greenness"         = "#0A3610"
    ),
    breaks = c(
      "First Caribou Sighting",
      "Camera Death",
      "100% Green",
      "Peak Greenness"
    ),
    name = "Event & Phenophase Markers",
    guide = guide_legend(order = 2)
  ) +
  
  # Date formatting for X-axis
  scale_x_date(
    date_breaks = "1 week",
    date_labels = "%b %d"
  ) +
  
  labs(
    x = "Date",
    y = "Site",
    title = "First Spring Caribou Sighting and Vegetation Phenology (Absolute Dates)",
    subtitle = "Shaded bar spans First Ground Appearance to 90% Snow Free; Ticks show 100% Green and Peak Greenness; Red X shows Camera Death",
    caption = "* Windswept Ridge\n** Partial snowmelt data (missing 90% snow-free date)\nRed italicized site names indicate missing more data."
  ) +
  theme_minimal() +
  theme(
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0, face = "italic", size = 9, margin = margin(t = 10)),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(color = axis_colors, face = axis_faces)
  )

print(p_absolute)

# -----------------------------------------------------------------------------
# Chart 2: Relative Days Plot (Standard Relative View)
# -----------------------------------------------------------------------------
p_relative <- ggplot(df_plot, aes(y = Site_name_display)) +
  # 1a. Translucent snowmelt duration bars (supports single point fallback via coalesce)
  geom_segment(
    aes(
      x = snow_10_rel, 
      xend = coalesce(snow_90_rel, snow_10_rel), 
      y = Site_name_display, 
      yend = Site_name_display, 
      color = snow_bar_type
    ),
    linewidth = 6,
    alpha = 0.45,
    na.rm = TRUE
  ) +
  
  # 1b. Vertical tick for First Ground Appearance in relative view
  geom_point(
    aes(
      x = snow_10_rel,
      color = snow_bar_type
    ),
    shape = 124,
    size = 5,
    na.rm = TRUE
  ) +
  scale_color_manual(
    values = c("Caribou Present" = "skyblue3", "No Caribou Sighted" = "gray80"),
    name = "Snowmelt Window (10%-90%)",
    guide = guide_legend(order = 1)
  ) +
  
  # Reset color scale for event indicators
  new_scale_color() +
  
  # 2. Vertical reference line at 0 (50% Snow Free Date)
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray30", linewidth = 0.8) +
  
  # 3. Line linking 50% snow free (0) to first caribou sighting
  geom_segment(
    aes(
      x = 0, 
      xend = caribou_rel, 
      y = Site_name_display, 
      yend = Site_name_display, 
      color = "First Caribou Sighting"
    ),
    linewidth = 1,
    na.rm = TRUE
  ) +
  
  # 4. Point for first caribou sighting
  geom_point(
    aes(
      x = caribou_rel, 
      color = "First Caribou Sighting"
    ),
    size = 3,
    na.rm = TRUE
  ) +
  scale_color_manual(
    values = c("First Caribou Sighting" = "#D95F02"),
    name = NULL,
    guide = guide_legend(order = 2)
  ) +
  
  labs(
    x = "Days Relative to 50% Snow Free",
    y = "Site",
    title = "First Spring Caribou Sighting Relative to Snowmelt",
    subtitle = "Shaded bar spans First Ground Appearance to 90% Snow Free",
    caption = "* Windswept Ridge\n** Partial snowmelt data (missing 90% snow-free date)\nRed italicized site names indicate missing more data."
  ) +
  theme_minimal() +
  theme(
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0, face = "italic", size = 9, margin = margin(t = 10)),
    axis.text.y = element_text(color = axis_colors, face = axis_faces)
  )

print(p_relative)