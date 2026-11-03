library(tidyverse)
library(lubridate)

# Load your data (replace with your actual file path/name)
df <- read_csv("C:/Users/jseider.stu/Desktop/PhenologyCameraLocations.csv")

df %>%
  arrange(Project, Site) %>%
  mutate(Site = fct_inorder(Site)) %>%
  pivot_longer(
    cols = matches("^(Start|End)_Date\\d+$"),
    names_to = c(".value", "deployment_period"),
    names_pattern = "(Start_Date|End_Date)(\\d+)"
  ) %>%
  filter(!is.na(Start_Date) & Start_Date != "NA") %>%
  mutate(
    Start_Date = dmy(Start_Date),
    End_Date = dmy(End_Date)
  ) %>%
  # Map color to Project
  ggplot(aes(y = fct_rev(Site), color = Project)) +
  geom_segment(
    aes(x = Start_Date, xend = End_Date, yend = fct_rev(Site)),
    linewidth = 4 
  ) +
  labs(
    x = "Date",
    y = "Camera Site",
    title = "Camera Deployment Timeline",
    color = "Project"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dotted", color = "gray80")
  )
