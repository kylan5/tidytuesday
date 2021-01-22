# load packages
library(rKenyaCensus)
library(tidyverse)
library(extrafont)
library(viridis)
library(sf)

# wrangle data
internet_usage <-
  st_as_sf(KenyaCounties_SHP) %>% 
  select(County, geometry) %>%
  left_join(
    select(filter(V4_T2.33, AdminArea == "County"), County, UoI_Total_Perc),
    by = "County")

# visualize internet usage by county 
plot <- 
ggplot(internet_usage) +
  geom_sf(aes(fill = UoI_Total_Perc), color = "#ffffff", size = 0.6) +
  scale_fill_viridis("Percent of Population Using Internet by County", option = "inferno",
                      labels = function(x) paste0(x, "%")) + 
  guides(fill = guide_colorsteps(barwidth = 20, barheight = 0.85, title.position = "top")) +
  labs(title = "Internet Usage in Kenya",
       caption = "Data Source: rKenyaCensus | Shelmith Kariuki") +
  theme(
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_rect(fill = "#f0f0f0"),
    legend.position = "bottom",
    legend.text = element_text(family = "Candara", size = 14, color = "#0a0a0a"),
    legend.title = element_text(family = "Candara", hjust = 0.5, size = 13, color = "#0a0a0a"),
    plot.title = element_text(family = "Candara", face = "bold", hjust = 0.5, size = 36, color = "#0a0a0a"),
    plot.caption = element_text(family = "Candara", hjust = 0.5, size = 12, color = "#440062"),
    plot.margin = margin(1, 1, 1, 1, unit = "cm")
  )

ggsave("file_path/internet_usage_kenya.png", plot, width = 22, height = 24, unit = "cm", dpi = 500)
