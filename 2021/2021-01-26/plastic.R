# packages
library(tidyverse)
library(extrafont)
library(ggtext)

# data
plastics <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-26/plastics.csv")

# wrangle and visualize 
plot <-
plastics %>%
  filter(!is.na(parent_company),
         parent_company != "Unbranded" & parent_company != "null",
         year == "2020") %>%
  count(parent_company, sort = TRUE) %>%
  top_n(15) %>%
  mutate(parent_company = fct_reorder(parent_company, n)) %>%
  ggplot(aes(x = parent_company, y = n)) + 
  geom_segment(aes(x = parent_company, xend = parent_company, y = 0, yend = n), color = "#FDDDA4", size = 1.15) + 
  geom_point(color = "#C7CEF6", size = 10) +
  geom_text(aes(label = n), family = "Georgia", color = "#2a2a29", size = 5, vjust = 0.3) + 
  scale_y_continuous(breaks = seq(0,50,5), labels = c("0","5", "10", "15", "20", "25", "30", "35", "40", "45", "50 Countries"), 
                     expand = expansion(mult = c(0, 0.05))) +
  labs(title = "<b  style='color:#FDDDA4'>Global Reach:</b> Which plastic producing company appeared in the most countries?",
       subtitle = "Volunteers in 55 countries conducted a brand audit report for the Break Free from Plastic movement in 2020. Coca-Cola was discovered in 51 of these.",
       caption = "Data Source: Break Free from Plastic") +
  coord_flip() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_rect(fill = "#456355"),
    plot.background = element_rect(fill = "#456355"),
    axis.title = element_blank(),
    axis.text.x = element_text(family = "Georgia", color = "#eff3f3", size = 13),
    axis.text.y = element_text(family = "Georgia", color = "#eff3f3", size = 14, vjust = 0.25),
    axis.ticks.x = element_line(color = "#eff3f3"),
    axis.ticks.y = element_blank(),
    plot.title = element_markdown(family = "Georgia", face = "bold", color = "#eff3f3", size = 30, hjust = 3),
    plot.subtitle = element_text(family = "Georgia", color = "#F7B0AA", size = 18, hjust = -22),
    plot.caption = element_text(family = "Georgia", color = "#F7B0AA", size = 11, vjust = -3, hjust = -0.18),
    plot.margin = margin(1.5, 1, 1.5, 1.5, unit = "cm")
  )

ggsave("file_path/plastic.png", plot, width = 20, height = 12, dpi = 500)
