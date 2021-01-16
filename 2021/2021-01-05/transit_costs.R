# Packages
library(tidyverse)
library(extrafont)
library(countrycode)
library(ggflags)

# load fonts
loadfonts(device = "win", quiet = TRUE)

# Tidy Tuesday Data
transit_cost <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-05/transit_cost.csv')

# Drop NA observations, Convert UK to correct Country Code of GB, and add country_name column
transit_cost <- transit_cost %>%
  filter(!is.na(country)) %>%
  mutate(code = tolower(country),
         country = case_when(country == "UK" ~ "GB", 
                             TRUE ~ country)) %>%
  left_join(codelist %>%
              select(continent, country.name.en, ecb),
            by = c("country" = "ecb"))

# Examine and View Scandinavian Transit Projects
plot <- transit_cost %>%
  filter(country.name.en %in% c("Denmark", "Finland", "Norway", "Sweden")) %>%
  arrange(cost_km_millions) %>%
  mutate(line = factor(line, levels = line)) %>%
  ggplot(aes(x = line, y = cost_km_millions, fill = country.name.en)) + 
  geom_bar(stat = "identity", size = 2) +
  geom_text(y = 1, aes(label = toupper(line)), hjust = 0, size = 11, color = "#343434", family = "Georgia") +
  geom_flag(y = -6.5, aes(country = code), size = 16) + 
  scale_fill_manual(values=c("#FBFAF5", "#4682b4", "#ff4d4d", "#ffd633")) +
  scale_y_continuous(breaks = seq(50, 250, 50),labels = c("50","100", "150", "200", "250 Million USD per Kilometer")) +
  coord_flip() + 
  labs(title = "Transit Projects in Scandinavia",
       subtitle = "Review of data provided by Transit Costs Project shows that Sweden has the most expensive line in terms of \ncost per kilometer. After Sweden, Denmark and Norway have the next most expensive lines, respectively.") + 
  theme(axis.title = element_blank(),
        axis.text.x = element_text(family = "Georgia", face = "bold", color = "#FBFAF5", size = 14),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#212F3C"),
        plot.background = element_rect(fill = "#212F3C"),
        plot.title = element_text(family = "Georgia", face = "bold", size = 34, color = "#FBFAF5"),
        plot.subtitle = element_text(family = "Georgia", face = "bold", size = 20, color = "#f7f5fb"),
        plot.margin = margin(2, 2, 1, 2, unit = "cm"))

ggsave("file_path.png", plot, width = 20, height = 12, dpi = 450)
