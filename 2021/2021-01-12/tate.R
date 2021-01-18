# packages
library(tidyverse)
library(janitor)
library(ggtext)

# Load fonts
extrafont::loadfonts(device="win")

# data
artwork <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-12/artwork.csv")
artists <- read_csv("https://github.com/tategallery/collection/raw/master/artist_data.csv")

# clean column names with janitor package
artwork <- artwork %>%
  clean_names()

artists <- artists %>%
  clean_names()

# Wrangle and visualize
plot <- artwork %>%
  filter(artist == "Turner, Joseph Mallord William",
         artist_role == "artist" | artist_role == "after") %>%
  left_join(artists, by = c("artist_id" = "id")) %>%
  mutate(age = year - year_of_birth) %>%
  filter(age <= 76) %>%
  group_by(year, age) %>%
  count() %>%
  mutate(paintings_per_day = n / 365) %>%
  ggplot(aes(x = age, y = paintings_per_day)) + 
    geom_bar(aes(fill = paintings_per_day), stat = "identity") + 
    scale_fill_gradient(low = "#6448dc", high = "#6ec2de") + 
    scale_x_continuous(limits = c(10,77), breaks = seq(10,75,5),
                       labels = c("10","15", "20", "25", "30", "35", "40", "45", "50", "55", "60", "65", "70", "75 Years Old")) +
    scale_y_continuous(limits = c(0,9), breaks = seq(0,9,1), labels = seq(0,9,1), 
                       expand = expansion(mult = c(0, .1))) +
  labs(title = "The Productive Artist: <b  style='color:#6ec2de'>Joseph Mallord William Turner</b>",
       subtitle = "Living at the peak of the Industrial Revolution, Turner produced over 39,000 pieces of art throughout his life. These include various works on \npaper, watercolors, and oil paintings. There were an astonishing 36 years in which he created more than one art piece per day on average.",
       caption = "Data Source: The Tate Collection",
       y = "Average Pieces Per Day") + 
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_text(family = "Rockwell", face = "bold", color = "#FBFAF5", size = 20, vjust = 4),
    axis.ticks.x = element_line(color = "#FBFAF5"),
    axis.ticks.y = element_blank(),
    axis.ticks.length.x = unit(.25, "cm"),
    axis.text = element_text(family = "Rockwell", face = "bold", size = 15, color = "#FBFAF5"),
    plot.title = element_markdown(family = "Rockwell", face = "bold", size = 30, color = "#FBFAF5"),
    plot.subtitle = element_text(family = "Rockwell", size = 20, color = "#f7f5fb", margin = margin(0,0,-1,0, unit = "cm")),
    plot.caption = element_text(family = "Rockwell", color = "#8873e4", size = 9, vjust = -1),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "#04030f"),
    plot.background = element_rect(fill = "#04030f"),
    plot.margin = margin(1, 1.5, 1, 1.5, unit = "cm"),
  )

ggsave("file_path/tate_art_turner.png", plot, width = 20, height = 12, dpi = 500)
