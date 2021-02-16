# packages
library(tidyverse)
library(extrafont)
library(patchwork)
library(scales)
library(ggtext)

# data
student_debt <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/student_debt.csv')
retirement <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/retirement.csv')
home_owner <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/home_owner.csv')
race_wealth <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/race_wealth.csv')

# wrangle and visualize 
p1 <-
  home_owner %>%
  filter(year > 1988 & race != "Hispanic") %>%
  ggplot(aes(year, home_owner_pct, group = race, color = race)) +
  geom_line(size = 1.5) +
  scale_color_manual(values=c("#ac3a74", "#006ab5")) + 
  geom_label(data = home_owner %>% filter(year == 2016 & race != "Hispanic"), 
             aes(x = year, y = home_owner_pct, label = race, color = race, size = 7)) +
  scale_x_continuous(breaks = seq(1990,2015,5)) +
  scale_y_continuous(label = percent, limits = c(.40, .75), breaks = seq(.45, .75, .10)) + 
  labs(title = "Homeownership percentage for families") +  
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.x = element_line(color = "#FBFAF5"),
    axis.ticks.y = element_blank(),
    axis.text = element_text(family = "Candara", face = "bold", size = 14, color = "#FBFAF5"),
    plot.title = element_text(family = "Candara", size = 16, hjust = 0.5, color = "#FBFAF5"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#0d0d0d", color = NA),
    panel.background = element_rect(fill = "#0d0d0d"),
    plot.margin = margin(10, 10, 10, 10, unit = "mm")
  )

p2 <-
  student_debt %>%
  filter(race != "Hispanic") %>%
  ggplot(aes(year, loan_debt, color = race)) +
  geom_line(size = 1.5) + 
  scale_color_manual(values=c("#ac3a74", "#006ab5")) +
  geom_label(data = student_debt %>% filter(year == 2016 & race != "Hispanic"), 
             aes(x = year, y = loan_debt, label = race, color = race, size = 7)) +
  scale_x_continuous(breaks = seq(1990,2015,5)) +
  scale_y_continuous(limits = c(0,15000), breaks = seq(0,15000,5000), labels = c("$0", "$5,000", "$10,000", "$15,000")) + 
  labs(title = "Average family student loan debt for ages 25-55 normalized to 2016 dollars") + 
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.x = element_line(color = "#FBFAF5"),
    axis.ticks.y = element_blank(),
    axis.text = element_text(family = "Candara", face = "bold", size = 14, color = "#FBFAF5"),
    plot.title = element_text(family = "Candara", size = 16, hjust = 0.5, color = "#FBFAF5"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#0d0d0d", color = "#ffe34d", size = 3),
    panel.background = element_rect(fill = "#0d0d0d"),
    plot.margin = margin(10, 10, 10, 10, unit = "mm")
  )

p3 <-
  retirement %>%
  filter (race != "Hispanic") %>%
  ggplot(aes(year, retirement, color = race)) +
  geom_line(size = 1.5) + 
  scale_color_manual(values=c("#ac3a74", "#006ab5")) +
  geom_label(data = retirement %>% filter(year == 2016 & race != "Hispanic"), 
             aes(x = year, y = retirement, label = race, color = race, size = 7)) +
  scale_x_continuous(breaks = seq(1990,2015,5)) +
  scale_y_continuous(label = dollar) + 
  labs(title = "Average family liquid retirement savings normalized to 2016 dollars") + 
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.x = element_line(color = "#FBFAF5"),
    axis.ticks.y = element_blank(),
    axis.text = element_text(family = "Candara", face = "bold", size = 14, color = "#FBFAF5"),
    plot.title = element_text(family = "Candara", size = 16, hjust = 0.5, color = "#FBFAF5"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#0d0d0d", color = NA),
    panel.background = element_rect(fill = "#0d0d0d"),
    plot.margin = margin(10, 10, 10, 10, unit = "mm")
  )

p4 <-
  race_wealth %>% 
  filter(type == "Median" & year > 1988 & race != "Non-White" & race != "Hispanic") %>%
  ggplot(aes(year, wealth_family, color = race)) +
  geom_line(size = 1.5) + 
  scale_color_manual(values=c("#ac3a74", "#006ab5")) +
  geom_label(data = race_wealth %>% filter(year == 2016 & type == "Median" & race != "Non-White" & race != "Hispanic"), 
             aes(x = year, y = wealth_family, label = race, color = race, size = 7)) +
  scale_x_continuous(breaks = seq(1990,2015,5)) +
  scale_y_continuous(label = dollar) +
  labs(title = "Median family wealth by normalized to 2016 dollars") + 
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.x = element_line(color = "#FBFAF5"),
    axis.ticks.y = element_blank(),
    axis.text = element_text(family = "Candara", face = "bold", size = 14, color = "#FBFAF5"),
    plot.title = element_text(family = "Candara", size = 16, hjust = 0.5, color = "#FBFAF5"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#0d0d0d", color = NA),
    panel.background = element_rect(fill = "#0d0d0d"),
    plot.margin = margin(10, 10, 10, 10, unit = "mm")
  )


# patchwork
patchwork <- p1 + p2 + p3 + p4

plot <-
patchwork + 
  plot_layout(guides = "collect") + 
  plot_annotation(
                  title = "<b  style='color:#ffe34d'>Student Loans:</b> Yet Another Financial Hurdle", 
                  subtitle = "The disparity between Black and White families has been clear on measures of homeownership, retirement savings, and wealth since 1989. In 2007, Black families surpassed White\nfamilies with average student loan debt as well and have continued to outpace them since.",
                  caption = "Data source: Urban Institute | www.urban.org", 
                  theme = theme(plot.background = element_rect(fill = "#0d0d0d", color = NA), 
                                text = element_text(family = "Candara", color = "#FBFAF5"),
                                plot.caption = element_text(size = 11, vjust = 8, color = "#a9a9a9"), 
                                plot.title = element_markdown(size = 32), 
                                plot.subtitle = element_text(family = "Candara", size = 18), 
                                plot.margin = margin(4, 2.5, 2.5, 2.5, unit = "mm"))
                  )
                  
ggsave("file_path/wealth.png", plot, width = 20, height = 12, dpi = 500)
