library(tidyverse)
library(devtools)
library(ggpubr)
library(ggplot2)

df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-01/horror_movies.csv')

# add theme

theme_minimal()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "#4a4e4d"),
        text = element_text(family = "Segoe UI", color = "#4a4e4d"),
        strip.text.y.left  = element_text(angle = 0),
        panel.background = element_rect(fill = "white", color = "white"),
        strip.background = element_rect(fill = "white", color = "white"),
        strip.text = element_text(color = "#4a4e4d", family = "Segoe UI"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.spacing = unit(0, "lines"),
        plot.margin = margin(1,1,.5,1, "cm"))

# filtering and manipulating data to add profit

df %>%
  filter(release_year >= 2021)%>%
  select(title, budget, revenue, vote_average, popularity, runtime)%>% #select columns of interest
  mutate(diff = revenue - budget) -> df2

df3 <- df2[df2$diff != 0, ] 

df2 %>%
filter(diff = 0) -> df3
  
# plotting

p <- ggscatter(df3, x = "vote_average", y = "diff",
           size = "popularity",
           color = "#54000e",
          dot.size = 10,
          alpha = 0.5,
           sorting = "descending",                       # Sort value in descending order
           ggtheme = theme_bw(),
          xlab ="Average Vote",
          ylab ="Profit (Budget - Revenue)",# ggplot2 theme
) +
  scale_size(range = c(1, 10)) +
    geom_text(x = 5, y = -70, label = "Loss", size = 7, color = "#e34b6a",)+
  geom_rect(xmin = -1, xmax = 11,
            ymin = 0, ymax = -100, fill = "#e0b1b1", alpha = .007) +
  annotate("text", x = 6, y = 220, label = "A Quiet Place II") +
  annotate(
    geom = "curve", x = 7.2, y = 220, xend = 7.5, yend = 230, 
    curvature = .3, arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate("text", x = 1, y = -50, label = "The Pale Blue Eye") +
  annotate(
    geom = "curve", x = 1.3, y = -60, xend = 0.3, yend = -72, 
    curvature = -0.2, arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate("text", x = 9, y = 20, label = "Orphan: First Kill") +
  annotate(
    geom = "curve", x = 8, y = 10, xend = 7.2, yend = 11, 
    curvature = -0.2, arrow = arrow(length = unit(2, "mm"))
  ) +labs(
    title = "2021-22 Horror Films: Average vote vs Profit")

 # save
ggsave(paste0("p", format(Sys.time(), "%d%m%Y"), ".png"),
       dpi = 320,
       width = 7,
       height =  4.1)
