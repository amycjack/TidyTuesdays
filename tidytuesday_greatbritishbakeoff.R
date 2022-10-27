library(tidyverse)
library(devtools)
library(ggpubr)
library(ggplot2)

bakers <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-25/bakers.csv')


p <- ggscatter(bakers, x = "total_episodes_appeared", y = "technical_median",
               group="technical_median", color = "series_winner",                                
               sorting = "ascending",                       
               rotate = TRUE,                                
               dot.size = 10,                                 
               y.text.col = TRUE,                            
               ggtheme = theme_bw(),
               size = "technical_winner",
               alpha = 0.5,
               legend = "bottom",
               xlab ="Total Episodes Appeared") +
  gradient_color(c("#386cb0", "#7fc97f")) +
  scale_size(range = c(4, 15))

p2 <- p + scale_y_log10() + scale_x_log10() + labs(
  title = "Great British Bake Off Data Visualization",
  subtitle = "Scatter plot of median techincal score vs total episode appearenaces, for 10 seasons",
  x = "Total Episodes Appeared",
  y = "Median Ranking in the Technical") +
  scale_y_continuous(limits = c(1, 9), n.breaks = 10) +
  scale_x_continuous(limits = c(1, 10), n.breaks = 10)