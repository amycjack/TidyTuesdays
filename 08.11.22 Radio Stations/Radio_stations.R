#load packages
library(sf)
library(tidyverse)
library(ggplot2) 
library(usmap)
library(RColorBrewer)
library(PNWColors)
library(ggtext)
library(showtext)
library(data.table)

# input date
raw_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-08/state_stations.csv')

# clean state names
data <- gsub(" ", "_", raw_data)

# count the radio station formats
count <- data %>%
  group_by(format|state) %>%
  summarise(n = n()) %>%
  mutate(Freq = n/sum(n))

# selected the highest number of stations for each state
df <- setDT(count)[, .SD[which.max(n)], state]


# plot using usmap library for ease
map <- plot_usmap(regions="states", data = df, values = "format", color = "white") +
  scale_fill_brewer(palette="Set2", name = "Format") +
  labs(title = "<span style='color:#957DAD'>Country Radio</span> dominates the USA airwaves", subtitle = "The most common radio station format per state") +
  theme(legend.position = "right") +
  theme(plot.title=element_markdown(size=15, family="bell",lineheight=1.3,color="grey60"),
        plot.subtitle=element_markdown(size=10, family="bell",lineheight=1.3,color="grey60"),
        plot.caption=element_text(size=7,family="bell",color="grey40",hjust=.5, margin=margin(t=10)),
        plot.background = element_rect(color = "#FCF5EE", fill = "#FCF5EE"),
        legend.background = element_rect(color = NA, fill = "#FCF5EE"),) +
  labs(caption="#TidyTuesday 08-11-22: USA Radio Stations | Github: amycjack | Twitter: amycjack")

# save
ggsave(paste0("map", format(Sys.time(), "%d%m%Y"), ".png"),
       dpi = 320,
       width = 7,
       height =  4.1)
