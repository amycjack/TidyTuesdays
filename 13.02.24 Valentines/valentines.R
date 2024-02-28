historical_spending <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-13/historical_spending.csv')
gifts_age <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-13/gifts_age.csv')
gifts_gender <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-13/gifts_gender.csv')

library(tidyverse)
library(ggplot2)
install.packages("ggExtra")
library(ggstream)
library(ggthemes)
library(ggtext)

new_data <- historical_spending %>%
  pivot_longer(cols = -c(Year, PercentCelebrating), names_to = "gifts", values_to = "value") %>%
  filter(!grepl("PerPerson", gifts))


plot <- ggplot(new_data, aes(Year, value, fill = gifts)) +
  geom_stream(colour="white", bw = 0.9, alpha=.9, show.legend = FALSE) +
  geom_stream_label(aes(label = gifts)) +
  scale_fill_brewer(palette="PuRd") +
  theme(legend.position = "none") +
  theme_bw(base_family="Calibri") +
  theme(panel.grid.major=element_blank()) +
  theme(panel.grid.minor=element_blank()) +
  theme(panel.border=element_blank()) +
  theme(axis.ticks=element_blank()) +
  theme(plot.title=element_text(face="bold", hjust = 0.5)) +
  theme(plot.subtitle=element_text(face="italic", size=9, margin=margin(b=12))) +
  theme(plot.caption=element_text(size=7, margin=margin(t=12), color="#7a7d7e")) +
  theme(plot.title=element_markdown(size=22, family="bell",lineheight=0,color="grey60"),
        plot.subtitle=element_markdown(size=8, family="bell",lineheight=0,color="grey60"),
        plot.caption=element_text(size=7,family="bell",color="grey40",hjust=.5, margin=margin(t=10)),
        plot.background = element_rect(color = "#ffffff", fill = "#ffffff"),
        legend.background = element_rect(color = NA, fill = "#ffffff"),) +
  labs(caption="#TidyTuesday 14-02-24: Valentine's Day Consumer Spending | Github: amycjack | Twitter: amycjack") + 
  labs(title = "<span style='color:#6D3A5D'>**Valentine's Day**</span> Consumer Data",
       x = "Year", y = "Average Amount Spent (USD)") +
  scale_x_continuous(breaks = seq(from = 2010, to = 2022, by = 1)) +
  annotate("text", x = 2010, y = 130, hjust = 0,
           label = "Average spending on Valentine's Day gifts increase by 52% from 2010 to 2022, according to the US National Retail Federation.")
plot

ggsave(paste0("plot", format(Sys.time(), "%d%m%Y"), ".png"),
       dpi = 500,
       width = 10,
       height =  11)

