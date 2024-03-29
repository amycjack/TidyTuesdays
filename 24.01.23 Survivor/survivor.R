data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-24/survivalists.csv')
loadouts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-24/loudouts.csv')

library(tidyverse)
library(ggplot2)
library(ggtext)

total <- merge(data,loadouts,by="name")

freq <- total %>%
  group_by(item, result) %>%
  summarise(n = n())

freq2 <- total %>%
  group_by(item) %>%
  summarise(Popularity = n())

av <- total  %>%
  group_by(item) %>% 
  summarise(avg = mean(result), sd=sd(result))

df <- merge(av,freq2,by="item")


plot <- ggplot(df,aes(x=reorder(item,-avg),
                   y=avg)) +
  geom_point(aes(size = Popularity, colour=avg), stat="identity") +
  coord_flip() +
  xlab("") +
  theme_bw() +
  geom_errorbar(aes(ymin=avg-sd, ymax=avg+sd, color=avg), width=.2) +
  labs(y = 'Item Performance (Average Episodes Survived)') +
  labs(title = "<span style='color:#957DAD'>Survivor</span> Loadout Performace", subtitle = "Do your 10 chosen items determine your chance of surviving to episode 10?", caption = '@amycjack') +
  theme_classic() +
  theme(legend.position = "right") +
  theme(plot.title=element_markdown(size=20, family="bell",lineheight=1.3,color="grey40"),
        plot.subtitle=element_markdown(size=15, family="bell",lineheight=1.3,color="grey40"),
        plot.caption=element_text(size=10,family="bell",color="grey60",hjust=.5, margin=margin(t=10)),
        plot.background = element_rect(color = "#FCF5EE", fill = "#FCF5EE"),
        panel.background = element_rect(color = "#FCF5EE", fill = "#FCF5EE"),
        legend.background = element_rect(color = "#FCF5EE", fill = "#FCF5EE"),) +
  labs(caption="#TidyTuesday 24-01-23: Survivor | Github: amycjack | Twitter: amycjack") +
  scale_colour_gradient(low="#C70039", high="#2980B9") +
  scale_y_continuous(breaks=seq(0,10,1)) +
  scale_size(range = c(2, 10))
