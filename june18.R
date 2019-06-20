# June 18 Tidy Tuesday
# setwd()

# load packages
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)

# load data
bird_counts <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-18/bird_counts.csv")

# tidy data
bird_counts_filtered <- bird_counts %>% 
  filter(how_many_counted > 0) %>% 
  mutate(count = 1) %>% 
  group_by(year) %>% 
  mutate(sum.year.species = sum(count),
         sum.year.individuals = sum(how_many_counted),
         prop = sum.year.individuals/sum.year.species*100) %>% 
  ungroup() %>% 
  select(year, sum.year.species, sum.year.individuals, prop) %>% 
  gather(type, value, sum.year.species:prop) %>% 
  distinct(year, type, value) %>% 
  filter(type != "sum.year.species")

# plot 
p <- ggplot(data = bird_counts_filtered, mapping = aes(x = year, y = value, group = type, color = type))
p + geom_line() +
  theme_minimal() +
  labs(title = "Bird count observed, raw and weighted by the number of species observed in the same year",
       subtitle = "Hamilton area, around Christmas time, 1921-2017", 
       x = "",
       y = "Count") +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  scale_color_discrete(breaks = c("sum.year.individuals", "prop"),
                       labels = c("Raw number of birds observed", "Weighted by number of species")) +
  annotate("text", 
           x = 2014, 
           y = 110000,
           label = "Number of birds\nmore proportional\nto number of species", 
           size = 2.5) + 
  annotate("segment",
           x = 2005.7,
           xend = 2013.5,
           y = 90000,
           yend = 104000, 
           colour = "black", 
           size = 0.2) +
  annotate("text", 
           x = 1925, 
           y = 44000,
           label = "Number of birds\nless proportional\nto number of species", 
           size = 2.5) + 
  annotate("segment",
           x = 1935,
           xend = 1925,
           y = 25000,
           yend = 38000, 
           colour = "black", 
           size = 0.2)

ggsave(filename="june18-2019-plot.png", height = 6, width = 10)

