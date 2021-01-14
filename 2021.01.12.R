library(tidyverse)
library(extrafont)
artwork <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-12/artwork.csv')
# artists <- readr::read_csv("https://github.com/tategallery/collection/raw/master/artist_data.csv")

table(artwork$acquisitionYear)
summary(artwork)
table(artwork$acquisitionYear)

viz <- artwork %>% 
  drop_na(width,height) %>% 
  filter(is.na(depth) == T) %>% 
  arrange(acquisitionYear) %>% 
  mutate(size = (width*height),
         n = row_number()) %>% 
  drop_na(acquisitionYear)

year1900 <- viz %>%  filter(acquisitionYear == 1900)
year2000 <- viz %>%  filter(acquisitionYear == 2000)

p <- ggplot(viz, aes(n, acquisitionYear)) + 
  geom_point(aes(size = size)) +
  scale_size_continuous(range = c(.1, 4)) +
  scale_x_continuous(limits=c(0,80000))+
  scale_y_continuous(breaks=c(1823, 1856, 2000))+
  annotate("text", 
           x = 2000, y = 1940, 
           label = "Tate\nMuseum\nAcquisitions\nOver 190 Years", 
           size = 15, 
           family = ".SF Compact Text",
           hjust = 0) +
  annotate("text", x = 5000, y = 1825, label = "1823. First Tate acquisition.", family = "Arial Narrow") +
  annotate("text", x = 45500, y = 1858, label = "Tate acquired 37,893 pieces of art in 1856.", family = "Arial Narrow") +
  annotate("text", x = 48000, y = 1900, label = "By 1900, approx. 38,300 artworks belonged to Tate.", family = "Arial Narrow") +
  annotate("text", x = 56000, y = 1975, label = "In 1975, Tate acquired 3,000+ artworks.", family = "Arial Narrow") +
  annotate("text", x = 71000, y = 2000, label = "By the new millenium, 58,400 pieces of art belonged to Tate.", family = "Arial Narrow") +
  annotate("text", x = 69000, y = 2013, label = "2013. 63,000+ pieces.", family = "Arial Narrow") +
  annotate("text", x = 70000, y = 1815, label = "Data: Tate Art Museum. Data viz: @florencevdubois", family = "Arial Narrow") +
  theme_void() +
  theme(legend.position = "",
        text = element_text(family = "Arial Narrow"))

ggsave("plot-2021.01.12.png", plot = p, width = 15, height = 5)


