library(tidyverse)
library(ggrepel)
library(countrycode)
library(patchwork)

transit_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-05/transit_cost.csv')

class(transit_cost$real_cost)
summary(transit_cost$length)
table(transit_cost$end_year)

# real_cost ---  y axis
# length --- x axis

viz <- transit_cost %>% 
  mutate(real_cost = as.numeric(real_cost)) %>% 
  filter(length <= 100,
         end_year == 2019) %>% 
  group_by(country) %>% 
  mutate(mean_cost = mean(real_cost, na.rm = T),
         mean_length = mean(length, na.rm = T)) %>% 
  ungroup() %>% 
  distinct(country, mean_cost, mean_length) %>% 
  mutate(country_long = countrycode(country, origin = "genc2c", destination = "country.name"))

p <- ggplot(data = viz) 
p1 <- p + geom_point(aes(x = mean_length, y = mean_cost), size = 2) +
  geom_segment(aes(y = 0, yend = mean_cost, x = 0, xend = mean_length), color = "forestgreen", alpha = .4) + # lines for each point
  geom_segment(aes(y = -500, yend = -500, x = 45, xend =  65), color = "grey69", size = .3) + # x axis line
  geom_segment(aes(y = 4000, yend = 6000, x = -2, xend = -2), color = "grey69", size = .3) + # y axis line
  geom_label_repel(aes(x = mean_length, y = mean_cost, label = country_long), # labels
                   size = 3, ## size of text
                   label.r = .01, # corners (roundness)
                   label.size = .08,
                   box.padding   = 0.2,
                   point.padding = 0.3,
                   segment.color = 'transparent', # no segment from point to box
                   nudge_x = 0,
                   nudge_y = 0) + 
  annotate("segment", x = 64.25, y = -400, xend = 65, yend = -500, colour = "grey69") + # x axis arrow
  annotate("segment", x = 64.25, y = -600, xend = 65, yend = -500, colour = "grey69") + # x axis arrow
  annotate("segment", x = -2.5, y = 5900, xend = -2, yend = 6000, colour = "grey69") + # y axis arrow
  annotate("segment", x = -1.5, y = 5900, xend = -2, yend = 6000, colour = "grey69") + # y axis arrow
  annotate("text", label = "Length", x = 67, y = -500, colour = "grey69", size = 3) + # x axis label
  annotate("text", label = "Costs", x = -2, y = 6400, angle = 90, colour = "grey69", size = 3) + # y axis label
  annotate("text", label = "Urban Rail Builts", x = 50, y = 800, colour = "black", size = 20) + # title
  annotate("text", label = "Average length and costs of projects ending in 2019.", x = 53, y = 300, colour = "black", size = 5) + # title
  scale_y_continuous(limits = c(-1000,7000)) +
  scale_x_continuous(limits = c(-6, 70)) +
  theme_void() +
  labs(caption = "Data: Transit Costs Project\nVisualization: @florencevdubois")
p1

viz2 <- viz %>% 
  mutate(ratio = mean_cost/mean_length)

p <- ggplot(aes(x = reorder(country_long, -ratio), y = ratio), data = viz2)
p2 <- p + geom_point() +
  geom_segment(aes(y = 0, yend = ratio, x = country_long, xend = country_long), color = "forestgreen", alpha = .4) +
  coord_flip() +
  theme_minimal() +
  labs(title =  "Way to go, Turkey!",
       x = "",
       y = "Average cost for 1km built (in million $US)") +
  theme(aspect.ratio = 1/3)+
  labs(caption = "Data: Transit Costs Project\nVisualization: @florencevdubois")
p2

ggsave(filename = "plot1-2021.01.05.png", plot = p1, width = 12, height = 8)
ggsave(filename = "plot2-2021.01.05.png", plot = p2, width = 9, height = 6)

####  projects ending in 2020
# viz2 <- transit_cost %>% 
#   mutate(real_cost = as.numeric(real_cost)) %>% 
#   filter(length <= 100,
#          end_year == 2020) %>% 
#   group_by(country) %>% 
#   mutate(mean_cost = mean(real_cost, na.rm = T),
#          mean_length = mean(length, na.rm = T)) %>% 
#   ungroup() %>% 
#   distinct(country, mean_cost, mean_length) %>% 
#   mutate(country_long = countrycode(country, origin = "genc2c", destination = "country.name"))
# 
# p <- ggplot(data = viz2) 
# p1 <- p + geom_point(aes(x = mean_length, y = mean_cost), size = 2) +
#   geom_segment(aes(y = 0, yend = mean_cost, x = 0, xend = mean_length), color = "forestgreen", alpha = .4) + # lines for each point
#   geom_segment(aes(y = -500, yend = -500, x = 10, xend = 30), color = "grey69", size = .3) + # x axis line
#   geom_segment(aes(y = 1000, yend = 4000, x = -4, xend = -4), color = "grey69", size = .3) + # y axis line
#   geom_label_repel(aes(x = mean_length, y = mean_cost, label = country_long),
#                    size = 3,
#                    label.r = .01,
#                    label.size = .08,
#                    box.padding   = 0.2,
#                    point.padding = 0.3,
#                    segment.color = 'transparent',
#                    nudge_x = 0, #4
#                    nudge_y = 0) + #100
#   annotate("segment", x = 29.5, y = -400, xend = 30, yend = -500, colour = "grey69") + # x axis arrow
#   annotate("segment", x = 29.5, y = -600, xend = 30, yend = -500, colour = "grey69") + # x axis arrow
#   annotate("segment", x = -4.5, y = 3900, xend = -4, yend = 4000, colour = "grey69") + # y axis arrow
#   annotate("segment", x = -3.5, y = 3900, xend = -4, yend = 4000, colour = "grey69") + # y axis arrow
#   annotate("text", label = "Length", x = 32, y = -500, colour = "grey69", size = 3) + # x axis label
#   annotate("text", label = "Costs", x = -4, y = 4400, angle = 90, colour = "grey69", size = 3) + # y axis label
#   annotate("text", label = "Urban Rail Builts", x = 33, y = 1000, colour = "black", size = 20) + # title
#   annotate("text", label = "Average length and costs of projects ending in 2020.", x = 36, y = 500, colour = "black", size = 5) + # title
#   scale_y_continuous(limits = c(-1000,7000)) +
#   scale_x_continuous(limits = c(-6, 50)) +
#   theme_void() 
# p1
