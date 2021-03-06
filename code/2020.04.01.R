library(tidyverse)
library(ggplot2)
library(patchwork)

# load data
brewing_materials <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewing_materials.csv')

# recode
brewing_materials <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewing_materials.csv') %>% 
  filter(str_detect(type, "Corn") | # keep only ingredients
         str_detect(type, "Malt") |
         str_detect(type, "Barley") |
         str_detect(type, "Rice") |
         str_detect(type, "Wheat") |
         str_detect(type, "Hops") |
         str_detect(type, "Sugar")) %>% 
  select(-data_type, -material_type) %>% # remove unecessary variables
  mutate(month = month/12, # recode months 
         year_month = year + month, # make year.months variable
         diff_month = (month_current-month_prior_year)/1000,  # create difference between current month and same month in prior year (and divide by 1000 because numbers are huge)
         positive = ifelse(diff_month > 0, "positive", "negative")) # create this category to make colours later in the plot

# malt
p <- ggplot(subset(brewing_materials, type == "Malt and malt products"), aes(x = year_month, y = diff_month, xend = year_month, yend = 0, color = positive)) # xend and yend are necessary for geom_segment
p1 <- p + geom_point() +
  geom_segment() + # to draw line between dots and x axis
  scale_color_manual(values = c("orange", "gold")) + # this is probably not recommended, but I wanted "beer colours"
  scale_x_continuous(name = "",
                     limits = c(2008,2014)) + ## I kept only 2008-2014 because the data after 2014 seems messy
  scale_y_continuous(name = "Difference in number of barrels (*1000)", # this is the only plot with a y-axis label
                     limits = c(-50000, 50000)) +
  labs(title = "Malt and malt products") +
  annotate("rect", xmin = 2011.85, xmax = 2012.95, ymin = 0, ymax = 23000, # trying to practice annotation, I still find it hard to position text/rectangles/segments where I want them
           alpha = .2) +
  annotate("text", x = 2011.7, y = 32000, label = "2012 was a good year for malt.", size = 3) +
  geom_curve(aes(x = 2011.5, y = 30000, xend = 2011.8, yend = 22900), color = "black") + # my first time using geom_curve -- just like, it's hard to get it where you want it
  theme_minimal() +
  theme(legend.position = "none")
p1

# hops
p <- ggplot(subset(brewing_materials, type == "Hops (dry)"), aes(x = year_month, y = diff_month, xend = year_month, yend = 0, color = positive))
p2 <- p + geom_point() +
  geom_segment() +
  scale_color_manual(values = c("orange", "gold")) +
  scale_x_continuous(name = "",
                     limits = c(2008,2014)) +
  scale_y_continuous(name = "",
                     limits = c(-50000, 50000)) +
  labs(title = "Dry hops") +
  annotate("rect", xmin = 2009.85, xmax = 2011.85, ymin = -1000, ymax = 11000,
           alpha = .2) +
  annotate("text", x = 2009, y = -15000, label = "Early 2010s:\nfirst New England IPAs\ncreated in Vermont.", size = 3) +
  geom_curve(aes(x = 2010.1, y = -15000, xend = 2010.3, yend = -1200), color = "black") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.y = element_blank())
p2

# sugars
p <- ggplot(subset(brewing_materials, type == "Sugar and syrups"), aes(x = year_month, y = diff_month, xend = year_month, yend = 0, color = positive))
p3 <- p + geom_point() +
  geom_segment() +
  scale_color_manual(values = c("orange", "gold")) +
  scale_x_continuous(name = "",
                     limits = c(2008,2014)) +
  scale_y_continuous(name = "",
                     limits = c(-50000, 50000)) +
  labs(title = "Sugar and syrups") +
  annotate("text", x = 2012, y = 41000, label = "Sugar lightens\nyour beer.", size = 3) +
  geom_curve(aes(x = 2011.3, y = 40900, xend = 2010.7, yend = 23000), color = "black") +
  geom_curve(aes(xend = 2012.65, yend = 40800, x = 2012.9, y = 23000), color = "black") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.y = element_blank())
p3

# join everything together
patchwork <- p1 + p2 + p3 +
  plot_annotation(title = 'Monthly Beer Ingredients Usage, USA, 2008-2014',
                  subtitle = 'Difference Between the Number of Barrels in Current Month and One Year Prior',
                  caption = 'Data: Alcohol and Tobacco Tax and Trade Bureau')
patchwork

#ggsave(filename = "plot-2020.04.01.png", plot = patchwork, width = 15, height = 6)

