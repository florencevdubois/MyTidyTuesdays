library(tidyverse)
library(ggridges)
library(extrafont)
library(patchwork)
font_import() 
fonts() 

allCategories <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-30/allCategories.csv')

df <- allCategories %>% 
  filter(str_detect(categories, "color|drink|food|gem|wood")) %>% 
  mutate(categories2 = str_split(categories, ",")) %>% 
  unnest(categories2) %>% 
  filter(str_detect(categories2, "color|drink|food|gem|wood")) %>% 
  mutate(categories2 = str_trim(categories2, side = "both"),
         count = 1,
         lightness_cat = cut(lightness, breaks=c(0,.05,.1,.15,.2,.25,.3,.35,4,.45,.5,.55,.6,.65,.7,.75,.8,.85,.9,.95, 1))) %>% 
  group_by(categories2, lightness_cat) %>% 
  mutate(num = row_number()) %>% 
  ungroup()

#colours <- c(df$hex) 
#names(colours) <- c(df$name) 
#ggplot(aes(x = lightness_cat, y = num), data = df) +
#  geom_raster(aes(fill = name)) + 
#  scale_fill_manual(values = colours) +
#  theme_void() +
#  theme(legend.position = "") 

df1 <- df %>% filter(str_detect(categories2, "wood")) 

colours <- c(df1$hex) 
names(colours) <- c(df1$name) 
p1 <- ggplot(aes(x = lightness_cat, y = num), data = df1) +
  geom_raster(aes(fill = name)) + 
  annotate("text", x = 7, y =36, label = "WOOD", size = 10, family = "Andale Mono", color = "salmon4") + #"text", x = 16.5, y =8.5, label = "WOOD", size = 20, family = "Andale Mono", color = "salmon4"
  scale_fill_manual(values = colours) +
  scale_x_discrete() + #expand=c(0.77,0)
  theme_void() +
  theme(legend.position = "",
        plot.background = element_rect(fill = "cornsilk", color = "cornsilk"),
        plot.margin=unit(c(1,1,1,1),"cm")) 
p1

df2 <- df %>% filter(str_detect(categories2, "gem")) 

colours <- c(df2$hex) 
names(colours) <- c(df2$name) 
p2 <- ggplot(aes(x = lightness_cat, y = num), data = df2) +
  geom_raster(aes(fill = name)) + 
  annotate("text", x = 12.5, y =95, label = "GEM", size = 10, family = "Andale Mono", color = "salmon4") + #"text", x = 16.5, y =8.5, label = "WOOD", size = 20, family = "Andale Mono", color = "salmon4"
  scale_fill_manual(values = colours) +
  scale_x_discrete() + #expand=c(0.77,0)
  theme_void() +
  theme(legend.position = "",
        plot.background = element_rect(fill = "cornsilk", color = "cornsilk"),
        plot.margin=unit(c(1,1,1,1),"cm")) 
p2

df3 <- df %>% filter(str_detect(categories2, "drink")) 

colours <- c(df3$hex) 
names(colours) <- c(df3$name) 
p3 <- ggplot(aes(x = lightness_cat, y = num), data = df3) +
  geom_raster(aes(fill = name)) + 
  annotate("text", x = 9, y =51, label = "DRINK", size = 10, family = "Andale Mono", color = "salmon4") + #"text", x = 16.5, y =8.5, label = "WOOD", size = 20, family = "Andale Mono", color = "salmon4"
  scale_fill_manual(values = colours) +
  scale_x_discrete() + #expand=c(0.77,0)
  theme_void() +
  theme(legend.position = "",
        plot.background = element_rect(fill = "cornsilk", color = "cornsilk"),
        plot.margin=unit(c(1,1,1,1),"cm")) 
p3

df4 <- df %>% filter(str_detect(categories2, "food")) 

colours <- c(df4$hex) 
names(colours) <- c(df4$name) 
p4<- ggplot(aes(x = lightness_cat, y = num), data = df4) +
  geom_raster(aes(fill = name)) + 
  annotate("text", x = 12.5, y =145, label = "FOOD", size = 10, family = "Andale Mono", color = "salmon4") + #"text", x = 16.5, y =8.5, label = "WOOD", size = 20, family = "Andale Mono", color = "salmon4"
  scale_fill_manual(values = colours) +
  scale_x_discrete() + #expand=c(0.77,0)
  theme_void() +
  theme(legend.position = "",
        plot.background = element_rect(fill = "cornsilk", color = "cornsilk"),
        plot.margin=unit(c(1,1,1,1),"cm"),
        plot.caption = element_text(family = "Andale Mono", color = "salmon4")) +
  labs(caption = "")
p4

patch <- (p1+p2)/(p3+p4) +
  plot_annotation(title = "Names and Colours\nof Complexion Products",
                  subtitle = "Data: @ProQuesAsker and @OfunneO\nDataviz: @florencevdubois")& 
  theme(plot.title = element_text(family = "Andale Mono", face = "bold", color = "salmon4", size = 25, hjust = 0.5, vjust = -6),
        plot.subtitle = element_text(family = "Andale Mono", color = "salmon3", size = 10,hjust = 0.7, vjust = -18),
        plot.background = element_rect(fill = "cornsilk", color = "cornsilk"))
patch
 
ggsave("graphs/plot-2021.03.30.png", plot = patch, width = 9, height = 9)




